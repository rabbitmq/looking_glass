%% Copyright (c) 2017-Present Pivotal Software, Inc.  All rights reserved.
%%
%% This package, Looking Glass, is double-licensed under the Mozilla
%% Public License 1.1 ("MPL") and the Apache License version 2
%% ("ASL"). For the MPL, please see LICENSE-MPL-RabbitMQ. For the ASL,
%% please see LICENSE-APACHE2.
%%
%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND,
%% either express or implied. See the LICENSE file for specific language governing
%% rights and limitations of this software.
%%
%% If you have any questions regarding licensing, please contact us at
%% info@rabbitmq.com.

-module(lg_socket_tracer).

-export([start_link/2]).
-export([init/2]).

-export([system_continue/3]).
-export([system_terminate/4]).
-export([system_code_change/4]).

-record(state, {
    parent :: pid(),
    lsocket :: inet:socket(),
    timeout_ref :: reference() | undefined
}).

start_link(Nth, BasePort) ->
    Pid = proc_lib:spawn_link(?MODULE, init, [self(), BasePort + Nth - 1]),
    {ok, Pid}.

init(Parent, Port) ->
    %% Store all messages off the heap to avoid unnecessary GC.
    process_flag(message_queue_data, off_heap),
    %% We need to trap exit signals in order to shutdown properly.
    process_flag(trap_exit, true),
    %% Open the listening socket.
    {ok, LSocket} = gen_tcp:listen(Port, [
        binary, {reuseaddr, true}, {nodelay, true},
        %% We encode all events to binary inside a 2-byte length frame.
        {packet, 2},
        %% We expect the client to send pings every second or so and
        %% nothing else, so using active mode is faster and still safe.
        {active, true},
        %% We only expect one connection at a time. We don't need
        %% a backlog except for the cases where the connection is
        %% lost and will reconnect immediately before we get a
        %% chance to accept again.
        {backlog, 1}
        %% We are using non-blocking TCP send. We therefore do not
        %% need to configure send timeout options.
    ]),
    %% We reject all messages until we get a connection.
    accept(#state{parent=Parent, lsocket=LSocket}).

accept(State=#state{lsocket=LSocket}) ->
    {ok, AcceptRef} = prim_inet:async_accept(LSocket, -1),
    accept_loop(State, AcceptRef).

accept_loop(State=#state{parent=Parent, lsocket=LSocket}, AcceptRef) ->
    receive
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [],
                {accept_loop, State, AcceptRef});
        {inet_async, LSocket, AcceptRef, {ok, CSocket}} ->
            trace_loop(set_timeout(State), CSocket);
        {inet_async, LSocket, AcceptRef, Error} ->
            exit({accept_error, Error});
        %% We discard all trace events when no client is connected.
        %% We may also end up discarding old timeouts or TCP messages.
        _ ->
            accept_loop(State, AcceptRef)
    end.

trace_loop(State=#state{parent=Parent, timeout_ref=TRef}, CSocket) ->
    receive
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [],
                {trace_loop, State, CSocket});
        %% Reset the timeout when we receive data.
        {tcp, CSocket, _} ->
            trace_loop(reset_timeout(State), CSocket);
        {tcp_closed, CSocket} ->
            close(State, CSocket);
        {tcp_error, CSocket, _} ->
            close(State, CSocket);
        {timeout, TRef, ?MODULE} ->
            close(State, CSocket);
        %% Discard the non-blocking send reply when successful.
        {inet_reply, CSocket, ok} ->
            trace_loop(State, CSocket);
        %% And close the socket when an error occured.
        {inet_reply, CSocket, _} ->
            close(State, CSocket);
        %% Discard TCP messages from closed sockets.
        {tcp, _, _} ->
            trace_loop(State, CSocket);
        {tcp_closed, _} ->
            trace_loop(State, CSocket);
        {tcp_error, _, _} ->
            trace_loop(State, CSocket);
        %% Discard any previous timeout.
        {timeout, _, ?MODULE} ->
            trace_loop(State, CSocket);
        Msg ->
            Bin = term_to_binary(Msg),
            _ = byte_size(Bin),
            case erlang:port_command(CSocket, <<Bin/binary>>, [nosuspend]) of
                true ->
                    trace_loop(State, CSocket);
                %% The send buffer is full.
                false ->
                    close(State, CSocket)
            end
    end.

close(State, CSocket) ->
    _ = gen_tcp:close(CSocket),
    accept(cancel_timeout(State)).

system_continue(_, _, {accept_loop, State, AcceptRef}) ->
    accept_loop(State, AcceptRef);
system_continue(_, _, {trace_loop, State, CSocket}) ->
    trace_loop(State, CSocket).

-spec system_terminate(any(), _, _, _) -> no_return().
system_terminate(Reason, _, _, _) ->
    exit(Reason).

system_code_change(Misc, _, _, _) ->
    {ok, Misc}.

reset_timeout(State) ->
    set_timeout(cancel_timeout(State)).

set_timeout(State) ->
    TRef = erlang:start_timer(5000, self(), ?MODULE),
    State#state{timeout_ref=TRef}.

cancel_timeout(State=#state{timeout_ref=TRef}) ->
    _ = erlang:cancel_timer(TRef, [{async, true}, {info, false}]),
    State#state{timeout_ref=undefined}.
