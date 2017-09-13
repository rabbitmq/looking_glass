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

-module(lg_socket_client).
-behavior(gen_statem).

-export([start_link/2]).
-export([stop/1]).

%% gen_statem.
-export([callback_mode/0]).
-export([init/1]).
-export([connect/3]).
-export([open_file/3]).
-export([process_events/3]).
-export([close_file/3]).
-export([code_change/4]).
-export([terminate/3]).

-record(state, {
    port :: inet:port_number(),
    base_filename :: file:filename_all(),
    nth = 0 :: non_neg_integer(),
    socket :: inet:socket() | undefined,
    io_device :: file:io_device() | undefined,
    events_per_frame = 100000 :: pos_integer(),
    events_this_frame = 0 :: non_neg_integer(),
    buffer = <<>> :: binary()
}).

start_link(Port, BaseFilename) ->
    gen_statem:start_link(?MODULE, [Port, BaseFilename], []).

stop(Pid) ->
    gen_statem:stop(Pid).

callback_mode() ->
    state_functions.

init([Port, BaseFilename]) ->
    %% Store all messages off the heap to avoid unnecessary GC.
    process_flag(message_queue_data, off_heap),
    %% We need to trap exit signals in order to shutdown properly.
    process_flag(trap_exit, true),
    {ok, connect, #state{port=Port, base_filename=BaseFilename},
        {next_event, internal, run}}.

connect(internal, _, State) ->
    do_connect(State);
connect({timeout, retry}, retry, State) ->
    do_connect(State);
connect(_, _, State) ->
    {keep_state, State}.

do_connect(State=#state{port=Port}) ->
    case gen_tcp:connect("localhost", Port, [binary, {packet, 2}, {active, true}]) of
        {ok, Socket} ->
            {next_state, open_file, State#state{socket=Socket},
                {next_event, internal, run}};
        {error, _} ->
            {keep_state, State, [{{timeout, retry}, 1000, retry}]}
    end.

open_file(internal, _, State=#state{base_filename=Filename0, nth=Nth}) ->
    Filename = filename:flatten([Filename0, ".", integer_to_list(Nth)]),
    {ok, IoDevice} = file:open(Filename, [write, raw]),
    {next_state, process_events, State#state{nth=Nth + 1, io_device=IoDevice}}.

process_events(info, {tcp, Socket, Bin}, State=#state{socket=Socket, io_device=IoDevice,
        events_per_frame=MaxEvents, events_this_frame=NumEvents0, buffer=Buffer0}) ->
    BinSize = byte_size(Bin),
    Buffer = <<Buffer0/binary, BinSize:16, Bin/binary>>,
    NumEvents = NumEvents0 + 1,
    if
        MaxEvents =:= NumEvents ->
            ok = file:write(IoDevice, lz4f:compress_frame(Buffer)),
            {keep_state, State#state{events_this_frame=0, buffer= <<>>}};
        true ->
            {keep_state, State#state{events_this_frame=NumEvents, buffer=Buffer}}
    end;
process_events(info, {tcp_closed, Socket}, State=#state{socket=Socket}) ->
    {next_state, close_file, State#state{socket=undefined},
        {next_event, internal, run}};
process_events(info, {tcp_error, Socket, _}, State=#state{socket=Socket}) ->
    _ = gen_tcp:close(Socket),
    {next_state, close_file, State#state{socket=undefined},
        {next_event, internal, run}}.

close_file(internal, _, State) ->
    do_close_file(State),
    {next_state, connect, State#state{io_device=undefined},
        {next_event, internal, run}}.

do_close_file(#state{io_device=IoDevice, buffer=Buffer}) ->
    _ = file:write(IoDevice, lz4f:compress_frame(Buffer)),
    _ = file:close(IoDevice),
    ok.

code_change(_OldVsn, OldState, OldData, _Extra) ->
    {callback_mode(), OldState, OldData}.

terminate(_, _, #state{io_device=undefined}) ->
    ok;
terminate(_, _, State) ->
    do_close_file(State),
    ok.
