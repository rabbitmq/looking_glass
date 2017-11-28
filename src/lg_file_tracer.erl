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

-module(lg_file_tracer).

-export([start_link/2]).
-export([init/2]).

-export([system_continue/3]).
-export([system_terminate/4]).
-export([system_code_change/4]).

-record(state, {
    parent :: pid(),
    filename :: file:filename_all(),
    io_device :: file:io_device(),
    events_per_frame = 100000 :: pos_integer(),
    events_this_frame = 0 :: non_neg_integer(),
    buffer = <<>> :: binary()
}).

start_link(Nth, Filename0) ->
    Filename = filename:flatten([Filename0, ".", integer_to_list(Nth)]),
    Pid = proc_lib:spawn_link(?MODULE, init, [self(), Filename]),
    {ok, Pid}.

init(Parent, Filename) ->
    %% Store all messages off the heap to avoid unnecessary GC.
    process_flag(message_queue_data, off_heap),
    %% We need to trap exit signals in order to shutdown properly.
    process_flag(trap_exit, true),
    %% No need to close the file, it'll be closed when the process exits.
    {ok, IoDevice} = file:open(Filename, [write, raw]),
    loop(#state{parent=Parent, filename=Filename, io_device=IoDevice}).

loop(State=#state{parent=Parent, io_device=IoDevice,
        events_per_frame=MaxEvents, events_this_frame=NumEvents0, buffer=Buffer0}) ->
    receive
        {'EXIT', Parent, Reason} ->
            terminate(Reason, State);
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [], State);
        Msg ->
            Bin = term_to_binary(Msg),
            BinSize = byte_size(Bin),
            Buffer = <<Buffer0/binary, BinSize:32, Bin/binary>>,
            NumEvents = NumEvents0 + 1,
            if
                MaxEvents =:= NumEvents ->
                    ok = file:write(IoDevice, lz4f:compress_frame(Buffer)),
                    loop(State#state{events_this_frame=0, buffer= <<>>});
                true ->
                    loop(State#state{events_this_frame=NumEvents, buffer=Buffer})
            end
    end.

system_continue(_, _, State) ->
    loop(State).

-spec system_terminate(any(), _, _, #state{}) -> no_return().
system_terminate(Reason, _, _, State) ->
    terminate(Reason, State).

system_code_change(Misc, _, _, _) ->
    {ok, Misc}.

-spec terminate(any(), #state{}) -> no_return().
terminate(Reason, #state{io_device=IoDevice, buffer=Buffer}) ->
    _ = file:write(IoDevice, lz4f:compress_frame(Buffer)),
    exit(Reason).
