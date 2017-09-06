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

-module(lg_messages_seqdiag).

-export([profile/2]).
-export([profile_many/2]).

-record(state, {
    events = [],
    pids
}).

-spec profile(file:filename_all(), list()) -> ok.
profile(Input, Pids) ->
    {ok, FinalState} = lg_file_reader:fold(fun handle_event/2,
        #state{pids=prepare_pids(Pids)}, Input),
    flush(FinalState).

-spec profile_many(file:filename(), list()) -> ok.
profile_many(Wildcard, Pids) ->
    Files = filelib:wildcard(Wildcard),
    FinalState = lists:foldl(fun(Input, State0) ->
        {ok, State} = lg_file_reader:fold(fun handle_event/2, State0, Input),
        State
    end, #state{pids=prepare_pids(Pids)}, Files),
    flush(FinalState).

handle_event(Event = {Type, From0, _, _, To0}, State=#state{events=Events, pids=Pids})
        when Type =:= send; Type =:= send_to_non_existing_process ->
    From = hide_pid_node(From0),
    To = hide_pid_node(To0),
    case {lists:member(From, Pids), lists:member(To, Pids)} of
        {true, true} ->
            State#state{events=[Event|Events]};
        _ ->
            State
    end;
%% Ignore all other events. We only care about messages.
handle_event(_, State) ->
    State.

prepare_pids(Pids) ->
    [hide_pid_node(Pid) || Pid <- Pids].

hide_pid_node(Pid) when is_pid(Pid) -> hide_pid_node(pid_to_list(Pid));
hide_pid_node([$<, _, $.|Tail]) -> "<***." ++ Tail;
hide_pid_node([$<, _, _, $.|Tail]) -> "<***." ++ Tail;
hide_pid_node([$<, _, _, _, $.|Tail]) -> "<***." ++ Tail;
hide_pid_node([$<, _, _, _, _, $.|Tail]) -> "<***." ++ Tail;
hide_pid_node([$<, _, _, _, _, _, $.|Tail]) -> "<***." ++ Tail;
hide_pid_node(Name) -> Name.

flush(#state{events=Events0}) ->
    %% Sort by timestamp from oldest to newest.
    Events = lists:keysort(3, Events0),
    %% Initialize the formatting state.
    put(num_calls, 0),
    %% Output everything.
    file:write_file("seq.diag", [
        "seqdiag {\n"
        "    edge_length = 300;\n"
        "    activation = none;\n"
        "\n",
        [format_event(Event) || Event <- Events],
        "}\n"
    ]),
    io:format(
        "The file seq.diag was created. Use seqdiag to make a PNG.~n"
        "$ seqdiag -Tpng --no-transparency seq.diag~n"
        "~n"
        "To use a custom font, use the -f modifier:~n"
        "$ seqdiag -Tpng --no-transparency -f /usr/share/fonts/TTF/verdana.ttf seq.diag~n"
        "~n"
        "You can also edit the file to remove uninteresting messages.~n"
        "One line in the file is equal to a message sent by a process to another.~n"),
    ok.

format_event({Type, From, _, {'$gen_call', {From, Ref}, Msg}, To}) ->
    NumCalls = get(num_calls) + 1,
    put(num_calls, NumCalls),
    put(Ref, NumCalls),
    io_lib:format("    \"~w\" ~s \"~w\" [label=\"gen:call #~w ~9999P\"];~n",
        [From, case Type of send -> "->"; _ -> "-->" end, To, NumCalls, Msg, 8]);
format_event(Event={Type, From, _, {Ref, Msg}, To}) ->
    case get(Ref) of
        undefined ->
            default_format_event(Event);
        NumCall ->
            io_lib:format("    \"~w\" ~s \"~w\" [label=\"#~w ~9999P\"];~n",
                [From, case Type of send -> "->"; _ -> "-->" end, To, NumCall, Msg, 8])
    end;
format_event(Event) ->
    default_format_event(Event).

default_format_event({Type, From, _, Msg, To}) ->
    io_lib:format("    \"~w\" ~s \"~w\" [label=\"~9999P\"];~n",
        [From, case Type of send -> "->"; _ -> "-->" end, To, Msg, 8]).
