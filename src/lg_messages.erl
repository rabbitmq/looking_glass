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

-module(lg_messages).

-export([profile/1]).
-export([profile_many/1]).

-record(state, {
    processes = #{} :: #{pid() => pos_integer()},
    pairs = #{} :: #{{pid(), pid()} => pos_integer()},
    non_existing = #{} :: #{pid() => pos_integer()},
    last_msgs = #{} :: #{pid() => atom()}
}).

-spec profile(file:filename_all()) -> ok.
profile(Input) ->
    {ok, FinalState} = lg_file_reader:fold(fun handle_event/2, #state{}, Input),
    flush(FinalState).

-spec profile_many(file:filename()) -> ok.
profile_many(Wildcard) ->
    Files = filelib:wildcard(Wildcard),
    FinalState = lists:foldl(fun(Input, State0) ->
        {ok, State} = lg_file_reader:fold(fun handle_event/2, State0, Input),
        State
    end, #state{}, Files),
    flush(FinalState).

%% @todo Later we may want to look at the latency of gen_server call/reply.
%% @todo Later we may want to look at particular messages, have some sort of callback.
handle_event({send, From, _, Msg, To},
        State=#state{processes=Procs, pairs=Pairs, last_msgs=Msgs}) ->
    ProcsCount = maps:get(From, Procs, 0),
    PairsCount = maps:get({From, To}, Pairs, 0),
    State#state{
        processes=Procs#{From => ProcsCount + 1},
        pairs=Pairs#{{From, To} => PairsCount + 1},
        last_msgs=Msgs#{From => Msg}};
handle_event({send_to_non_existing_process, From, _, Msg, _},
        State=#state{non_existing=Map, last_msgs=Msgs}) ->
    Count = maps:get(From, Map, 0),
    State#state{
        non_existing=Map#{From => Count + 1},
        last_msgs=Msgs#{From => Msg}};
%% Ignore all other events. We only care about messages.
handle_event(_, State) ->
    State.

%% Output of the profiling.

flush(State) ->
    flush_most_active_processes(State),
    flush_most_non_existing(State),
    flush_most_active_pair_unidirectional(State),
    flush_most_active_pair_bidirectional(State),
    io:format("~n"),
    ok.

flush_most_active_processes(State=#state{processes=Procs}) ->
    List = lists:sublist(
        lists:reverse(lists:keysort(2, maps:to_list(Procs))),
        1, 100),
    format_by_count("They sent the most messages", List, State).

flush_most_non_existing(State=#state{non_existing=Procs}) ->
    List = lists:sublist(
        lists:reverse(lists:keysort(2, maps:to_list(Procs))),
        1, 100),
    format_by_count("They sent the most messages to dead processes", List, State).

format_by_count(Title, List, #state{last_msgs=Msgs}) ->
    MsgCols = case io:columns() of
        {ok, Cols} -> Cols;
        _ -> 80
    end,
    io:format(
        "~n~s~n~s~n~n"
        "Process ID      Count      Most recent message~n"
        "----------      -----      -------------------~n",
        [Title, lists:duplicate(length(Title), $=)]),
    _ = [
        io:format("~-15w ~-10b ~" ++ integer_to_list(MsgCols) ++ "P~n",
            [P, C, maps:get(P, Msgs), 5])
    || {P, C} <- List],
    ok.

flush_most_active_pair_unidirectional(#state{pairs=Procs, last_msgs=Msgs}) ->
    List = lists:sublist(
        lists:reverse(lists:keysort(2, maps:to_list(Procs))),
        1, 100),
    Title = "They sent the most messages to one other process",
    MsgCols = case io:columns() of
        {ok, Cols} -> Cols;
        _ -> 80
    end,
    io:format(
        "~n~s~n~s~n~n"
        "From pid        To pid          Count      Most recent message~n"
        "--------        ------          -----      -------------------~n",
        [Title, lists:duplicate(length(Title), $=)]),
    _ = [
        io:format("~-15w ~-15w ~-10b ~" ++ integer_to_list(MsgCols) ++ "P~n",
            [F, T, C, maps:get(F, Msgs), 5])
    || {{F, T}, C} <- List],
    ok.

flush_most_active_pair_bidirectional(#state{pairs=Procs0, last_msgs=Msgs}) ->
    Procs = maps:fold(fun merge_pairs/3, #{}, Procs0),
    List = lists:sublist(
        lists:reverse(lists:keysort(2, maps:to_list(Procs))),
        1, 100),
    Title = "They sent the most messages to each other",
    MsgCols = case io:columns() of
        {ok, Cols} -> Cols;
        _ -> 80
    end,
    io:format(
        "~n~s~n~s~n~n"
        "Count      Pid 1           Most recent message~n"
        "           Pid 2           from the corresponding process~n"
        "-----      -----           ------------------------------~n",
        [Title, lists:duplicate(length(Title), $=)]),
    _ = [
        io:format(
            "~-10b ~-15w ~" ++ integer_to_list(MsgCols) ++ "P~n"
            "           ~-15w ~" ++ integer_to_list(MsgCols) ++ "P~n",
            [C, F, maps:get(F, Msgs, '<none>'), 5,
                T, maps:get(T, Msgs, '<none>'), 5])
    || {{F, T}, C} <- List],
    ok.

merge_pairs({From, To}, Count, Acc) ->
    Key = if
        From < To -> {From, To};
        true -> {To, From}
    end,
    AccCount = maps:get(Key, Acc, 0),
    Acc#{Key => AccCount + Count}.
