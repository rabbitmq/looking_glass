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
    meta = #{} :: map(),
    senders = #{} :: #{pid() => pos_integer()},
    receivers = #{} :: #{pid() => pos_integer()},
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
handle_event({send, From, _, Info, lg}, State=#state{meta=Meta0}) ->
    Meta = case Meta0 of
        #{From := Info0} -> Meta0#{From => maps:merge(Info0, Info)};
        _ -> Meta0#{From => Info}
    end,
    State#state{meta=Meta};
handle_event({send, From, _, Msg, To},
        State=#state{senders=Senders, receivers=Receivers, pairs=Pairs, last_msgs=Msgs}) ->
    SendersCount = maps:get(From, Senders, 0),
    ReceiversCount = maps:get(To, Receivers, 0),
    PairsCount = maps:get({From, To}, Pairs, 0),
    State#state{
        senders=Senders#{From => SendersCount + 1},
        receivers=Receivers#{To => ReceiversCount + 1},
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
    flush_most_active_senders(State),
    flush_most_active_receivers(State),
    flush_most_non_existing(State),
    flush_most_active_pair_unidirectional(State),
    flush_most_active_pair_bidirectional(State),
    io:format("~n"),
    flush_digraph(State),
    ok.

flush_most_active_senders(State=#state{senders=Procs}) ->
    List = lists:sublist(
        lists:reverse(lists:keysort(2, maps:to_list(Procs))),
        1, 100),
    format_by_count("They sent the most messages", List, State).

flush_most_active_receivers(State=#state{receivers=Procs}) ->
    List = lists:sublist(
        lists:reverse(lists:keysort(2, maps:to_list(Procs))),
        1, 100),
    format_by_count("They received the most messages", List, State).

flush_most_non_existing(State=#state{non_existing=Procs}) ->
    List = lists:sublist(
        lists:reverse(lists:keysort(2, maps:to_list(Procs))),
        1, 100),
    format_by_count("They sent the most messages to dead processes", List, State).

format_by_count(Title, List, State) ->
    MsgCols = case io:columns() of
        {ok, Cols} -> Cols;
        _ -> 80
    end,
    io:format(
        "~n~s~n~s~n~n"
        "Process ID      Count      (Label) OR Message sent~n"
        "----------      -----      -----------------------~n",
        [Title, lists:duplicate(length(Title), $=)]),
    _ = [begin
        {Prefix, Label, Suffix} = label_or_msg(P, State),
        io:format("~-15w ~-10b ~s~" ++ integer_to_list(MsgCols) ++ "P~s~n",
            [P, C, Prefix, Label, 5, Suffix])
    end || {P, C} <- List],
    ok.

label_or_msg(P, #state{meta=Meta, last_msgs=Msgs}) ->
    case maps:get(P, Meta, #{}) of
        #{process_type := PT} -> {"(", PT, ")"};
        _ -> {"", maps:get(P, Msgs, '<none>'), ""}
    end.

flush_most_active_pair_unidirectional(State=#state{pairs=Procs}) ->
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
        "From pid        To pid          Count      (Label) OR Message sent~n"
        "--------        ------          -----      -----------------------~n",
        [Title, lists:duplicate(length(Title), $=)]),
    _ = [begin
        {Prefix, Label, Suffix} = label_or_msg(F, State),
        io:format("~-15w ~-15w ~-10b ~s~" ++ integer_to_list(MsgCols) ++ "P~s~n",
            [F, T, C, Prefix, Label, 5, Suffix])
    end || {{F, T}, C} <- List],
    ok.

flush_most_active_pair_bidirectional(State=#state{pairs=Procs0}) ->
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
        "Count      Pid 1           (Label) OR Message sent~n"
        "           Pid 2           by the corresponding process~n"
        "-----      -----           ----------------------------~n",
        [Title, lists:duplicate(length(Title), $=)]),
    _ = [begin
        {FPrefix, FLabel, FSuffix} = label_or_msg(F, State),
        {TPrefix, TLabel, TSuffix} = label_or_msg(T, State),
        io:format(
            "~-10b ~-15w ~s~" ++ integer_to_list(MsgCols) ++ "P~s~n"
            "           ~-15w ~s~" ++ integer_to_list(MsgCols) ++ "P~s~n",
            [C, F, FPrefix, FLabel, 5, FSuffix,
                T, TPrefix, TLabel, 5, TSuffix])
    end || {{F, T}, C} <- List],
    ok.

flush_digraph(State=#state{pairs=Procs0}) ->
    Procs = maps:fold(fun group_pairs/3, #{}, Procs0),
    List = maps:to_list(Procs),
    file:write_file("digraph.gv", [
        "digraph {\n"
        "    concentrate=true;\n"
        "    splines=ortho;\n"
        "    edge [arrowhead=none, labelfontsize=12.0, minlen=3];\n"
        "\n",
        [io_lib:format("    \"~w~s\" -> \"~w~s\" [taillabel=~b, headlabel=~b];~n",
            [F, label(F, State), T, label(T, State), FC, TC]) || {{F, T}, {FC, TC}} <- List],
        "}\n"
    ]),
    io:format(
        "The file digraph.gv was created. Use GraphViz to make a PNG.~n"
        "$ dot -Tpng -O digraph.gv~n"
        "~n"
        "You can also edit the file to remove uninteresting processes.~n"
        "One line in the file is equal to a connection between two processes.~n"),
    ok.

label(P, #state{meta=Meta}) ->
    case maps:get(P, Meta, #{}) of
        #{process_type := PT} -> io_lib:format(" (~w)", [PT]);
        _ -> ""
    end.

merge_pairs({From, To}, Count, Acc) ->
    Key = if
        From < To -> {From, To};
        true -> {To, From}
    end,
    AccCount = maps:get(Key, Acc, 0),
    Acc#{Key => AccCount + Count}.

group_pairs({From, To}, Count, Acc) when From < To ->
    Key = {From, To},
    {_, AccCount} = maps:get(Key, Acc, {0, 0}),
    Acc#{Key => {Count, AccCount}};
group_pairs({From, To}, Count, Acc) ->
    Key = {To, From},
    {AccCount, _} = maps:get(Key, Acc, {0, 0}),
    Acc#{Key => {AccCount, Count}}.
