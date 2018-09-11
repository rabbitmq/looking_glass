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

%% Going for hardcoded values for now. We can't spend time
%% looking up inside a record or map for this.

-module(lg_term).

-export([truncate/1]).
-export([truncate/2]).

-define(MAX_DEPTH, 5).
-define(MAX_BINARY_SIZE, 128).
-define(MAX_BITSTRING_SIZE, ?MAX_BINARY_SIZE * 8).
-define(MAX_DATA_STRUCTURES, 5).
-define(MAX_LIST_LENGTH, 32).
-define(MAX_MAP_SIZE, 32).
-define(MAX_TUPLE_SIZE, 32).

truncate(Term) ->
    truncate(Term, 1).

truncate(_, Depth) when Depth > ?MAX_DEPTH ->
    '$truncated';
truncate(Term, _) when bit_size(Term) > ?MAX_BITSTRING_SIZE ->
    <<Truncated:?MAX_BINARY_SIZE/binary, _/bits>> = Term,
    <<Truncated/binary, "$truncated">>;
truncate(Term, Depth) when is_list(Term), Depth =:= ?MAX_DEPTH ->
    ['$truncated'];
truncate(Term, Depth) when is_list(Term) ->
    truncate_list(Term, Depth, 0, ?MAX_LIST_LENGTH, 0);
truncate(Term, Depth) when is_map(Term), Depth =:= ?MAX_DEPTH ->
    #{'$truncated' => '$truncated'};
truncate(Term, Depth) when is_map(Term) ->
    maps:from_list(truncate_map(maps_to_list(Term, ?MAX_MAP_SIZE), Depth, 0));
truncate(Term, Depth) when is_tuple(Term), Depth =:= ?MAX_DEPTH ->
    {'$truncated'};
truncate(Term, Depth) when is_tuple(Term) ->
    list_to_tuple(truncate_list(tuple_to_list(Term), Depth, 0, ?MAX_TUPLE_SIZE, 0));
truncate(Term, _) ->
    Term.

truncate_list([], _, _, _, _) ->
    [];
truncate_list(_, _, Len, MaxLen, _) when Len > MaxLen ->
    ['$truncated'];
truncate_list(_, _, _, _, NumStructs) when NumStructs > ?MAX_DATA_STRUCTURES ->
    ['$truncated'];
truncate_list([Term|Tail], Depth, Len, MaxLen, NumStructs) ->
    [truncate(Term, Depth + 1)
     %% if List was a cons, Tail can be anything
     |truncate_list(Tail, Depth, Len + 1, MaxLen, NumStructs + is_struct(Term))];
truncate_list(Term, Depth, _, _, _) -> %% if List was a cons 
    truncate(Term, Depth + 1).

truncate_map([], _, _) ->
    [];
truncate_map(_, _, NumStructs) when NumStructs > ?MAX_DATA_STRUCTURES ->
    [{'$truncated', '$truncated'}];
truncate_map([{Key, Value}|Tail], Depth, NumStructs) ->
    AddStruct = is_struct(Key) + is_struct(Value),
    [{truncate(Key, Depth + 1), truncate(Value, Depth + 1)}
        |truncate_map(Tail, Depth, NumStructs + AddStruct)].

is_struct(Term) ->
    if
        is_list(Term) -> 1;
        is_map(Term) -> 1;
        is_tuple(Term) -> 1;
        true -> 0
    end.

%% Map iterators were introduced in Erlang/OTP 21. They replace
%% the undocumented function erts_internal:maps_to_list/2.
-ifdef(OTP_RELEASE).

maps_to_list(Map, MaxSize) ->
    I = maps:iterator(Map),
    maps_to_list(maps:next(I), MaxSize, []).

%% Returns elements in arbitrary order. We reverse when we truncate
%% so that the truncated elements come at the end to avoid having
%% two truncated elements in the final output.
maps_to_list(none, _, Acc) ->
    Acc;
maps_to_list(_, 0, Acc) ->
    lists:reverse([{'$truncated', '$truncated'}|Acc]);
maps_to_list({K, V, I}, N, Acc) ->
    maps_to_list(maps:next(I), N - 1, [{K, V}|Acc]).

-else.

maps_to_list(Map, MaxSize) ->
    erts_internal:maps_to_list(Map, MaxSize).

-endif.

-ifdef(TEST).

maps_to_list_test() ->
    [] = maps_to_list(#{}, 10),
    [{'$truncated', '$truncated'}] = maps_to_list(#{a => b}, 0),
    [{a, b}] = maps_to_list(#{a => b}, 10),
    [{a, b}, {c, d}, {e, f}] = lists:sort(maps_to_list(
        #{a => b, c => d, e => f}, 3)),
    [{'$truncated', '$truncated'}, {a, b}, {c, d}, {e, f}] = lists:sort(maps_to_list(
        #{a => b, c => d, e => f, g => h}, 3)),
    [{'$truncated', '$truncated'}, {a, b}, {c, d}, {e, f}] = lists:sort(maps_to_list(
        #{a => b, c => d, e => f, g => h, i => j}, 3)),
    %% Confirm that truncated values are at the end.
    [_, _, _, {'$truncated', '$truncated'}] = maps_to_list(
        #{a => b, c => d, e => f, g => h, i => j}, 3),
    ok.

-endif.
