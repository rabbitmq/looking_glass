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

-module(lg_flame).

-export([profile/2]).
-export([profile_many/2]).

-record(state, {
          output_path="",
          pid,
          last_ts,
          count=0,
          acc=[]}). % per-process state

-spec profile(file:filename_all(), file:filename_all()) -> ok.
profile(Input, Output) ->
    InitialState = exp1_init(Output),
    {ok, FinalState} = lg_file_reader:fold(fun handle_event/2, InitialState, Input),
    flush(FinalState).

-spec profile_many(file:filename(), file:filename()) -> ok.
profile_many(Wildcard, Output) ->
    InitialState = exp1_init(Output),
    Files = filelib:wildcard(Wildcard),
    FinalState = lists:foldl(fun(Input, State0) ->
        case lg_file_reader:fold(fun handle_event/2, State0, Input) of
            {ok, State} ->
                State;
            {error, Reason, HumanReadable} ->
                io:format("Error ~p while reading ~s:~n~s~n",
                    [Reason, Input, HumanReadable]),
                State0
        end
    end, InitialState, Files),
    flush(FinalState).

flush(#state{output_path=OutputPath}) ->
    PidStates = get(),
    {ok, FH} = file:open(OutputPath, [write, raw, binary, delayed_write]),
    io:format("\n\nWriting to ~s for ~w processes... ", [OutputPath, length(PidStates)]),
    _ = [
        [begin
             Pid_str0 = lists:flatten(io_lib:format("~w", [Pid])),
             Size = length(Pid_str0),
             Pid_str = [$(, lists:sublist(Pid_str0, 2, Size-2), $)],
             Time_str = integer_to_list(Time),
             file:write(FH, [Pid_str, $;, intersperse($;, lists:reverse(Stack)), 32, Time_str, 10])
         end || {Stack, Time} <- Acc]
     || {Pid, #state{acc=Acc} = _S} <- PidStates],
    _ = file:close(FH),
    io:format("finished!\n"),
    ok.

handle_event({Type, Pid, Ts, Arg}, State) ->
    exp1({trace_ts, Pid, Type, Arg, Ts}, State);
handle_event({Type, Pid, Ts, Arg, ExtraOrMspec}, State) ->
    exp1({trace_ts, Pid, Type, Arg, ExtraOrMspec, Ts}, State);
handle_event({Type, Pid, Ts, Arg, Extra, Mspec}, State) ->
    exp1({trace_ts, Pid, Type, Arg, Extra, Mspec, Ts}, State).

%% Below is Scott L. Fritchie's ISC licensed work with only a handful changes.

exp1_init(OutputPath) ->
    #state{output_path=OutputPath}.

exp1(T, #state{output_path=OutputPath} = S) ->
    trace_ts = element(1, T),
    Pid = element(2, T),
    PidState = case erlang:get(Pid) of
                   undefined ->
                       io:format("~p ", [Pid]),
                       #state{output_path=OutputPath};
                   SomeState ->
                       SomeState
               end,
    NewPidState = exp1_inner(T, PidState),
    erlang:put(Pid, NewPidState),
    S.

exp1_inner({trace_ts, _Pid, InOut, _MFA, _TS}, #state{last_ts=undefined} = S)
  when InOut == in; InOut == out ->
    %% in & out, without call context, don't help us
    S;
exp1_inner({trace_ts, _Pid, Return, _MFA, _TS}, #state{last_ts=undefined} = S)
  when Return == return_from; Return == return_to ->
    %% return_from and return_to, without call context, don't help us
    S;
exp1_inner({trace_ts, Pid, call, MFA, BIN, TS},
     #state{last_ts=LastTS, acc=Acc, count=Count} = S) ->
  try
    %% Calculate time elapsed, TS-LastTs.
    %% 0. If Acc is empty, then skip step #1.
    %% 1. Credit elapsed time to the stack on the top of Acc.
    %% 2. Push a 0 usec item with this stack onto Acc.
    Stak = lists:filter(fun(<<"unknown function">>) -> false;
                           (_)                      -> true
                        end, stak_binify(BIN)),
    Stack0 = stak_trim(Stak),
    MFA_bin = mfa_binify(MFA),
    Stack1 = [MFA_bin|lists:reverse(Stack0)],
    Acc2 = case Acc of
               [] ->
                   [{Stack1, 0}];
               [{LastStack, LastTime}|Tail] ->
                   USec = TS - LastTS,
%                   io:format("Stack1: ~p ~p\n", [Stack1, USec]),
                   [{Stack1, 0},
                    {LastStack, LastTime + USec}|Tail]
           end,
    %% TODO: more state tracking here.
    S#state{pid=Pid, last_ts=TS, count=Count+1, acc=Acc2}
  catch XX:YY:ZZ ->
            io:format(user, "~p: ~p:~p @ ~p\n", [?LINE, XX, YY, ZZ]),
            S
  end;
exp1_inner({trace_ts, _Pid, return_to, MFA, TS}, #state{last_ts=LastTS, acc=Acc} = S) ->
  try
    %% Calculate time elapsed, TS-LastTs.
    %% 1. Credit elapsed time to the stack on the top of Acc.
    %% 2. Push a 0 usec item with the "best" stack onto Acc.
    %%    "best" = MFA exists in the middle of the stack onto Acc,
    %%    or else MFA exists at the top of a stack elsewhere in Acc.
    [{LastStack, LastTime}|Tail] = Acc,
    MFA_bin = mfa_binify(MFA),
    BestStack = lists:dropwhile(fun(SomeMFA) when SomeMFA /= MFA_bin -> true;
                                   (_)                               -> false
                                end, find_matching_stack(MFA_bin, Acc)),
    USec = TS - LastTS,
    Acc2 = [{BestStack, 0},
            {LastStack, LastTime + USec}|Tail],
%    io:format(user, "return-to: ~p\n", [lists:sublist(Acc2, 4)]),
    S#state{last_ts=TS, acc=Acc2}
  catch XX:YY:ZZ ->
            io:format(user, "~p: ~p:~p @ ~p\n", [?LINE, XX, YY, ZZ]),
            S
  end;
    
exp1_inner({trace_ts, _Pid, gc_start, _Info, TS}, #state{last_ts=LastTS, acc=Acc} = S) ->
  try
    %% Push a 0 usec item onto Acc.
    [{LastStack, LastTime}|Tail] = Acc,
    NewStack = [<<"GARBAGE-COLLECTION">>|LastStack],
    USec = TS - LastTS,
    Acc2 = [{NewStack, 0},
            {LastStack, LastTime + USec}|Tail],
%    io:format(user, "GC 1: ~p\n", [lists:sublist(Acc2, 4)]),
    S#state{last_ts=TS, acc=Acc2}
  catch _XX:_YY:_ZZ ->
            %% io:format(user, "~p: ~p:~p @ ~p\n", [?LINE, _XX, _YY, _ZZ]),
            S
  end;
exp1_inner({trace_ts, _Pid, gc_end, _Info, TS}, #state{last_ts=LastTS, acc=Acc} = S) ->
  try
    %% Push the GC time onto Acc, then push 0 usec item from last exec
    %% stack onto Acc.
    [{GCStack, GCTime},{LastExecStack,_}|Tail] = Acc,
    USec = TS - LastTS,
    Acc2 = [{LastExecStack, 0}, {GCStack, GCTime + USec}|Tail],
%    io:format(user, "GC 2: ~p\n", [lists:sublist(Acc2, 4)]),
    S#state{last_ts=TS, acc=Acc2}
  catch _XX:_YY:_ZZ ->
            %% io:format(user, "~p: ~p:~p @ ~p\n", [?LINE, _XX, _YY, _ZZ]),
            S
  end;

exp1_inner({trace_ts, _Pid, out, MFA, TS}, #state{last_ts=LastTS, acc=Acc} = S) ->
  try
    %% Push a 0 usec item onto Acc.
    %% The MFA reported here probably doesn't appear in the stacktrace
    %% given to us by the last 'call', so add it here.
    [{LastStack, LastTime}|Tail] = Acc,
    MFA_bin = mfa_binify(MFA),
    NewStack = [<<"SLEEP">>,MFA_bin|LastStack],
    USec = TS - LastTS,
    Acc2 = [{NewStack, 0},
            {LastStack, LastTime + USec}|Tail],
    S#state{last_ts=TS, acc=Acc2}
  catch XX:YY:ZZ ->
            io:format(user, "~p: ~p:~p @ ~p\n", [?LINE, XX, YY, ZZ]),
            S
  end;
exp1_inner({trace_ts, _Pid, in, MFA, TS}, #state{last_ts=LastTS, acc=Acc} = S) ->
  try
    %% Push the Sleep time onto Acc, then push 0 usec item from last
    %% exec stack onto Acc.
    %% The MFA reported here probably doesn't appear in the stacktrace
    %% given to us by the last 'call', so add it here.
    MFA_bin = mfa_binify(MFA),
    [{SleepStack, SleepTime},{LastExecStack,_}|Tail] = Acc,
    USec = TS - LastTS,
    Acc2 = [{[MFA_bin|LastExecStack], 0}, {SleepStack, SleepTime + USec}|Tail],
    S#state{last_ts=TS, acc=Acc2}
  catch XX:YY:ZZ ->
            io:format(user, "~p: ~p:~p @ ~p\n", [?LINE, XX, YY, ZZ]),
            S
  end;

%exp1_inner(end_of_trace = _Else, #state{pid=Pid, output_path=OutputPath, acc=Acc} = S) ->
%    {ok, FH} = file:open(OutputPath, [write, raw, binary, delayed_write]),
%    io:format("Writing to ~s ... ", [OutputPath]),
%    [begin
%         Pid_str = io_lib:format("~w", [Pid]),
%         Time_str = integer_to_list(Time),
%         file:write(FH, [Pid_str, $;, intersperse($;, lists:reverse(Stack)), 32, Time_str, 10])
%     end || {Stack, Time} <- Acc],
%    file:close(FH),
%    io:format("finished\n"),
%    S;
exp1_inner(_Else, S) ->
%    io:format("?? ~P\n", [_Else, 10]),
    S.

find_matching_stack(MFA_bin, [{H,_Time}|_] = Acc) ->
    case lists:member(MFA_bin, H) of
        true ->
            H;
        false ->
            find_matching_stack2(MFA_bin, Acc)
    end.

find_matching_stack2(MFA_bin, [{[MFA_bin|_StackTail]=Stack,_Time}|_]) ->
    Stack;
find_matching_stack2(MFA_bin, [_H|T]) ->
    find_matching_stack2(MFA_bin, T);
find_matching_stack2(_MFA_bin, []) ->
    [<<"FIND-MATCHING-FAILED">>].    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

intersperse(_, []) -> [];
intersperse(_, [X]) -> [X];
intersperse(Sep, [X | Xs]) -> [X, Sep | intersperse(Sep, Xs)].

stak_trim([<<"proc_lib:init_p_do_apply/3">>,<<"gen_fsm:decode_msg/9">>,<<"gen_fsm:handle_msg/7">>,<<"gen_fsm:loop/7">>|T]) ->
    stak_trim([<<"GEN-FSM">>|T]);
stak_trim([<<"GEN-FSM">>,<<"gen_fsm:decode_msg/9">>,<<"gen_fsm:handle_msg/7">>,<<"gen_fsm:loop/7">>|T]) ->
    stak_trim([<<"GEN-FSM">>|T]);
stak_trim(Else) ->
    Else.

stak_binify(Bin) when is_binary(Bin) ->
    [list_to_binary(X) || X <- stak(Bin)];
stak_binify(X) ->
    list_to_binary(io_lib:format("~w", [X])).

mfa_binify({M,F,A}) ->
    list_to_binary(io_lib:format("~w:~w/~w", [M, F, A]));
mfa_binify(X) ->
    list_to_binary(io_lib:format("~w", [X])).

%% Borrowed from redbug.erl

stak(Bin) ->
  lists:foldl(fun munge/2,[],string:tokens(binary_to_list(Bin),"\n")).

munge(I,Out) ->
  case I of %% lists:reverse(I) of
    "..."++_ -> ["truncated!!!"|Out];
    _ ->
      case string:str(I, "Return addr") of
        0 ->
          case string:str(I, "cp = ") of
            0 -> Out;
            _ -> [mfaf(I)|Out]
          end;
        _ ->
          case string:str(I, "erminate process normal") of
            0 -> [mfaf(I)|Out];
            _ -> Out
          end
      end
  end.

mfaf(I) ->
  [_, C|_] = string:tokens(I,"()+"),
  string:strip(C).
