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

-module(lg_callgrind).

-export([patterns/0]).
-export([profile/2]).
-export([profile/3]).
-export([profile_many/2]).
-export([profile_many/3]).

%% @todo Add an option with a list of modules to exclude.

-type opts() :: #{
    %% Whether we filter the output per process.
    scope => global | per_process,
    %% Whether we compute and save wait times.
    running => boolean()
}.

-record(call, {
    %% The MFA for the call.
    mfa :: atom(),
    %% The source file name.
    source :: {string(), pos_integer()},
    %% The timestamp for the call.
    ts :: pos_integer(),
    %% The timestamp for when we last started executing this function.
    self_ts :: pos_integer(),
    %% Execution time including subcalls.
    incl :: undefined | non_neg_integer(),
    %% Execution time excluding subcalls.
    self = 0 :: integer(),
    %% Number of times the function was called.
    count = 1 :: pos_integer(),
    %% Time when the process was not running in this function.
    wait = 0 :: non_neg_integer(),
    %% Time when the process was not running in this function or any subcalls.
    wait_incl = 0 :: non_neg_integer(),
    %% Number of times the process was scheduled out.
    wait_count = 0 :: non_neg_integer(),
    %% Number of times the function or any subcall was scheduled out.
    wait_count_incl = 0 :: non_neg_integer(),
    %% Calls done by this MFA.
    calls = #{} :: #{atom() => #call{}}
}).

-record(proc, {
    %% Call stack.
    stack = [] :: [#call{}],
    %% Profile information waiting to be written to file.
    mfas = #{} :: #{atom() => #call{}},
    %% Timestamp the process got scheduled out.
    out = undefined :: undefined | non_neg_integer()
}).

-record(state, {
    %% Input file name.
    input :: file:filename_all(),
    %% Output file name.
    output :: file:filename_all(),
    %% Output fd.
    output_device :: file:io_device(),
    %% Options.
    opts :: opts(),
    %% List of processes.
    processes = #{} :: #{pid() => #proc{}},
    %% Cache of source file information.
    sources = #{} :: #{mfa() => {string(), pos_integer()}}
}).

-spec patterns() -> lg:input().
patterns() ->
    [{app, kernel}, {app, stdlib}, {app, looking_glass}].

-spec profile(file:filename_all(), file:filename_all()) -> ok.
profile(Input, Output) ->
    profile(Input, Output, #{}).

-spec profile(file:filename_all(), file:filename_all(), opts()) -> ok.
profile(Input, Output, Opts) ->
    {ok, OutDevice} = file:open(Output, [write]),
    State = #state{input=Input, output=Output, output_device=OutDevice, opts=Opts},
    write_header(State),
    {ok, FinalState} = lg_file_reader:fold(fun handle_event/2, State, Input),
    flush(FinalState),
    _ = file:close(OutDevice),
    ok.

flush(State=#state{processes=Procs}) ->
    maps:fold(fun(Pid, #proc{mfas=MFAs}, _) ->
        write_mfas(Pid, MFAs, State)
    end, undefined, Procs),
    ok.

-spec profile_many(file:filename(), file:filename()) -> ok.
profile_many(Wildcard, Prefix) ->
    profile_many(Wildcard, Prefix, #{}).

-spec profile_many(file:filename(), file:filename(), opts()) -> ok.
profile_many(Wildcard, Prefix, Opts) ->
    Files = filelib:wildcard(Wildcard),
    Seq = lists:seq(1, length(Files)),
    OutFiles = [Prefix ++ "." ++ integer_to_list(N) || N <- Seq],
    Many = lists:zip(Files, OutFiles),
    Refs = [monitor(process, spawn_link(?MODULE, profile, [Input, Output, Opts]))
        || {Input, Output} <- Many],
    wait_for_procs(Refs).

%% We do not need to worry about failure because we are linked.
wait_for_procs([]) ->
    ok;
wait_for_procs(Refs) ->
    receive
        %% We purposefully ignore any stray messages.
        {'DOWN', R, process, _, _} ->
            wait_for_procs(Refs -- [R])
    end.

%% We handle trace events one by one, keeping track of the
%% execution stack for each process.

%% We don't care about match spec results for callgrind.
handle_event({call, Pid, Ts, MFA, _MSpec}, State) ->
    handle_event({call, Pid, Ts, MFA}, State);
handle_event({call, Pid, Ts, MFA}, State0) ->
    Proc = case is_process_profiled(Pid, State0) of
        {true, P} -> P;
        {empty, P} -> P;
        false -> #proc{}
    end,
    {Source, State} = find_source(MFA, State0),
    handle_call(Pid, convert_mfa(MFA), Source, Ts, Proc, State);
handle_event({return_to, Pid, Ts, MFA}, State) ->
    case is_process_profiled(Pid, State) of
        {true, Proc} -> handle_return_to(Pid, convert_mfa(MFA), Ts, Proc, State);
        _ -> State
    end;
%% Process exited. Unfold the stacktrace entirely.
%%
%% We use the atom exit because we know it will not match
%% a function call and will therefore unfold everything.
handle_event({exit, Pid, Ts, _Reason}, State0) ->
    case is_process_profiled(Pid, State0) of
        {true, Proc} ->
            State=#state{processes=Procs} = handle_return_to(Pid, exit, Ts, Proc, State0),
            %% Remove the pid from the state to save memory.
            State#state{processes=maps:remove(Pid, Procs)};
        _ ->
            State0
    end;
handle_event({in, Pid, Ts, _MFA}, State=#state{opts=#{running := true}}) ->
    case is_process_profiled(Pid, State) of
        {true, Proc} -> handle_in(Pid, Ts, Proc, State);
        _ -> State
    end;
handle_event({out, Pid, Ts, _MFA}, State=#state{opts=#{running := true}}) ->
    case is_process_profiled(Pid, State) of
        {true, Proc} -> handle_out(Pid, Ts, Proc, State);
        _ -> State
    end;
%% Ignore all other events. We do not need them for building the callgrind file.
handle_event(_, State) ->
    State.

is_process_profiled(Pid, #state{processes=Procs}) ->
    case maps:get(Pid, Procs, undefined) of
        %% We never received events for this process. Ignore.
        undefined -> false;
        %% We received events but are not in a known function currently. Ignore.
        Proc=#proc{stack=[]} -> {empty, Proc};
        %% All good!
        Proc -> {true, Proc}
    end.

%% We track a number of different things:
%% - how much time was spent in the different function calls
%% - how much time they spent calling other functions
%% - how many times functions were called
%%
%% We track everything on a per process basis. For each process,
%% we maintain a call stack. Every time a function return, we may
%% end up saving call information to the 'mfas' map. We then write
%% this information to the disk whenever the stacktrace becomes
%% empty, or when the process terminates.

%% When we receive a call event, we add the call information
%% to the stack, regardless of what it already contains.
%% This means that recursive calls, whether tail or body,
%% will appear multiple times in the stack. And since Erlang
%% doesn't have loops, it will appear a little weird if
%% compared to an imperative language.

%% Recursive call. Just increase the call count.
handle_call(Pid, MFA, _Source, _Ts,
        Proc0=#proc{stack=[Call=#call{mfa=MFA, count=Count}|Stack0]},
        State=#state{processes=Procs}) ->
    Stack = [Call#call{count=Count + 1}|Stack0],
    Proc = Proc0#proc{stack=Stack},
    State#state{processes=Procs#{Pid => Proc}};
%% Non-recursive call.
handle_call(Pid, MFA, Source, Ts, Proc0=#proc{stack=Stack0},
        State=#state{processes=Procs}) ->
    Stack = [#call{mfa=MFA, source=Source, ts=Ts, self_ts=Ts}|Stack0],
    Proc = Proc0#proc{stack=Stack},
    State#state{processes=Procs#{Pid => Proc}}.

%% We return from the current call, so the current call
%% ends regardless of what it was doing. We stop as soon
%% as we see the caller we return to; or if we return all
%% the way up, higher than where we started (for example
%% because we were not tracing the function we actually
%% end up returning to), we get everything.
%%
%% The current call started when it was called and stopped
%% on the return_to timestamp. Therefore it is fairly simple
%% to calculate its incl/self times.
%%
%% Other calls returning at the same time are tail calls.
%% In their case, the incl time is the same as for the
%% current call. However the self time must not stop when
%% returning but rather when doing the final tail call.
%% We also update sub call times since those must be
%% maintained separately.
%%
%% NOTE: Due to how the VM works, if a function has both
%% tail and non-tail calls, it becomes impossible to know
%% what is or is not a tail call, and therefore values
%% may be wrong. Do not write such functions! For example:
%%
%%    a(true) -> 1 + b(); a(false) -> b().
%%
%% Finally we must also update the self for the call we
%% actually return to. In its case we use the time we
%% were last executing the function as a start point,
%% and the return time for the end. Here again we also
%% update the sub call times.

handle_return_to(Pid, MFA, Ts, Proc0=#proc{stack=[Current0|Stack0], mfas=MFAs0},
        State=#state{processes=Procs}) ->
    {Returned0, Stack1} = lists:splitwith(
        fun(#call{mfa=E}) -> E =/= MFA end,
        Stack0),
    #call{ts=CurrentTs, self_ts=CurrentSelfTs, self=CurrentSelf} = Current0,
    Current = Current0#call{incl=Ts - CurrentTs, self=CurrentSelf + Ts - CurrentSelfTs},
    Returned = update_tail_calls([Current|Returned0], Ts),
    Stack = update_stack(Returned, Stack1, Ts),
    %% Save the profile information in the state, potentially flushing it
    %% to disk if the stack is empty.
    MFAs1 = update_mfas(Returned, MFAs0),
    MFAs = case Stack of
        [] ->
            write_mfas(Pid, MFAs1, State),
            #{};
        _ ->
            MFAs1
    end,
    Proc = Proc0#proc{stack=Stack, mfas=MFAs},
    State#state{processes=Procs#{Pid => Proc}}.

update_tail_calls([Call], _) ->
    [Call];
update_tail_calls([
            Callee=#call{ts=CalleeTs},
            Caller0=#call{ts=CallerTs, self_ts=CallerSelfTs, self=CallerSelf}
        |Tail], ReturnToTs) ->
    Caller1 = Caller0#call{
        incl=ReturnToTs - CallerTs,
        self=CallerSelf + CalleeTs - CallerSelfTs
    },
    Caller = update_sub_calls(Callee, Caller1),
    [Callee|update_tail_calls([Caller|Tail], ReturnToTs)].

%% Update nothing; there's nothing in the stack.
update_stack(_, [], _) ->
    [];
%% Update the incl/self value based on the top-level function we return from,
%% but only update the sub calls based on the function we directly called.
update_stack(Returned,
        [Caller0=#call{self_ts=CallerSelfTs, self=CallerSelf}|Stack],
        ReturnToTs) ->
    Callee = #call{ts=CalleeTs} = hd(lists:reverse(Returned)),
    Caller = Caller0#call{
        self_ts=ReturnToTs,
        self=CallerSelf + CalleeTs - CallerSelfTs
    },
    [update_sub_calls(Callee, Caller)|Stack].

update_sub_calls(Callee=#call{mfa=MFA, incl=CallerIncl, count=CallerCount,
        wait_incl=CallerWaitIncl}, Caller=#call{calls=SubCalls}) ->
    case maps:get(MFA, SubCalls, undefined) of
        %% Add the callee to the subcalls but remove the callee's subcalls
        %% since we don't need those here.
        undefined ->
            Caller#call{calls=SubCalls#{MFA => Callee#call{calls=#{}}}};
        %% Same as above, except we add to the existing values.
        Sub = #call{incl=SubIncl, count=SubCount, wait_incl=SubWaitIncl} ->
            Caller#call{calls=SubCalls#{MFA => Sub#call{
                %% We do not care about self/wait here as we will be using incl/wait_incl in the output.
                incl=SubIncl + CallerIncl,
                count=SubCount + CallerCount,
                wait_incl=SubWaitIncl + CallerWaitIncl
            }}}
    end.

%% Processes get scheduled in and out. We get the corresponding
%% in and out events when the 'running' option is set to true.
%% We keep track of how many times the process was scheduled out
%% per function, and how long.

handle_in(Pid, InTs, Proc0=#proc{stack=[Current0|Stack0], out=OutTs},
        State=#state{processes=Procs}) ->
    #call{wait=Wait, wait_incl=WaitIncl,
        wait_count=WaitCount, wait_count_incl=WaitCountIncl
    } = Current0,
    ThisWait = InTs - OutTs,
    %% We increase the wait time for self first.
    Current = Current0#call{wait=Wait + ThisWait, wait_incl=WaitIncl + ThisWait,
        wait_count=WaitCount + 1, wait_count_incl=WaitCountIncl + 1},
    %% And then for the parent calls to include wait time of subcalls.
    Stack = [
        Call#call{wait_incl=ParentWaitIncl + ThisWait, wait_count_incl=ParentWaitCount + 1}
    || Call=#call{wait_incl=ParentWaitIncl, wait_count_incl=ParentWaitCount} <- Stack0],
    Proc = Proc0#proc{stack=[Current|Stack], out=undefined},
    State#state{processes=Procs#{Pid => Proc}}.

handle_out(Pid, Ts, Proc0=#proc{out=undefined},
        State=#state{processes=Procs}) ->
    Proc = Proc0#proc{out=Ts},
    State#state{processes=Procs#{Pid => Proc}}.

%% Update the profiling information we currently hold.
update_mfas([], MFAs) ->
    MFAs;
update_mfas([Call=#call{mfa=MFA, incl=Incl, self=Self, wait=Wait, wait_incl=WaitIncl,
        wait_count=WaitCount, wait_count_incl=WaitCountIncl,
        count=Count, calls=SubCalls}|Tail], MFAs) ->
    case MFAs of
        #{MFA := AggCall0=#call{incl=AggIncl, self=AggSelf, wait=AggWait, wait_incl=AggWaitIncl,
                wait_count=AggWaitCount, wait_count_incl=AggWaitCountIncl,
                count=AggCount, calls=AggSubCalls0}} ->
            AggSubCalls = update_mfas(maps:values(SubCalls), AggSubCalls0),
            AggCall=AggCall0#call{incl=Incl + AggIncl, self=Self + AggSelf,
                wait=Wait + AggWait, wait_incl=WaitIncl + AggWaitIncl,
                wait_count=WaitCount + AggWaitCount,
                wait_count_incl=WaitCountIncl + AggWaitCountIncl,
                count=Count + AggCount, calls=AggSubCalls},
            update_mfas(Tail, MFAs#{MFA => AggCall});
        _ ->
            update_mfas(Tail, MFAs#{MFA => Call})
    end.

%% The callgrind format is documented at http://valgrind.org/docs/manual/cl-format.html
%%
%% We currently only store the real time spent in the calls
%% (including wait times).
%%
%% The option 'scope' can be used to enable per process tracking.

write_header(#state{output_device=OutDevice, opts=#{running := true}}) ->
    ok = file:write(OutDevice,
        "# callgrind format\n"
        "events: Total Active Wait WaitCount\n"
        "event: Total : Total time in microseconds\n"
        "event: Active : Active time in microseconds\n"
        "event: Wait : Wait time in microseconds (scheduled out)\n"
        "event: WaitCount : Number of times the process was scheduled out\n"
        "\n");
write_header(#state{output_device=OutDevice}) ->
    ok = file:write(OutDevice,
        "# callgrind format\n"
        "events: Total\n"
        "event: Total : Total time in microseconds\n"
        "\n").

write_mfas(Pid, MFAs, State) ->
    _ = [write_call(Pid, Call, State) || Call <- maps:values(MFAs)],
    ok.

write_call(Pid, #call{mfa=MFA, source={Source, LN0}, self=Self, wait=Wait,
        wait_count=WaitCount, calls=Calls0},
        #state{output_device=OutDevice, opts=Opts}) ->
    LN = line_number(LN0),
    Calls = maps:values(Calls0),
    Ob = case Opts of
        #{scope := per_process} ->
            ["ob=", io_lib:write(Pid), "\n"];
        _ ->
            []
    end,
    RunningCosts = case Opts of
        #{running := true} ->
            [
                " ", integer_to_list(Self - Wait),
                " ", integer_to_list(Wait),
                " ", integer_to_list(WaitCount)
            ];
        _ ->
            []
    end,
    file:write(OutDevice, [Ob,
        "fl=", Source, "\n"
        "fn=", atom_to_list(MFA), "\n",
        integer_to_list(LN), " ", integer_to_list(Self), RunningCosts, "\n",
        format_subcalls(LN, Calls, Opts),
        "\n"]).

format_subcalls(_, [], _) ->
    [];
%% @todo We don't need to write the filename for functions in the same module.
%% @todo We also don't want to put the full file name; instead we should remove
%% the prefix (path to the release).
%%
%% We only look at where the function is defined, we can't really get
%% the actual line number where the call happened, unfortunately.
format_subcalls(LN0, [#call{mfa=MFA, source={Source, TargetLN0}, incl=Incl,
        wait_incl=Wait, wait_count_incl=WaitCount, count=Count, calls=_Calls}|Tail], Opts) ->
    LN = line_number(LN0),
    TargetLN = line_number(TargetLN0),
    RunningCosts = case Opts of
        #{running := true} ->
            [
                " ", integer_to_list(Incl - Wait),
                " ", integer_to_list(Wait),
                " ", integer_to_list(WaitCount)
            ];
        _ ->
            []
    end,
    [[
        "cfi=", Source, "\n"
        "cfn=", atom_to_list(MFA), "\n"
        "calls=", integer_to_list(Count), " ", integer_to_list(TargetLN), "\n",
        integer_to_list(LN), " ", integer_to_list(Incl), RunningCosts, "\n"
    ]|format_subcalls(LN, Tail, Opts)].

%% Starting from OTP-24 we now have {Line, Column}.
line_number({LN, _}) -> LN;
line_number(LN) -> LN.

convert_mfa(undefined) ->
    undefined;
convert_mfa({M0, F0, A0}) ->
    M = atom_to_binary(M0, latin1),
    F = atom_to_binary(F0, latin1),
    A = integer_to_binary(A0),
    binary_to_atom(<<M/binary, $:, F/binary, $/, A/binary>>, latin1).

find_source(MFA, State0=#state{sources=Cache}) ->
    case Cache of
        #{MFA := Source} ->
            {Source, State0};
        _ ->
            State = #state{sources=#{MFA := Source}} = cache_module(MFA, State0),
            {Source, State}
    end.

%% We extract the line number of the functions by loading the
%% beam file (which is already loaded when we reach this function)
%% and looking into the abstract code directly. When something
%% goes wrong, for example the module was not compiled with
%% +debug_info, the function will return line number 1.
%%
%% Note that we can only retrieve the location of the function.
%% For functions with many clauses we are not able to properly
%% identify which clause was involved. It's probably a good
%% idea to structure your code to have more functions than
%% function clauses, especially when using behaviours.
%%
%% While this is an expensive operation, we cache the result
%% and therefore this function will only be called once per module.
cache_module(MFA={Module, _, _}, State0=#state{sources=Cache}) ->
    try
        %% If the module is in the path, we can simply query
        %% it for the source file.
        Info = Module:module_info(compile),
        %% @todo We don't want to return an absolute path,
        %% but rather the app/src/file.erl path if it's in
        %% an application, or just folder/file.erl if not.
        %% This allows different users to point to the
        %% same source at different locations on their machine.
        {_, Src} = lists:keyfind(source, 1, Info),
        cache_module(MFA, State0, Src)
    catch _:_ ->
        %% Either the module was not found, or it doesn't
        %% have a 'source' key in the compile info.
        %%
        %% We can't cache the module; on the other hand
        %% we can cache the result of this operation.
        %% Just append .erl to the module name and set the
        %% line number to 1, which is of course incorrect.
        State0#state{sources=Cache#{MFA => {atom_to_list(Module) ++ ".erl", 1}}}
    end.

cache_module(MFA={Module, _, _}, State=#state{sources=Cache0}, Src) ->
    {Module, Beam, _} = code:get_object_code(Module),
    {ok, {Module, Chunks}} = beam_lib:chunks(Beam, [abstract_code]),
    [{abstract_code, {raw_abstract_v1, Forms}}] = Chunks,
    Funcs = [{{Module, F, A}, LN} || {function, LN, F, A, _} <- Forms],
    Cache1 = lists:foldl(fun({Key, LN}, Acc) -> Acc#{Key => {Src, LN}} end, Cache0, Funcs),
    %% We cannot currently retrieve line number information
    %% for list comprehensions and funs. We therefore
    %% cache the correct file with line number set to 1.
    Cache = case Cache1 of
        #{MFA := _} -> Cache1;
        _ -> Cache1#{MFA => {Src, 1}}
    end,
    State#state{sources=Cache}.
