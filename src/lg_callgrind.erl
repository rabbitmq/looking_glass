-module(lg_callgrind).

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
    source :: string(),
    %% The timestamp for the call.
    ts :: pos_integer(),
    %% Execution time including subcalls.
    incl :: undefined | non_neg_integer(),
    %% Execution time excluding subcalls.
    self = 0 :: integer(),
    %% Number of times the function was called.
    count = 1 :: pos_integer(),
    %% Time when the process was not running.
    wait = 0 :: non_neg_integer(),
    %% Number of times the process was scheduled out.
    wait_count = 0 :: non_neg_integer(),
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
    sources = #{} :: #{module() => string()}
}).

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
    %% @todo Do it in parallel.
    _ = [profile(Input, Output, Opts) || {Input, Output} <- Many],
    ok.

%% We handle trace events one by one, keeping track of the
%% execution stack for each process.

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

handle_call(Pid, MFA, Source, Ts, Proc0=#proc{stack=Stack0},
        State=#state{processes=Procs}) ->
    %% @todo As an optimization, we should probably increase the
    %% call count instead of adding to the stack. This requires
    %% us to track the number of returns from the function.
    Stack = [#call{mfa=MFA, source=Source, ts=Ts}|Stack0],
    Proc = Proc0#proc{stack=Stack},
    State#state{processes=Procs#{Pid => Proc}}.

%% We return from the current call, so the current call
%% ends regardless of what it was doing. We stop as soon
%% as we see the caller we return to; or if we return all
%% the way up, higher than where we started (for example
%% because we were not tracing the function we actually
%% end up returning to), we get everything.

handle_return_to(Pid, MFA, Ts, Proc0=#proc{stack=[Current|Stack0], mfas=MFAs0},
        State=#state{processes=Procs}) ->
    {Returned0, Stack1} = lists:splitwith(
        fun(#call{mfa=E}) -> E =/= MFA end,
        Stack0),
    Returned1 = [Current|Returned0],
    %% First we calculate the time spent in all the calls that returned.
    %% This is the time including all sub calls. We also add the time
    %% difference to self. Self might be negative then if it calls many
    %% functions, as update_parent_call might substract subcall time
    %% while the function is in the stack.
    Returned2 = [Call#call{incl=Ts - CallTs, self=Self + Ts - CallTs}
        || Call=#call{ts=CallTs, self=Self} <- Returned1],
    %% Then we update all the sub-calls for all functions returned,
    %% and the first function in the new stack (the function we
    %% return to).
    {Returned, Stack} = update_calls(Returned2, Stack1),
    %% Last, we save the profile information, potentially flushing it
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

%% Processes get scheduled in and out. We get the corresponding
%% in and out events when the 'running' option is set to true.
%% We keep track of how many times the process was scheduled out
%% per function, and how long.

handle_in(Pid, InTs, Proc0=#proc{stack=[Current0|Stack], out=OutTs},
        State=#state{processes=Procs}) ->
    #call{wait=Wait, wait_count=WaitCount} = Current0,
    Current = Current0#call{wait=Wait + InTs - OutTs, wait_count=WaitCount + 1},
    Proc = Proc0#proc{stack=[Current|Stack], out=undefined},
    State#state{processes=Procs#{Pid => Proc}}.

handle_out(Pid, Ts, Proc0=#proc{out=undefined},
        State=#state{processes=Procs}) ->
    Proc = Proc0#proc{out=Ts},
    State#state{processes=Procs#{Pid => Proc}}.

%% Update the call we return to in the stack.
update_calls(Returned=[Call], [ParentCall0|Stack]) ->
    ParentCall = update_parent_call(Call, ParentCall0),
    {Returned, [ParentCall|Stack]};
%% Update nothing; there's nothing in the stack.
update_calls(Returned=[_], Stack=[]) ->
    {Returned, Stack};
%% Update the parent function that returned at the same time.
%% This occurs when the function call was tail recursive,
%% and the MFA is different between caller and callee.
update_calls([Call, ParentCall0|Tail], Stack0) ->
    ParentCall = update_parent_call(Call, ParentCall0),
    {[_|Returned], Stack} = update_calls([ParentCall|Tail], Stack0),
    {[Call, ParentCall|Returned], Stack}.

update_parent_call(Call=#call{mfa=MFA, incl=Incl},
        ParentCall=#call{self=ParentSelf, calls=SubCalls}) ->
    case maps:get(MFA, SubCalls, undefined) of
        undefined ->
            %% We substract the time spent in the callee from the caller
            %% and remove any callee subcalls as we don't need those.
            ParentCall#call{self=ParentSelf - Incl, calls=SubCalls#{MFA => Call#call{calls=#{}}}};
        _ ->
            %% Same as above, except we don't touch the sub calls.
            %% Values get updated in update_mfas/2.
            ParentCall#call{self=ParentSelf - Incl}
    end.

%% Update the profiling information we currently hold.
update_mfas([], MFAs) ->
    MFAs;
update_mfas([Call=#call{mfa=MFA, incl=Incl, self=Self, count=Count, calls=SubCalls}|Tail], MFAs) ->
    case MFAs of
        #{MFA := AggCall0=#call{incl=AggIncl, self=AggSelf, count=AggCount, calls=AggSubCalls0}} ->
            AggSubCalls = update_mfas(maps:values(SubCalls), AggSubCalls0),
            AggCall=AggCall0#call{incl=Incl + AggIncl, self=Self + AggSelf,
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
        "events: Total Active Wait WaitCount\n"
        "event: Total : Total time in microseconds\n"
        "event: Active : Active time in microseconds\n"
        "event: Wait : Wait time in microseconds (scheduled out)\n"
        "event: WaitCount : Number of times the process was scheduled out\n"
        "\n");
write_header(#state{output_device=OutDevice}) ->
    ok = file:write(OutDevice,
        "events: Total\n"
        "event: Total : Total time in microseconds\n"
        "\n").

write_mfas(Pid, MFAs, State) ->
    _ = [write_call(Pid, Call, State) || Call <- maps:values(MFAs)],
    ok.

write_call(Pid, #call{mfa=MFA, source={Source, LN}, self=Self, wait=Wait,
        wait_count=WaitCount, calls=Calls0},
        #state{output_device=OutDevice, opts=Opts}) ->
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
        format_subcalls(LN, Calls),
        "\n"]).

format_subcalls(_, []) ->
    [];
%% @todo We don't need to write the filename for functions in the same module.
%% @todo We also don't want to put the full file name; instead we should remove
%% the prefix (path to the release).
%%
%% We only look at where the function is defined, we can't really get
%% the actual line number where the call happened, unfortunately.
format_subcalls(LN, [#call{mfa=MFA, source={Source, TargetLN}, incl=Incl, count=Count, calls=_Calls}|Tail]) ->
    [[
        "cfi=", Source, "\n"
        "cfn=", atom_to_list(MFA), "\n"
        "calls=", integer_to_list(Count), " ", integer_to_list(TargetLN), "\n",
        integer_to_list(LN), " ", integer_to_list(Incl), "\n"
    ]|format_subcalls(LN, Tail)].

convert_mfa(undefined) ->
    undefined;
convert_mfa({M0, F0, A0}) ->
    M = atom_to_binary(M0, latin1),
    F = atom_to_binary(F0, latin1),
    A = integer_to_binary(A0),
    binary_to_atom(<<M/binary, $:, F/binary, $/, A/binary>>, latin1).

find_source(MFA, State=#state{sources=Cache}) ->
    case Cache of
        #{MFA := Source} -> {Source, State};
        _ -> do_find_source(MFA, State)
    end.

do_find_source(MFA={Module, _, _}, State=#state{sources=Cache}) ->
    Source = try
        %% If the module is in the path, we can simply query
        %% it for the source file.
        Info = Module:module_info(compile),
        {_, Src} = lists:keyfind(source, 1, Info),
        LN = find_line_number(MFA),
        %% @todo We don't want to return an absolute path,
        %% but rather the app/src/file.erl path if it's in
        %% an application, or just folder/file.erl if not.
        %% This allows different users to point to the
        %% same source at different locations on their machine.
        {Src, LN}
    catch _:_ ->
        %% If we couldn't use the beam file to retrieve the
        %% source file name, we just append .erl and hope for
        %% the best. The line number will of course be incorrect.
        {atom_to_list(Module) ++ ".erl", 1}
    end,
    {Source, State#state{sources=Cache#{MFA => Source}}}.

%% We extract the line number of the function by loading the
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
%% and therefore this function will not be called very often.
find_line_number({Module, Function, Arity}) ->
    try
        {Module, Beam, _} = code:get_object_code(Module),
        {ok, {Module, Chunks}} = beam_lib:chunks(Beam, [abstract_code]),
        [{abstract_code, {raw_abstract_v1, Forms}}] = Chunks,
        [{_, LN, _, _, _}] = [Form || Form={function, _, F, A, _} <-
            Forms, F =:= Function, A =:= Arity],
        LN
    catch _:_ ->
        %% We cannot currently retrieve line number information
        %% for list comprehensions and funs. We always return
        %% line number 1 in these cases.
        1
    end.
