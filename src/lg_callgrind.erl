-module(lg_callgrind).

-export([profile/2]).
-export([profile/3]).

-type opts() :: #{
    %% Whether we filter the output per process.
    scope => global | per_process
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
    %% Calls done by this MFA.
    calls = #{} :: #{atom() => #call{}}
}).

-record(proc, {
    %% Call stack.
    stack = [] :: [#call{}],
    %% Profile information waiting to be written to file.
    mfas = #{} :: #{atom() => #call{}}
}).

-record(state, {
    %% Input file name.
    input :: file:file_name(),
    %% Output file name.
    output :: file:file_name(),
    %% Output fd.
    output_device :: file:io_device(),
    %% Options.
    opts :: opts(),
    %% List of processes.
    processes = #{} :: #{pid() => #proc{}}
}).

-spec profile(file:file_name(), file:file_name()) -> ok.
profile(Input, Output) ->
    profile(Input, Output, #{}).

profile(Input, Output, Opts) ->
    {ok, InDevice} = file:open(Input, [read, binary, compressed]),
    {ok, OutDevice} = file:open(Output, [write]),
    write_header(OutDevice),
    {ok, _State} = fold_file_head(InDevice, #state{
        input=Input, output=Output, output_device=OutDevice, opts=Opts}),
    _ = file:close(OutDevice),
    _ = file:close(InDevice),
    ok.

fold_file_head(IoDevice, State) ->
    case file:read(IoDevice, 5) of
        {ok, <<_, Size:32>>} ->
            fold_file_body(IoDevice, State, Size);
        eof ->
            %% @todo We need to write the MFAs we got pending before returning.
            {ok, State};
        {error, Reason} ->
            {error, Reason,
                'An error occurred while trying to read the size of an event in the file.'}
    end.

fold_file_body(IoDevice, State0, Size) ->
    case file:read(IoDevice, Size) of
        {ok, Bin} ->
            case process_body(Bin, State0) of
                {ok, State} ->
                    fold_file_head(IoDevice, State);
                Error ->
                    Error
            end;
        {error, Reason} ->
            {error, Reason,
                'An error occured while trying to read an event from the file.'}
    end.

process_body(Bin, State) ->
    try binary_to_term(Bin) of
        Term ->
            handle_event(Term, State)
    catch Class:Reason ->
        {error, {crash, Class, Reason},
            'The binary form of an event could not be decoded to an Erlang term.'}
    end.

%% We handle trace events one by one, keeping track of the
%% execution stack for each process.

handle_event({trace_ts, Pid, call, MFA, Ts}, State) ->
    handle_call(Pid, convert_mfa(MFA), find_source(MFA), convert_ts(Ts), State);
handle_event({trace_ts, Pid, return_to, MFA, Ts}, State) ->
    handle_return_to(Pid, convert_mfa(MFA), convert_ts(Ts), State);
%% Process exited. Unfold the stacktrace entirely.
%%
%% We use the atom exit because we know it will not match
%% a function call and will therefore unfold everything.
handle_event({trace_ts, Pid, exit, _Reason, Ts}, State0) ->
    {ok, State=#state{processes=Procs}}
        = handle_return_to(Pid, exit, convert_ts(Ts), State0),
    %% Remove the pid from the state to save memory.
    {ok, State#state{processes=maps:remove(Pid, Procs)}};
%% Ignore all other events. We do not need them for building the callgrind file.
handle_event(_, State) ->
    {ok, State}.

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

handle_call(Pid, MFA, Source, Ts, State=#state{processes=Procs}) ->
    Proc1 = case maps:get(Pid, Procs, undefined) of
        undefined -> #proc{};
        Proc0 -> Proc0
    end,
    #proc{stack=Stack0} = Proc1,
    %% @todo As an optimization, we should probably increase the
    %% call count instead of adding to the stack. This requires
    %% us to track the number of returns from the function.
    Stack = [#call{mfa=MFA, source=Source, ts=Ts}|Stack0],
    Proc = Proc1#proc{stack=Stack},
    {ok, State#state{processes=Procs#{Pid => Proc}}}.

%% We return from the current call, so the current call
%% ends regardless of what it was doing. We stop as soon
%% as we see the caller we return to; or if we return all
%% the way up, higher than where we started (for example
%% because we were not tracing the function we actually
%% end up returning to), we get everything.

handle_return_to(Pid, MFA, Ts, State=#state{processes=Procs}) ->
    case maps:get(Pid, Procs) of
        %% Depending on when tracing started, we might have an empty stack
        %% at the very beginning. We basically ignore the return_to in
        %% this case.
        #proc{stack=[]} ->
            {ok, State};
        %% Otherwise we process it.
        Proc1=#proc{stack=[Current|Stack0], mfas=MFAs0} ->
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
            Proc = Proc1#proc{stack=Stack, mfas=MFAs},
            {ok, State#state{processes=Procs#{Pid => Proc}}}
    end.

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

update_parent_call(Call=#call{mfa=MFA, incl=Incl, count=_Count},
        ParentCall=#call{self=Self, calls=SubCalls}) ->
    case maps:get(MFA, SubCalls, undefined) of
        undefined ->
            %% We substract the time spent in the callee from the caller
            %% and remove any callee subcalls as we don't need those.
            ParentCall#call{self=Self - Incl, calls=SubCalls#{MFA => Call#call{calls=#{}}}}
         %% @todo Need an example where MFA is defined. This is the
         %% case if for example we call the same function twice in
         %% a row.
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

write_header(OutDevice) ->
    ok = file:write(OutDevice, "events: Microseconds\n\n").

write_mfas(Pid, MFAs, State) ->
    _ = [write_call(Pid, Call, State) || Call <- maps:values(MFAs)],
    ok.

write_call(Pid, #call{mfa=MFA, source=Source, self=Self, calls=Calls0},
        #state{output_device=OutDevice, opts=Opts}) ->
    Calls = maps:values(Calls0),
    %% @todo It would be useful to look at the actual line number.
    LN = 1,
    Ob = case Opts of
        #{scope := per_process} ->
            io_lib:format("ob=~p~n", [Pid]);
        _ ->
            []
    end,
    file:write(OutDevice, [Ob, io_lib:format(
        "fl=~s~n"
        "fn=~s~n"
        "~p ~p~n"
        "~s"
        "~n",
        [Source, MFA, LN, Self, format_subcalls(Calls)])]).

format_subcalls([]) ->
    [];
%% @todo We don't need to write the filename for functions in the same module.
%% @todo We also don't want to put the full file name; instead we should remove
%% the prefix (path to the release).
format_subcalls([#call{mfa=MFA, source=Source, incl=Incl, count=Count, calls=_Calls}|Tail]) ->
    %% @todo It would be useful to look at the actual line number.
    LN = TargetLN = 1,
    [io_lib:format(
        "cfi=~s~n"
        "cfn=~s~n"
        "calls=~p ~p~n"
        "~p ~p~n",
        [Source, MFA, Count, TargetLN, LN, Incl])
    |format_subcalls(Tail)].

convert_mfa({M0, F0, A0}) ->
    M = atom_to_binary(M0, latin1),
    F = atom_to_binary(F0, latin1),
    A = integer_to_binary(A0),
    binary_to_atom(<<M/binary, $:, F/binary, $/, A/binary>>, latin1).

convert_ts({MegaSecs, Secs, MicroSecs}) ->
    MegaSecs * 1000000000000 + Secs * 1000000 + MicroSecs.

%% @todo What about the line number of the function being called?
find_source({Module, _, _}) ->
    try
        %% If the module is in the path, we can simply query
        %% it for the source file.
        Info = Module:module_info(compile),
        {_, Source} = lists:keyfind(source, 1, Info),
        %% @todo We don't want to return an absolute path,
        %% but rather the app/src/file.erl path if it's in
        %% an application, or just folder/file.erl if not.
        %% This allows different users to point to the
        %% same source at different locations on their machine.
        Source
    catch _:_ ->
        %% If we couldn't use the beam file to retrieve the
        %% source file name, we just append .erl and hope for
        %% the best.
        atom_to_list(Module) ++ ".erl"
    end.
