-module(lg).

%% @todo Check https://github.com/erlang/otp/pull/1328
%% which has a tip for making processes with big mailboxes
%% more efficient by avoiding copies on GC.

%% @todo Tracer that listens for one incoming connection and pushes everything through.

-export([trace/1]).
-export([trace/2]).
-export([trace/3]).
-export([trace/4]).
-export([stop/0]).
-export([stop/1]).

-type pattern() :: module() | {app, atom()} | {callback, module(), atom()}.
-type scope() :: {scope, [
    pid() | port() | all | processes | ports |
    existing | existing_processes | existing_ports |
    new | new_processes | new_ports]}.

-type input() :: [pattern() | scope()].
-export_type([input/0]).

%% The trace functions input is not as strict for user convenience.
-type user_input() :: pattern() | input().

-type opts() :: #{
    mode => trace | profile,
    pool_id => any(),
    pool_size => pos_integer(),
    running => boolean()
}.

-spec trace(user_input()) -> ok.
trace(Input) ->
    trace(Input, lg_raw_console_tracer).

-spec trace(user_input(), module()) -> ok.
trace(Input, TracerMod) ->
    trace(Input, TracerMod, undefined, #{}).

-spec trace(user_input(), module(), any()) -> ok.
trace(Input, TracerMod, TracerOpts) ->
    trace(Input, TracerMod, TracerOpts, #{}).

-spec trace(user_input(), module(), any(), opts()) -> ok.
trace(Input, TracerMod, TracerOpts, Opts) when is_list(Input) ->
    do_trace(Input, TracerMod, TracerOpts, Opts);
trace(Input, TracerMod, TracerOpts, Opts) ->
    trace([Input], TracerMod, TracerOpts, Opts).

do_trace(Input0, TracerMod, TracerOpts, Opts) ->
    %% @todo Remove eventually?
    _ = application:ensure_all_started(looking_glass),
    %% Start the pool of tracer processes.
    PoolID = maps:get(pool_id, Opts, default),
    PoolSize = maps:get(pool_size, Opts, erlang:system_info(schedulers)),
    {ok, PoolPid} = supervisor:start_child(looking_glass_sup, #{
        id => PoolID,
        start => {lg_tracer_pool, start_link, [PoolSize, TracerMod, TracerOpts]},
        restart => temporary,
        type => supervisor
    }),
    Tracers = lg_tracer_pool:tracers(PoolPid),
    Mode = maps:get(mode, Opts, trace),
    Running = maps:get(running, Opts, false),
    Input1 = flatten(Input0, []),
    Input2 = ensure_pattern(Input1),
    Input = ensure_scope(Input2),
    trace_input(Input, #{mode => Mode, tracers => Tracers}, Running),
    ok.

flatten([], Acc) ->
    lists:flatten(Acc);
flatten([{callback, Mod, Fun}|Tail], Acc) when is_atom(Mod), is_atom(Fun) ->
    Input = flatten(Mod:Fun(), []),
    flatten(Tail, [Input|Acc]);
flatten([{app, App}|Tail], Acc) when is_atom(App) ->
    _ = application:load(App),
    {ok, Mods} = application:get_key(App, modules),
    flatten(Tail, [Mods|Acc]);
flatten([Input|Tail], Acc) ->
    flatten(Tail, [Input|Acc]).

ensure_pattern(Input) ->
    case [S || S={scope, _} <- Input] of
        Input -> ['_'|Input];
        _ -> Input
    end.

ensure_scope(Input) ->
    case [S || S={scope, _} <- Input] of
        [] -> [{scope, [processes]}|Input];
        _ -> Input
    end.

trace_input([], _, _) ->
    ok;
trace_input([{scope, Scope}|Tail], Opts, Running) ->
    %% We currently enable the following trace flags:
    %% - call: function calls
    %% - procs: process exit events; plus others we ignore
    %% - running: process being scheduled in/out
    %% - timestamp: events include the current timestamp
    %% - arity: function calls only include the arity, not arguments
    %% - return_to: return from functions
    %% - set_on_spawn: propagate trace flags to any children processes
    %%
    %% @todo It might be useful to count the number of sends
    %% or receives a function does.
    _ = [erlang:trace(PidPortSpec, true, [
            call, procs, timestamp, arity, return_to, set_on_spawn,
            {tracer, lg_tracer, Opts}
            |[running || Running]
        ])
    || PidPortSpec <- Scope],
    trace_input(Tail, Opts, Running);
trace_input([Mod|Tail], Opts, Running) when is_atom(Mod) ->
    %% The module must be loaded before we attempt to trace it.
    _ = code:load_file(Mod),
    _ = erlang:trace_pattern({Mod, '_', '_'}, true, [local]),
    trace_input(Tail, Opts, Running).

stop() ->
    stop(default).

%% @todo Confirm that we don't need to stop tracing,
%% that just terminating the tracers is enough. The
%% NIF does cancel traces when tracers go away, but
%% better make sure.
stop(PoolID) ->
    supervisor:terminate_child(looking_glass_sup, PoolID).
