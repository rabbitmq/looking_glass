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

%% @todo {profile, M::atom(), F::atom()}
-type pattern() :: module() | {app, atom()}.
-type input() :: pattern() | [pattern()].

-type opts() :: #{
    pool_id => any(),
    pool_size => pos_integer()
}.

-spec trace(input()) -> ok.
trace(Input) ->
    trace(Input, lg_raw_console_tracer).

-spec trace(input(), module()) -> ok.
trace(Input, TracerMod) ->
    trace(Input, TracerMod, undefined, #{}).

-spec trace(input(), module(), any()) -> ok.
trace(Input, TracerMod, TracerOpts) ->
    trace(Input, TracerMod, TracerOpts, #{}).

-spec trace(input(), module(), any(), opts()) -> ok.
trace(Input, TracerMod, TracerOpts, Opts) when is_list(Input) ->
    do_trace(Input, TracerMod, TracerOpts, Opts);
trace(Input, TracerMod, TracerOpts, Opts) ->
    trace([Input], TracerMod, TracerOpts, Opts).

do_trace(Input, TracerMod, TracerOpts, Opts) ->
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
    %% We currently enable the following trace flags:
    %% - call: function calls
    %% - procs: process exit events; plus others we ignore
    %% - timestamp: events include the current timestamp
    %% - arity: function calls only include the arity, not arguments
    %% - return_to: return from functions
    %%
    %% @todo We will need to add the 'running' trace flag
    %% to get events when the process gets scheduled in and out.
    %% We can then add the following infos to the callgrind file:
    %% - active execution time (without the wait times)
    %% - wait time
    %% - number of times the process got scheduled out
    %%   (can be useful to detect anomalies, for example
    %%   a process sending a message to a busy process can
    %%   get scheduled out)
    %%
    %% @todo It might be useful to count the number of sends
    %% or receives a function does.
    Mode = maps:get(mode, Opts, trace),
    _ = erlang:trace(processes, true, [
        call, procs, timestamp, arity, return_to,
        {tracer, lg_tracer, #{mode => Mode, tracers => Tracers}}
    ]),
    trace_patterns(Input),
    ok.

trace_patterns(Input) ->
    lists:foreach(fun trace_pattern/1, Input).

trace_pattern({app, App}) when is_atom(App) ->
    {ok, Mods} = application:get_key(App, modules),
    trace_patterns(Mods);
trace_pattern(Mod) when is_atom(Mod) ->
    %% The module must be loaded before we attempt to trace it.
    _ = code:load_file(Mod),
    _ = erlang:trace_pattern({Mod, '_', '_'}, true, [local]).

stop() ->
    stop(default).

%% @todo Confirm that we don't need to stop tracing,
%% that just terminating the tracers is enough. The
%% NIF does cancel traces when tracers go away, but
%% better make sure.
stop(PoolID) ->
    supervisor:terminate_child(looking_glass_sup, PoolID).
