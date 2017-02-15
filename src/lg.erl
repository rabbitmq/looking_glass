-module(lg).

%% @todo Check https://github.com/erlang/otp/pull/1328
%% which has a tip for making processes with big mailboxes
%% more efficient by avoiding copies on GC.

-export([trace/1]).
-export([trace/2]).
-export([stop/1]).

%% @todo {profile, M::atom(), F::atom()}
-type pattern() :: module() | {app, atom()}.
-type input() :: pattern() | [pattern()].

%% @todo Tracer that listens for one incoming connection and pushes everything through.

%% Options differ depending on the tracer module.
-type opts() :: any().

-spec trace(input()) -> {ok, pid()}.
trace(Input) ->
    trace(Input, lg_raw_console_tracer).

-spec trace(input(), module()) -> {ok, pid()}.
trace(Input, TracerMod) ->
    trace(Input, TracerMod, #{}).

-spec trace(input(), module(), opts()) -> {ok, pid()}.
trace(Input, TracerMod, Opts) when is_list(Input) ->
    do_trace(Input, TracerMod, Opts);
trace(Input, TracerMod, Opts) ->
    trace([Input], TracerMod, Opts).

do_trace(Input, TracerMod, Opts) ->
    %% @todo Remove eventually?
    _ = application:start(looking_glass),
    %% Start the pool of tracer processes.
    {ok, PoolPid} = lg_tracer_pool:start_link(10, TracerMod, Opts),
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
    _ = erlang:trace(processes, true, [
        call, procs, timestamp, arity, return_to,
        {tracer, lg_tracer, #{tracers => Tracers}}
    ]),
    trace_patterns(Input),
    {ok, PoolPid}.

trace_patterns(Input) ->
    lists:foreach(fun trace_pattern/1, Input).

trace_pattern({app, App}) when is_atom(App) ->
    {ok, Mods} = application:get_key(App, modules),
    trace_patterns(Mods);
trace_pattern(Mod) when is_atom(Mod) ->
    _ = erlang:trace_pattern({Mod, '_', '_'}, true, [local]).

stop(_PoolPid) ->
    todo.
