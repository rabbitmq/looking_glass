-module(lg_tracer_pool).
-behaviour(supervisor).

-export([start_link/3]).
-export([init/1]).
-export([tracers/1]).

start_link(NumTracers, TracerMod, Opts) ->
    supervisor:start_link(?MODULE, [NumTracers, TracerMod, Opts]).

init([NumTracers, TracerMod, Opts]) ->
    Procs = [#{
        id => {tracer, N},
        start => {TracerMod, start_link, [N, Opts]},
        restart => temporary
    } || N <- lists:seq(1, NumTracers)],
    {ok, {#{strategy => one_for_all}, Procs}}.

tracers(PoolPid) ->
    [Pid || {_, Pid, _, _} <- supervisor:which_children(PoolPid)].
