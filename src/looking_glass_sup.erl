-module(looking_glass_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    init_ets(),
	Procs = [],
	{ok, {{one_for_one, 1, 5}, Procs}}.

init_ets() ->
    looking_glass = ets:new(looking_glass, [named_table, public]),
    ok.
