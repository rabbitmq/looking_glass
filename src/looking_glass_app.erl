-module(looking_glass_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    looking_glass_sup:start_link().

stop(_State) ->
    ok.
