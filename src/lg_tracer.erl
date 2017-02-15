-module(lg_tracer).
%-behavior(erl_tracer).

-export([enabled/3]).
-export([trace/5]).

-on_load(on_load/0).
on_load() ->
    case code:priv_dir(looking_glass) of
        {error, _} ->
            {error, {load_failed, "Could not determine the looking_glass priv/ directory."}};
        Path ->
            erlang:load_nif(filename:join(Path, atom_to_list(?MODULE)), 0)
    end.

enabled(_, _, _) ->
    erlang:nif_error({not_loaded, ?MODULE}).

trace(_, _, _, _, _) ->
    erlang:nif_error({not_loaded, ?MODULE}).
