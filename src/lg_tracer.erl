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

-module(lg_tracer).
%-behavior(erl_tracer).

-export([enabled/3]).
-export([enabled_call/3]).
-export([enabled_procs/3]).
-export([enabled_running_procs/3]).
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

enabled_call(_, _, _) ->
    erlang:nif_error({not_loaded, ?MODULE}).

enabled_procs(_, _, _) ->
    erlang:nif_error({not_loaded, ?MODULE}).

enabled_running_procs(_, _, _) ->
    erlang:nif_error({not_loaded, ?MODULE}).

trace(_, _, _, _, _) ->
    erlang:nif_error({not_loaded, ?MODULE}).
