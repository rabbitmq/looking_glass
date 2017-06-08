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
