-module(lg_raw_console_tracer).

-export([start_link/2]).
-export([init/1]).
-export([loop/1]).

-export([system_continue/3]).
-export([system_terminate/4]).
-export([system_code_change/4]).

start_link(_Nth, _Opts) ->
    Pid = proc_lib:spawn_link(?MODULE, init, [self()]),
    {ok, Pid}.

init(Parent) ->
    %% Store all messages off the heap to avoid unnecessary GC.
    process_flag(message_queue_data, off_heap),
    loop(Parent).

loop(Parent) ->
    receive
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [], Parent);
        Msg ->
            erlang:display(Msg),
            loop(Parent)
    end.

system_continue(_, _, Parent) ->
    loop(Parent).

-spec system_terminate(any(), _, _, _) -> no_return().
system_terminate(Reason, _, _, _) ->
    exit(Reason).

system_code_change(Misc, _, _, _) ->
    {ok, Misc}.
