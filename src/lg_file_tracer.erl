-module(lg_file_tracer).

-export([start_link/2]).
-export([init/2]).

-export([system_continue/3]).
-export([system_terminate/4]).
-export([system_code_change/4]).

-record(state, {
    parent :: pid(),
    filename :: file:filename_all(),
    io_device :: file:io_device()
}).

start_link(Nth, Filename0) ->
    Filename = filename:flatten([Filename0, ".", integer_to_list(Nth)]),
    Pid = proc_lib:spawn_link(?MODULE, init, [self(), Filename]),
    {ok, Pid}.

init(Parent, Filename) ->
    %% No need to close the file, it'll be closed when the process exits.
    {ok, IoDevice} = file:open(Filename, [write, delayed_write, compressed]),
    loop(#state{parent=Parent, filename=Filename, io_device=IoDevice}).

loop(State=#state{parent=Parent, io_device=IoDevice}) ->
    receive
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [], State);
        Msg ->
            Bin = term_to_binary(Msg),
            BinSize = byte_size(Bin),
            %% @todo This gets badmatch if we q().
            ok = file:write(IoDevice, [<<BinSize:16>>, Bin]),
            loop(State)
    end.

system_continue(_, _, State) ->
    loop(State).

-spec system_terminate(any(), _, _, _) -> no_return().
system_terminate(Reason, _, _, _) ->
    exit(Reason).

system_code_change(Misc, _, _, _) ->
    {ok, Misc}.
