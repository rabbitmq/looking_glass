-module(lg_file_tracer).

-export([start_link/2]).
-export([init/2]).

-export([system_continue/3]).
-export([system_terminate/4]).
-export([system_code_change/4]).

-record(state, {
    parent :: pid(),
    filename :: file:filename_all(),
    io_device :: file:io_device(),
    events_per_frame = 100000 :: pos_integer(),
    events_this_frame = 0 :: non_neg_integer(),
    buffer = <<>> :: binary()
}).

start_link(Nth, Filename0) ->
    Filename = filename:flatten([Filename0, ".", integer_to_list(Nth)]),
    Pid = proc_lib:spawn_link(?MODULE, init, [self(), Filename]),
    {ok, Pid}.

init(Parent, Filename) ->
    %% No need to close the file, it'll be closed when the process exits.
    %% @todo We probably want to use the raw option.
    {ok, IoDevice} = file:open(Filename, [write, raw]),
    loop(#state{parent=Parent, filename=Filename, io_device=IoDevice}).

loop(State=#state{parent=Parent, io_device=IoDevice,
        events_per_frame=MaxEvents, events_this_frame=NumEvents0, buffer=Buffer0}) ->
    receive
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [], State);
        Msg ->
            Bin = term_to_binary(Msg),
            BinSize = byte_size(Bin),
            Buffer = <<Buffer0/binary, BinSize:16, Bin/binary>>,
            NumEvents = NumEvents0 + 1,
            if
                MaxEvents =:= NumEvents ->
                    ok = file:write(IoDevice, lz4f:compress_frame(Buffer)),
                    loop(State#state{events_this_frame=0, buffer= <<>>});
                true ->
                    loop(State#state{events_this_frame=NumEvents, buffer=Buffer})
            end
    end.

system_continue(_, _, State) ->
    loop(State).

-spec system_terminate(any(), _, _, #state{}) -> no_return().
system_terminate(Reason, _, _, #state{io_device=IoDevice, buffer=Buffer}) ->
    %% @todo This doesn't seem to be executed.
    _ = file:write(IoDevice, lz4f:compress_frame(Buffer)),
    exit(Reason).

system_code_change(Misc, _, _, _) ->
    {ok, Misc}.
