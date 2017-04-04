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
    ctx :: lz4f:cctx(),
    events_per_frame = 10000 :: pos_integer(),
    events_this_frame = 0 :: pos_integer()
}).

start_link(Nth, Filename0) ->
    Filename = filename:flatten([Filename0, ".", integer_to_list(Nth)]),
    Pid = proc_lib:spawn_link(?MODULE, init, [self(), Filename]),
    {ok, Pid}.

init(Parent, Filename) ->
    %% No need to close the file, it'll be closed when the process exits.
    {ok, IoDevice} = file:open(Filename, [write, delayed_write]),
    Ctx = lz4f:create_compression_context(),
    ok = file:write(IoDevice, lz4f:compress_begin(Ctx)),
    loop(#state{parent=Parent, filename=Filename, io_device=IoDevice,
        ctx=Ctx}).

loop(State=#state{parent=Parent, io_device=IoDevice, ctx=Ctx,
        events_per_frame=MaxEvents, events_this_frame=NumEvents}) ->
    receive
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [], State);
        Msg ->
            Bin0 = term_to_binary(Msg),
            BinSize = byte_size(Bin0),
            Bin = lz4f:compress_update(Ctx, <<BinSize:16, Bin0/binary>>),
            Suffix = if
                MaxEvents =:= NumEvents + 1 ->
                    [lz4f:compress_end(Ctx), lz4f:compress_begin(Ctx)];
                true ->
                    []
            end,
            %% @todo This gets badmatch if we q().
            ok = file:write(IoDevice, [Bin|Suffix]),
            loop(State)
    end.

system_continue(_, _, State) ->
    loop(State).

-spec system_terminate(any(), _, _, #state{}) -> no_return().
system_terminate(Reason, _, _, #state{io_device=IoDevice, ctx=Ctx}) ->
    %% @todo This doesn't seem to be executed.
    file:write(IoDevice, lz4f:compress_end(Ctx)),
    exit(Reason).

system_code_change(Misc, _, _, _) ->
    {ok, Misc}.
