-module(lg_file_reader).

-export([fold/3]).
-export([foreach/2]).

-export([open/1]).
-export([read_event/1]).
-export([close/1]).

-record(state, {
    io_device :: file:io_device(),
    ctx :: lz4f:dctx(),
    buffer = <<>> :: binary()
}).

%% High level API.

fold(Fun, Acc, Filename) ->
    {ok, IoDevice} = open(Filename),
    Ctx = lz4f:create_decompression_context(),
    Ret = fold1(#state{io_device=IoDevice, ctx=Ctx}, Fun, Acc),
    ok = close(IoDevice),
    Ret.

fold1(State0, Fun, Acc0) ->
    case read_event(State0) of
        {ok, Event, State} ->
            Acc = Fun(Event, Acc0),
            fold1(State, Fun, Acc);
        eof ->
            {ok, Acc0};
        Error = {error, _, _} ->
            Error
    end.

foreach(Fun, Filename) ->
    {ok, IoDevice} = open(Filename),
    Ctx = lz4f:create_decompression_context(),
    Ret = foreach1(#state{io_device=IoDevice, ctx=Ctx}, Fun),
    ok = close(IoDevice),
    Ret.

foreach1(State0, Fun) ->
    case read_event(State0) of
        {ok, Event, State} ->
            _ = Fun(Event),
            foreach1(State, Fun);
        eof ->
            ok;
        Error = {error, _, _} ->
            Error
    end.

%% Low level API.

open(Filename) ->
    file:open(Filename, [read, binary]).

read_event(State=#state{buffer=Buffer}) ->
    case Buffer of
        <<Size:16, Bin:Size/binary, Rest/bits>> ->
            convert_event_body(State#state{buffer=Rest}, Bin);
        _ ->
            read_file(State)
    end.

read_file(State=#state{io_device=IoDevice, ctx=Ctx, buffer=Buffer}) ->
    case file:read(IoDevice, 1000) of
        {ok, Data0} ->
            Data = iolist_to_binary(lz4f:decompress(Ctx, Data0)),
            read_event(State#state{buffer= <<Buffer/binary, Data/binary>>});
        eof ->
            eof;
        {error, Reason} ->
            {error, Reason,
                'An error occurred while trying to read from the file.'}
    end.

convert_event_body(State, Bin) ->
    try binary_to_term(Bin) of
        Term ->
            {ok, Term, State}
    catch Class:Reason ->
        {error, {crash, Class, Reason},
            'The binary form of an event could not be decoded to an Erlang term.'}
    end.

close(IoDevice) ->
    file:close(IoDevice).
