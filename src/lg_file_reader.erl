-module(lg_file_reader).

-export([fold/3]).
-export([foreach/2]).

-export([open/1]).
-export([read_event/1]).
-export([close/1]).

%% High level API.

fold(Fun, Acc, Filename) ->
    {ok, IoDevice} = open(Filename),
    Ret = fold1(IoDevice, Fun, Acc),
    ok = close(IoDevice),
    Ret.

fold1(IoDevice, Fun, Acc0) ->
    case read_event(IoDevice) of
        {ok, Event} ->
            Acc = Fun(Event, Acc0),
            fold1(IoDevice, Fun, Acc);
        eof ->
            {ok, Acc0};
        Error = {error, _, _} ->
            Error
    end.

foreach(Fun, Filename) ->
    {ok, IoDevice} = open(Filename),
    Ret = foreach1(IoDevice, Fun),
    ok = close(IoDevice),
    Ret.

foreach1(IoDevice, Fun) ->
    case read_event(IoDevice) of
        {ok, Event} ->
            _ = Fun(Event),
            foreach1(IoDevice, Fun);
        eof ->
            ok;
        Error = {error, _, _} ->
            Error
    end.

%% Low level API.

open(Filename) ->
    file:open(Filename, [read, binary, compressed]).

read_event(IoDevice) ->
    case file:read(IoDevice, 2) of
        {ok, <<Size:16>>} ->
            read_event_body(IoDevice, Size);
        eof ->
            eof;
        {error, Reason} ->
            {error, Reason,
                'An error occurred while trying to read the size of an event in the file.'}
    end.

read_event_body(IoDevice, Size) ->
    case file:read(IoDevice, Size) of
        {ok, Bin} ->
            convert_event_body(Bin);
        {error, Reason} ->
            {error, Reason,
                'An error occured while trying to read an event from the file.'}
    end.

convert_event_body(Bin) ->
    try binary_to_term(Bin) of
        Term ->
            {ok, Term}
    catch Class:Reason ->
        {error, {crash, Class, Reason},
            'The binary form of an event could not be decoded to an Erlang term.'}
    end.

close(IoDevice) ->
    file:close(IoDevice).
