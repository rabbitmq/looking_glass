-module(lg).

-export([trace/1]).
-export([trace/2]).
-export([stop/0]).
-export([display_trace/1]).

%% @todo {profile, M::atom(), F::atom()}
-type pattern() :: module() | {app, atom()}.
-type input() :: pattern() | [pattern()].
%% @todo {lg_file, file:file_name()} for an optimized file format.
%% @todo {ip, IP, Port} for client/server.
-type output() :: console | raw_console | {dbg_file, file:file_name()}.

-spec trace(input()) -> ok.
trace(Input) ->
    trace(Input, console).

-spec trace(input(), output()) -> ok.
trace(Input, Output) when is_list(Input) ->
    do_trace(Input, Output);
trace(Input, Output) ->
    trace([Input], Output).

stop() ->
    _ = case ets:lookup(looking_glass, io_device) of
        [] -> ok;
        [{_, IoDevice}] -> file:close(IoDevice)
    end,
    dbg:stop_clear().

do_trace(Input, Output) ->
    %% @todo Remove eventually?
    _ = application:start(looking_glass),
    {ok, _} = start_tracer(Output),
    trace_patterns(Input),
    %% We currently enable the following trace flags:
    %% - call: function calls
    %% - procs: process exit events; plus others we ignore
    %% - timestamp: events include the current timestamp
    %% - arity: function calls only include the arity, not arguments
    %% - return_to: return from functions
    %%
    %% @todo We will need to add the 'running' trace flag
    %% to get events when the process gets scheduled in and out.
    %% We can then add the following infos to the callgrind file:
    %% - active execution time (without the wait times)
    %% - wait time
    %% - number of times the process got scheduled out
    %%   (can be useful to detect anomalies, for example
    %%   a process sending a message to a busy process can
    %%   get scheduled out)
    %%
    %% @todo It might be useful to count the number of sends
    %% or receives a function does.
    {ok, _} = dbg:p(all, [call, procs, timestamp, arity, return_to]),
    ok.

start_tracer(console) ->
    dbg:tracer();
start_tracer(raw_console) ->
    dbg:tracer(process, {fun raw_console_tracer/2, undefined});
start_tracer({dbg_file, Filename}) ->
    %% The dbg binary format compresses remarkably well.
    %% We therefore unconditionally enable compression.
    {ok, IoDevice} = file:open(Filename, [write, delayed_write, compressed]),
    _ = ets:insert(looking_glass, {io_device, IoDevice}),
    dbg:tracer(process, {fun dbg_file_tracer/2, IoDevice}).

%% Print messages unformatted. Useful for debugging purposes.
raw_console_tracer(Msg, State) ->
    erlang:display(Msg),
    State.

%% Save to file following the Erlang/OTP dbg binary trace format.
dbg_file_tracer(Msg, IoDevice) ->
    Bin = term_to_binary(Msg),
    BinSize = byte_size(Bin),
    ok = file:write(IoDevice, [<<0, BinSize:32>>, Bin]),
    IoDevice.

trace_patterns(Input) ->
    lists:foreach(fun trace_pattern/1, Input).

trace_pattern({app, App}) when is_atom(App) ->
    {ok, Mods} = application:get_key(App, modules),
    trace_patterns(Mods);
trace_pattern(Mod) when is_atom(Mod) ->
    dbg:tpl(Mod, []).

%% @todo Move this to a separate module, or remove entirely.
-spec display_trace(file:file_name()) -> ok.
display_trace(Filename) ->
    {ok, IoDevice} = file:open(Filename, [read, binary, compressed]),
    ok = read_trace(IoDevice, fun erlang:display/1),
    ok = file:close(IoDevice).

read_trace(IoDevice, Fun) ->
    case file:read(IoDevice, 5) of
        {ok, <<0, BinSize:32>>} ->
            case file:read(IoDevice, BinSize) of
                {ok, Bin} ->
                    Fun(binary_to_term(Bin)),
                    read_trace(IoDevice, Fun);
                What ->
                    What
            end;
        eof ->
            ok;
        Nope ->
            Nope
    end.
