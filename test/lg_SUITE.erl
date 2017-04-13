-module(lg_SUITE).
-compile(export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).

%% ct.

all() ->
    [{group, all}].

%% We cannot run the tests in parallel or they would
%% interfere with each other.
groups() ->
    [{all, [], ct_helper:all(?MODULE)}].

%% Tests.

app(Config) ->
    doc("Trace a specific application."),
    lg:trace({app, stdlib}, lg_file_tracer, config(priv_dir, Config) ++ "/app.lz4"),
    lists:seq(1,10),
    lg:stop(),
    do_ensure_decompress(config(priv_dir, Config) ++ "/app.lz4").

callback(Config) ->
    doc("Trace using patterns from a callback function."),
    lg:trace({callback, ?MODULE, do_callback}, lg_file_tracer,
        config(priv_dir, Config) ++ "/callback.lz4"),
    lists:seq(1,10),
    lg:stop(),
    do_ensure_decompress(config(priv_dir, Config) ++ "/callback.lz4").

do_callback() ->
    [{scope, [self()]}, lists].

file_tracer(Config) ->
    doc("Save events to a files on disk."),
    lg:trace(lists, lg_file_tracer, config(priv_dir, Config) ++ "/file_tracer.lz4"),
    lists:seq(1,10),
    lg:stop(),
    do_ensure_decompress(config(priv_dir, Config) ++ "/file_tracer.lz4").

mod(Config) ->
    doc("Trace a specific module."),
    lg:trace(lists, lg_file_tracer, config(priv_dir, Config) ++ "/mod.lz4"),
    lists:seq(1,10),
    lg:stop(),
    do_ensure_decompress(config(priv_dir, Config) ++ "/mod.lz4").

profile_mode(Config) ->
    doc("Trace a specific module in profile mode."),
    lg:trace(lists, lg_file_tracer, config(priv_dir, Config) ++ "/profile_mode.lz4",
        #{mode => profile}),
    lists:seq(1,10),
    lg:stop(),
    do_ensure_decompress(config(priv_dir, Config) ++ "/profile_mode.lz4").

raw_console_tracer(_) ->
    doc("Print raw events to the console."),
    ct:print("Start tracing to the console."),
    %% @todo It seems the order matters when starting. Should it?
    lg:trace([{scope, [self()]}, lists]),
    lists:seq(1,10),
    lg:stop(),
    ct:print("Stop tracing to the console.").

running_true(Config) ->
    doc("Trace a specific module in with running option enabled."),
    lg:trace(lists, lg_file_tracer, config(priv_dir, Config) ++ "/running_true.lz4",
        #{running => true}),
    lists:seq(1,10),
    lg:stop(),
    do_ensure_decompress(config(priv_dir, Config) ++ "/running_true.lz4").

stop_while_trace_is_running(Config) ->
    doc("Stop tracing while events are still coming in."),
    Self = self(),
    Pid = spawn_link(fun() -> Self ! {self(), continue}, lists:seq(1,10000000) end),
    lg:trace([{scope, [Pid]}, lists], lg_file_tracer,
        config(priv_dir, Config) ++ "/stop_while_trace_is_running.lz4"),
    receive {Pid, continue} -> ok after 100 -> error(timeout) end,
    lg:stop(),
    do_ensure_decompress(config(priv_dir, Config) ++ "/stop_while_trace_is_running.lz4").

%% Internal.

do_ensure_decompress(Prefix) ->
    %% Ensure the files can be decompressed.
    Sizes = [begin
        Filename = Prefix ++ "." ++ integer_to_list(N),
        {ok, File} = file:read_file(Filename),
        _ = lz4f:decompress(File),
        filelib:file_size(Filename)
    end || N <- lists:seq(1, erlang:system_info(schedulers))],
    %% We also need to make sure there is actual data in the files,
    %% as lz4f:decompress will succeed when provided with no data.
    true = 0 < lists:sum(Sizes),
    ok.
