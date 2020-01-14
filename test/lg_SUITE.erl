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

callgrind_running(Config) ->
    doc("Save events to files on disk then build callgrind files."),
    PrivDir = config(priv_dir, Config),
    lg:trace([{scope, [self()]}, ?MODULE, {app, stdlib}], lg_file_tracer,
        PrivDir ++ "/callgrind_running.lz4",
        #{mode => profile, running => true}),
    do_callgrind_running(),
    lg:stop(),
    lg_callgrind:profile_many(
        PrivDir ++ "/callgrind_running.lz4.*",
        PrivDir ++ "/callgrind_running.out",
        #{running => true}),
    %% For debugging purposes, print the contents of the callgrind.out files.
    %% Uncomment for easier debugging, otherwise look into the files directly.
%    _ = [begin
%        {ok, File} = file:read_file(PrivDir ++ "/callgrind_running.out." ++ integer_to_list(N)),
%        io:format(user, "# callgrind_running.out.~p~n~s", [N, File]),
%        lg_file_reader:foreach(fun(E) -> io:format(user, "~p~n", [E]) end,
%            PrivDir ++ "/callgrind_running.lz4." ++ integer_to_list(N))
%    end || N <- lists:seq(1, erlang:system_info(schedulers))],
    ok.

do_callgrind_running() ->
    timer:sleep(1000),
    Ref = make_ref(),
    erlang:send_after(1000, self(), {go, Ref}),
    lists:seq(1,100),
    do_callgrind_running_receive(Ref),
    lists:seq(1,100),
    ok.

do_callgrind_running_receive(Ref) ->
    receive
        {go, Ref} ->
            ok
    end.

file_tracer(Config) ->
    doc("Save events to files on disk."),
    lg:trace(lists, lg_file_tracer, config(priv_dir, Config) ++ "/file_tracer.lz4"),
    lists:seq(1,10),
    lg:stop(),
    do_ensure_decompress(config(priv_dir, Config) ++ "/file_tracer.lz4").

file_tracer_rotation(Config) ->
    doc("Save events to files on disk; rotate the files if they get too big."),
    Prefix = config(priv_dir, Config) ++ "/file_tracer.lz4",
    lg:trace(lists, lg_file_tracer, #{
        filename_prefix => Prefix,
        max_size => 100, %% Intentionally low.
        events_per_frame => 10 %% Needed to trigger the rotation, default is too high.
    }),
    lists:seq(1,1000),
    lg:stop(),
    %% We should have one or more rotated files.
    Result = [begin
        Filename = Prefix ++ "." ++ integer_to_list(N) ++ ".bak",
        filelib:is_file(Filename)
    end || N <- lists:seq(1, erlang:system_info(schedulers))],
    true = lists:member(true, lists:usort(Result)),
    ok.

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
    doc("Trace a specific module with running option enabled."),
    lg:trace(lists, lg_file_tracer, config(priv_dir, Config) ++ "/running_true.lz4",
        #{running => true}),
    lists:seq(1,10),
    lg:stop(),
    do_ensure_decompress(config(priv_dir, Config) ++ "/running_true.lz4").

send_true(Config) ->
    doc("Trace a specific module with send option enabled."),
    lg:trace(lists, lg_file_tracer, config(priv_dir, Config) ++ "/send_true.lz4",
        #{send => true}),
    Self = self(),
    %% Send a message to and from an existing process.
    Pid = spawn(fun() ->
        receive {msg_from, Self} ->
            Self ! {msg_from, self()}
        end
    end),
    Pid ! {msg_from, Self},
    receive {msg_from, Pid} -> ok end,
    %% Also send a message to a non existing process.
    DeadPid = spawn(fun() -> ok end),
    receive after 100 -> ok end,
    DeadPid ! {msg_from, Self},
    lg:stop(),
    do_ensure_decompress(config(priv_dir, Config) ++ "/send_true.lz4").

socket_tracer(_) ->
    doc("Send events to a socket."),
    Port = 61234,
    lg:trace(lists, lg_socket_tracer, Port, #{pool_size => 1}),
    {ok, Socket} = gen_tcp:connect("localhost", Port,
        [binary, {packet, 2}, {active, true}]),
    lists:seq(1,10),
    lg:stop(),
    do_socket_tracer_recv(Socket).

socket_tracer_client(Config) ->
    doc("Send events to a socket client."),
    Port = 61234,
    lg:trace(lists, lg_socket_tracer, Port, #{pool_size => 1}),
    BaseFilename = config(priv_dir, Config) ++ "/socket_tracer_client.lz4",
    {ok, Pid} = lg_socket_client:start_link(Port, BaseFilename),
    timer:sleep(1000),
    lists:seq(1,10),
    lg:stop(),
    lg_socket_client:stop(Pid),
    {ok, File} = file:read_file(BaseFilename ++ ".0"),
    _ = lz4f:decompress(File),
    true = filelib:file_size(BaseFilename ++ ".0") > 0,
    ok.

socket_tracer_many(_) ->
    doc("Send events to many sockets."),
    Port = 61234,
    lg:trace(lists, lg_socket_tracer, Port, #{pool_size => 5}),
    {ok, _} = gen_tcp:connect("localhost", Port, []),
    {ok, _} = gen_tcp:connect("localhost", Port + 1, []),
    {ok, _} = gen_tcp:connect("localhost", Port + 2, []),
    {ok, _} = gen_tcp:connect("localhost", Port + 3, []),
    {ok, _} = gen_tcp:connect("localhost", Port + 4, []),
    {error, _} = gen_tcp:connect("localhost", Port + 5, []),
    lg:stop().

socket_tracer_reconnect(_) ->
    doc("Confirm we can reconnect to the tracer."),
    Port = 61234,
    lg:trace(lists, lg_socket_tracer, Port, #{pool_size => 1}),
    {ok, Socket0} = gen_tcp:connect("localhost", Port,
        [binary, {packet, 2}, {active, true}]),
    ok = gen_tcp:close(Socket0),
    {ok, Socket} = gen_tcp:connect("localhost", Port,
        [binary, {packet, 2}, {active, true}]),
    lists:seq(1,10),
    lg:stop(),
    do_socket_tracer_recv(Socket).

do_socket_tracer_recv(Socket) ->
    receive
        {tcp, Socket, Data} ->
            Term = binary_to_term(Data),
            true = is_tuple(Term),
            do_socket_tracer_recv(Socket);
        {tcp_closed, Socket} ->
            ok
    after 1000 ->
        error(timeout)
    end.

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
