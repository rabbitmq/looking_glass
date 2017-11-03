{application, 'looking_glass', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['lg','lg_callgrind','lg_file_reader','lg_file_tracer','lg_flame','lg_messages','lg_messages_seqdiag','lg_rabbit_hole','lg_raw_console_tracer','lg_socket_client','lg_socket_tracer','lg_tracer','lg_tracer_pool','looking_glass_app','looking_glass_sup']},
	{registered, [looking_glass_sup]},
	{applications, [kernel,stdlib,runtime_tools,lz4]},
	{mod, {looking_glass_app, []}},
	{env, []}
]}.