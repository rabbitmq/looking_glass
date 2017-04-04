// @todo Fix this copyright, it's incorrect.
// Copyright (c) 2014-2015, Loïc Hoguin <essen@ninenines.eu>
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice appear in all copies.
//
// THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
// WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
// MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
// ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
// WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
// ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
// OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

#define NIF_FUNCTION_NAME(f) lg_ ## f

#include "nif_helpers.h"

// List of atoms used by this NIF.
//
// @todo We don't use threads so perhaps we should make nif_helpers
// better by splitting concerns into threads/not and have nif_helpers
// decide when to create the _nif_thread_ret atom or not.

#define NIF_ATOMS(A) \
    A(_nif_thread_ret_) \
    A(call) \
    A(closed) \
    A(cpu_timestamp) \
    A(discard) \
    A(exception_from) \
    A(exit) \
    A(extra) \
    A(gc_major_end) \
    A(gc_major_start) \
    A(gc_minor_end) \
    A(gc_minor_start) \
    A(getting_linked) \
    A(getting_unlinked) \
    A(in) \
    A(in_exiting) \
    A(link) \
    A(match_spec_result) \
    A(mode) \
    A(monotonic) \
    A(ok) \
    A(open) \
    A(out) \
    A(out_exited) \
    A(out_exiting) \
    A(percent) \
    A(profile) \
    A(receive) \
    A(register) \
    A(remove) \
    A(return_from) \
    A(return_to) \
    A(scheduler_id) \
    A(send) \
    A(send_to_non_existing_process) \
    A(spawn) \
    A(spawned) \
    A(strict_monotonic) \
    A(timestamp) \
    A(trace) \
    A(trace_status) \
    A(tracers) \
    A(unlink) \
    A(unregister)

NIF_ATOMS(NIF_ATOM_DECL)

// List of functions defined in this NIF.

#define NIF_FUNCTIONS(F) \
    F(enabled, 3) \
    F(trace, 5)

NIF_FUNCTIONS(NIF_FUNCTION_H_DECL)

int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
void unload(ErlNifEnv* env, void* priv_data);

int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    NIF_ATOMS(NIF_ATOM_INIT)

    return 0;
}

int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    *priv_data = *old_priv_data;

    return 0;
}

void unload(ErlNifEnv* env, void* priv_data)
{
}

// enabled(TraceTag, TracerState, Tracee)

// TracerState :: #{
//   tracers := [pid()]
// }

#include <stdio.h>

NIF_FUNCTION(enabled)
{
    ERL_NIF_TERM mode, tracers, head;
    ErlNifPid tracer;

    int is_trace_status = enif_is_identical(atom_trace_status, argv[0]);
    ERL_NIF_TERM on_error = is_trace_status ? atom_remove : atom_discard;

    // Disable the trace when the tracers option is missing.
    if (!enif_get_map_value(env, argv[1], atom_tracers, &tracers))
        return on_error;

    // Disable the trace when the tracers option is not a list.
    if (!enif_is_list(env, tracers))
        return on_error;

    while (enif_get_list_cell(env, tracers, &head, &tracers)) {
        // Don't generate trace events for tracers.
        if (enif_is_identical(argv[2], head))
            return atom_discard;

        // Disable the trace when one of the tracers is not a local process.
        if (!enif_get_local_pid(env, head, &tracer))
            return on_error;

        // Disable the trace when one of the tracers is not alive.
        if (!enif_is_process_alive(env, &tracer))
            return on_error;
    }

    // @todo Discard trace events for a percent of processes.

    // Discard events we don't need when the tracer mode is 'profile'.
    // The default is to leave all events that were enabled by Erlang.
    if (enif_get_map_value(env, argv[1], atom_mode, &mode)) {
        if (enif_is_identical(atom_profile, mode)) {
            if (!(
                enif_is_identical(atom_call, argv[0]) ||
                enif_is_identical(atom_return_to, argv[0]) ||
                enif_is_identical(atom_exit, argv[0]) ||
                enif_is_identical(atom_in, argv[0]) ||
                enif_is_identical(atom_out, argv[0]) ||
                enif_is_identical(atom_trace_status, argv[0])))
                return on_error;
        }
    }

    return atom_trace;
}

// trace(TraceTag, TracerState, Tracee, TraceTerm, Opts)

NIF_FUNCTION(trace)
{
    ERL_NIF_TERM tracers, head, now, ts, extra, msg;
    const ERL_NIF_TERM *array;
    ErlNifPid tracer, tracee;
    unsigned int len, nth, i, megasec, sec, microsec;
    ErlNifUInt64 ts64;
    int arity;

    if (!enif_get_local_pid(env, argv[2], &tracee))
        return atom_ok;

    if (!enif_get_map_value(env, argv[1], atom_tracers, &tracers))
        return atom_ok;

    if (!enif_get_list_length(env, tracers, &len))
        return atom_ok;

    // Select the correct tracer for this process.
    //
    // The pid value is detailed in:
    //     5b6dd0e84cf0f1dc19ddd05f86cf04b2695d8a9e/erts/emulator/beam/erl_term.h#L498
    //
    // As can be seen there, the first four bits of the pid value
    // are always the same. We therefore shift them out.

    nth = (tracee.pid >> 4) % len;

    for (i = 0; i <= nth; i++) {
        if (!enif_get_list_cell(env, tracers, &head, &tracers))
            return atom_ok;
    }

    if (!enif_get_local_pid(env, head, &tracer))
        return atom_ok;

    // @todo Do we ever reach this?
    if (!enif_is_process_alive(env, &tracer))
        printf("dead\n");

    // Everything good. Generate a timestamp to include in the message.
    //
    // @todo While this function is deprecated, the erl_tracer_nif.c
    // coming with OTP currently uses it when the 'timestamp' option is used.

    now = enif_now_time(env);

    if (!enif_get_tuple(env, now, &arity, &array))
        return atom_ok;

    if (!enif_get_uint(env, array[0], &megasec))
        return atom_ok;

    if (!enif_get_uint(env, array[1], &sec))
        return atom_ok;

    if (!enif_get_uint(env, array[2], &microsec))
        return atom_ok;

    ts64 = megasec;
    ts64 *= 1000000;
    ts64 += sec;
    ts64 *= 1000000;
    ts64 += microsec;

    ts = enif_make_uint64(env, ts64);

    // Build the message. There can be two different messages
    // depending on whether the extra option was set:
    //
    // - {Tag, Tracee, Ts, Term}
    // - {Tag, Tracee, Ts, Term, Extra}

    if (enif_get_map_value(env, argv[4], atom_extra, &extra))
        msg = enif_make_tuple5(env, argv[0], argv[2], ts, argv[3], extra);
    else
        msg = enif_make_tuple4(env, argv[0], argv[2], ts, argv[3]);

    // Send the message to the selected tracer.

    enif_send(env, &tracer, NULL, msg);

    return atom_ok;
}

static ErlNifFunc nif_funcs[] = {
    NIF_FUNCTIONS(NIF_FUNCTION_ARRAY)
};

ERL_NIF_INIT(lg_tracer, nif_funcs, load, NULL, upgrade, unload)
