// Copyright (c) 2017-Present Pivotal Software, Inc.  All rights reserved.
//
// This package, Looking Glass, is double-licensed under the Mozilla
// Public License 1.1 ("MPL") and the Apache License version 2
// ("ASL"). For the MPL, please see LICENSE-MPL-RabbitMQ. For the ASL,
// please see LICENSE-APACHE2.
//
// This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND,
// either express or implied. See the LICENSE file for specific language governing
// rights and limitations of this software.
//
// If you have any questions regarding licensing, please contact us at
// info@rabbitmq.com.

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
    F(enabled_call, 3) \
    F(enabled_procs, 3) \
    F(enabled_running_procs, 3) \
    F(enabled_send, 3) \
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

NIF_FUNCTION(enabled)
{
    ERL_NIF_TERM tracers, value;
    ErlNifPid tracer;

    // @todo We can go one step further by having the one pid
    // in its own value in the map, skipping a get_map_value step.

    // This function will only be called for trace_status.
    // We can take a few shortcuts knowing this.

    // Disable the trace when the tracers option is missing.
    if (!enif_get_map_value(env, argv[1], atom_tracers, &tracers))
        return atom_remove;

    // Because the tracers supervisor is a one_for_all, we only need
    // to check one of the tracer processes to confirm all are alive.

    // We know for a fact that this key exists because
    // there's at least one tracer process.
    enif_get_map_value(env, tracers, enif_make_int(env, 0), &value);

    // Disable the trace when one of the tracers is not a local process.
    if (!enif_get_local_pid(env, value, &tracer))
        return atom_remove;

    // Disable the trace when one of the tracers is not alive.
    if (!enif_is_process_alive(env, &tracer))
        return atom_remove;

    return atom_discard;
}

NIF_FUNCTION(enabled_call)
{
    // We always want both call and return_to.
    return atom_trace;
}

NIF_FUNCTION(enabled_procs)
{
    ERL_NIF_TERM mode;

    // We only want the spawn and exit events when 'profile' mode
    // is enabled. Technically we only care about exits for callgrind,
    // but spawn is cheap to keep and useful for message profilers.
    if (enif_get_map_value(env, argv[1], atom_mode, &mode)
        && enif_is_identical(atom_profile, mode)
        && !(enif_is_identical(atom_spawn, argv[0])
            || enif_is_identical(atom_exit, argv[0]))) {
        return atom_discard;
    }

    return atom_trace;
}

NIF_FUNCTION(enabled_running_procs)
{
    // We always want both in and out.
    return atom_trace;
}

NIF_FUNCTION(enabled_send)
{
    // We always want both send and send_to_non_existing_process.
    return atom_trace;
}

// trace(TraceTag, TracerState, Tracee, TraceTerm, Opts)

NIF_FUNCTION(trace)
{
    ERL_NIF_TERM tracers, head, ts, extra, mspec, msg;
    ErlNifPid tracer;
    unsigned int nth;
    size_t len;
    int has_extra, has_mspec;

    if (!enif_get_map_value(env, argv[1], atom_tracers, &tracers))
        return atom_ok;

    // We know for a fact that the argument is a map. And if not,
    // no problem because we will return when trying to get a value from it.
    enif_get_map_size(env, tracers, &len);

#if (ERL_NIF_MAJOR_VERSION >= 2) && (ERL_NIF_MINOR_VERSION >= 12)
    nth = enif_hash(ERL_NIF_INTERNAL_HASH, argv[2], 0) % len;
#else
    // Select the correct tracer for this process.
    //
    // The pid value is detailed in:
    //     5b6dd0e84cf0f1dc19ddd05f86cf04b2695d8a9e/erts/emulator/beam/erl_term.h#L498
    //
    // As can be seen there, the first four bits of the pid value
    // are always the same. We therefore shift them out.

    ErlNifPid tracee;

    if (!enif_get_local_pid(env, argv[2], &tracee))
        return atom_ok;

    nth = (tracee.pid >> 4) % len;
#endif

    if (!enif_get_map_value(env, tracers, enif_make_int(env, nth), &head))
        return atom_ok;

    if (!enif_get_local_pid(env, head, &tracer))
        return atom_ok;

    // Everything good. Generate a timestamp to include in the message.

    ts = enif_make_int64(env, enif_monotonic_time(ERL_NIF_USEC));

    // Build the message. There can be two different messages
    // depending on whether the extra option was set:
    //
    // - {Tag, Tracee, Ts, Term}
    // - {Tag, Tracee, Ts, Term, Extra}
    //
    // On top of that when match specs are enabled we may have
    // one additional term at the end of the tuple containing
    // the result of the match spec function.
    //
    // - {Tag, Tracee, Ts, Term, Result}
    // - {Tag, Tracee, Ts, Term, Extra, Result}

    has_extra = enif_get_map_value(env, argv[4], atom_extra, &extra);
    has_mspec = enif_get_map_value(env, argv[4], atom_match_spec_result, &mspec);

    if (has_extra && has_mspec)
        msg = enif_make_tuple6(env, argv[0], argv[2], ts, argv[3], extra, mspec);
    else if (has_extra)
        msg = enif_make_tuple5(env, argv[0], argv[2], ts, argv[3], extra);
    else if (has_mspec)
        msg = enif_make_tuple5(env, argv[0], argv[2], ts, argv[3], mspec);
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
