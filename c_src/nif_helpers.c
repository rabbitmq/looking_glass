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

#include "nif_helpers.h"
#include <sys/queue.h>
#include <stdarg.h>

extern ERL_NIF_TERM atom_ok;
extern ERL_NIF_TERM atom__nif_thread_ret_;

typedef struct nif_thread_message {
	TAILQ_ENTRY(nif_thread_message) next_entry;

	ErlNifPid* from_pid;
	void* function;
	nif_thread_arg* args;
} nif_thread_message;

typedef TAILQ_HEAD(nif_thread_mailbox, nif_thread_message) nif_thread_mailbox;

typedef struct {
	ErlNifTid tid;
	ErlNifMutex* lock;
	ErlNifCond* cond;
	nif_thread_mailbox* mailbox;
} nif_thread_state;

// Message.

nif_thread_message* nif_thread_message_alloc(void* f, nif_thread_arg* args, ErlNifPid* pid)
{
	nif_thread_message* msg = (nif_thread_message*)enif_alloc(sizeof(nif_thread_message));

	msg->from_pid = pid;
	msg->function = f;
	msg->args = args;

	return msg;
}

void nif_thread_message_free(nif_thread_message* msg)
{
	enif_free(msg->from_pid);
	enif_free(msg->args);
	enif_free(msg);
}

// Calls and casts.

ERL_NIF_TERM nif_thread_send(nif_thread_state* st, nif_thread_message* msg)
{
	enif_mutex_lock(st->lock);

	TAILQ_INSERT_TAIL(st->mailbox, msg, next_entry);

	enif_cond_signal(st->cond);
	enif_mutex_unlock(st->lock);

	return atom_ok;
}

ERL_NIF_TERM nif_thread_cast(ErlNifEnv* env, void (*f)(nif_thread_arg*), int a, ...)
{
	va_list ap;
	int i;

	nif_thread_arg* args = (nif_thread_arg*)enif_alloc(a * sizeof(nif_thread_arg));

	va_start(ap, a);
	for (i = 0; i < a; i++)
		args[i] = va_arg(ap, void*);
	va_end(ap);

	nif_thread_message* msg = nif_thread_message_alloc(f, args, NULL);

	return nif_thread_send((nif_thread_state*)enif_priv_data(env), msg);
}

ERL_NIF_TERM nif_thread_call(ErlNifEnv* env, ERL_NIF_TERM (*f)(ErlNifEnv*, nif_thread_arg*), int a, ...)
{
	va_list ap;
	int i;

	nif_thread_arg* args = (nif_thread_arg*)enif_alloc(a * sizeof(nif_thread_arg));

	va_start(ap, a);
	for (i = 0; i < a; i++)
		args[i] = va_arg(ap, void*);
	va_end(ap);

	ErlNifPid* pid = (ErlNifPid*)enif_alloc(sizeof(ErlNifPid));
	nif_thread_message* msg = nif_thread_message_alloc((void*)f, args, enif_self(env, pid));

	return nif_thread_send((nif_thread_state*)enif_priv_data(env), msg);
}

// Main thread loop.

int nif_thread_receive(nif_thread_state* st, nif_thread_message** msg)
{
	enif_mutex_lock(st->lock);

	while (TAILQ_EMPTY(st->mailbox))
		enif_cond_wait(st->cond, st->lock);

	*msg = TAILQ_FIRST(st->mailbox);
	TAILQ_REMOVE(st->mailbox, TAILQ_FIRST(st->mailbox), next_entry);

	enif_mutex_unlock(st->lock);

	if ((*msg)->function == NULL)
		return 0;

	return 1;
}

void nif_thread_handle(ErlNifEnv* env, nif_thread_state* st, nif_thread_message* msg)
{
	if (msg->from_pid == NULL) {
		void (*cast)(nif_thread_arg*) = msg->function;
		cast(msg->args);
	} else {
		ERL_NIF_TERM (*call)(ErlNifEnv*, nif_thread_arg*) = msg->function;
		ERL_NIF_TERM ret = call(env, msg->args);

		enif_send(NULL, msg->from_pid, env,
			enif_make_tuple2(env, atom__nif_thread_ret_, ret));

		enif_clear_env(env);
	}

	nif_thread_message_free(msg);
}

void* nif_main_thread(void* obj)
{
	ErlNifEnv* env = enif_alloc_env();
	nif_thread_state* st = (nif_thread_state*)obj;
	nif_thread_message* msg;

	while (nif_thread_receive(st, &msg))
		nif_thread_handle(env, st, msg);

	enif_free_env(env);

	return NULL;
}

// Main thread creation/destruction.

void* nif_create_main_thread(char* name)
{
	nif_thread_state* st = (nif_thread_state*)enif_alloc(sizeof(nif_thread_state));

	st->lock = enif_mutex_create("esdl2_lock");
	st->cond = enif_cond_create("esdl2_cond");
	st->mailbox = (nif_thread_mailbox*)enif_alloc(sizeof(nif_thread_mailbox));
	TAILQ_INIT(st->mailbox);

#if defined(__APPLE__) && defined(__MACH__)
	// On OSX, SDL2 must run in the main thread, otherwise some operations
	// will not work properly. For example, input events would not be received.
	erl_drv_steal_main_thread(name, &(st->tid), nif_main_thread, st, NULL);
#else
	enif_thread_create(name, &(st->tid), nif_main_thread, st, NULL);
#endif

	return (void*)st;
}

void nif_destroy_main_thread(void* void_st)
{
	nif_thread_state* st = (nif_thread_state*)void_st;
	nif_thread_message* msg = nif_thread_message_alloc(NULL, NULL, NULL);

	nif_thread_send(st, msg);
	enif_thread_join(st->tid, NULL);

	enif_cond_destroy(st->cond);
	enif_mutex_destroy(st->lock);
	enif_free(st->mailbox);
	enif_free(st);
}
