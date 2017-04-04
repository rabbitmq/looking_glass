PROJECT = looking_glass
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = lz4
dep_lz4 = git https://github.com/essen/lz4-erlang master

LOCAL_DEPS = runtime_tools

C_SRC_OUTPUT ?= $(CURDIR)/priv/lg_tracer

include erlang.mk
