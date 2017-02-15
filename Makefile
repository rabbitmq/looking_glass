PROJECT = looking_glass
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

LOCAL_DEPS = runtime_tools

C_SRC_OUTPUT ?= $(CURDIR)/priv/lg_tracer

include erlang.mk
