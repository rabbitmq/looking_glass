PROJECT = looking_glass
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = lz4
dep_lz4 = git https://github.com/rabbitmq/lz4-erlang master

BUILD_DEPS = nif_helpers
dep_nif_helpers = git https://github.com/ninenines/nif_helpers ead6adc15fca3c314351523080d2ccb1956d5956
DEP_PLUGINS = nif_helpers

LOCAL_DEPS = runtime_tools

C_SRC_OUTPUT ?= $(CURDIR)/priv/lg_tracer

TEST_DEPS = ct_helper
dep_ct_helper = git https://github.com/ninenines/ct_helper master

include erlang.mk

# CT_OPTS += -boot start_sasl
