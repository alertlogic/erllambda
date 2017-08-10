APP := erllambda

ERLANG_VERSION ?= r19_3_6_1
COMPS_NEEDED := erlang_$(ERLANG_VERSION),rebar3,setenv
NATIVE_LIBS := true

all: unit

env :
	@echo "Ensuring build environment is boostrapped..."
	@APP=$(APP) COMPS_NEEDED=$(COMPS_NEEDED) ERLANG_VERSION=$(ERLANG_VERSION) ./setup.sh -u

MAKE_HOME = $(abspath $(or $(wildcard _checkouts/makeincl),\
			   $(wildcard _build/makeincl)))
-include $(MAKE_HOME)/makefile.allib
export MAKE_HOME
