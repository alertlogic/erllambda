APP := erllambda

COMP_NEEDED := erlang_r18_2_1,rebar3,setenv

all: unit

env :
	@echo "Ensuring build environment is boostrapped..."
	@APP=${APP} COMPS_NEEDED=$(COMP_NEEDED) ./setup.sh -u

MAKE_HOME = $(abspath $(or $(wildcard _checkouts/makeincl),\
			   $(wildcard _build/makeincl)))
-include $(MAKE_HOME)/makefile.allib
export MAKE_HOME
