all: unit

release: distclean unit
	@echo "Release built successfully!"


compile:
	rebar3 compile

unit:
	rebar3 do cover --reset, eunit --cover, cover --verbose

shell:
	rebar3 as test shell

clean:
	rebar3 clean

depclean:
	rebar3 clean -a

distclean: depclean
	rebar3 unlock
	rm -rf _build

