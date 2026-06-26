.PHONY: all compile test clean submodule

all: compile

submodule:
	git submodule update --init --recursive

compile: submodule
	rebar3 compile

test: submodule
	rebar3 eunit

clean:
	rebar3 clean
