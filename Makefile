all: compile

compile: deps
	./rebar compile

deps:
	./rebar get-deps

install: compile
	./rebar generate

test: deps compile
	./rebar eunit skip_deps=true

