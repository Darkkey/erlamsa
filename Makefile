default: compile test

test:
	./rebar eu

compile:
	./rebar get-deps
	./rebar compile
	./rebar escriptize



