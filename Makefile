default: compile test

test:
	./rebar eu

compile:
	echo "-define(GITVER, \"; commit: `git log -1 --format=%cd --date=local` `git rev-parse HEAD`\")." > src/version.hrl
	mkdir -p priv
	./rebar get-deps
	./rebar compile
	cp deps/erlexec/priv/*/exec-port priv/
	./rebar escriptize

fast:
	./rebar compile escriptize skip_deps=true


