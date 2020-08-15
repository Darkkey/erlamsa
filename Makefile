ifeq ($(PREFIX),)
    PREFIX := /usr/local
endif

REBAR3 := `which rebar3 || echo ./rebar3`

default: bootstrap compile test

bootstrap:
	which rebar3 || test -f rebar3 || (mkdir -p _build && cd _build && git clone https://github.com/erlang/rebar3.git && cd rebar3 && ./bootstrap && cp ./rebar3 ../../rebar3 && cd ../..)

test:
	$(REBAR3) eunit

compile:
	echo "-define(GITVER, \"; commit: `git log -1 --format=%cd --date=local` `git rev-parse HEAD`\")." > src/version.hrl
	echo "-define(PREFIXDIR, \"$(PREFIX)\")." >> src/version.hrl
	mkdir -p priv
	$(REBAR3) get-deps
	$(REBAR3) compile
	cp _build/default/lib/erlexec/priv/*/exec-port priv/
	cp _build/default/lib/erlserial/priv/bin/serial priv/erlserial
	cp -r _build/default/lib/procket/priv/procket* priv/
	escript script-builder

fast:
	$(REBAR3) compile skip_deps=true
	escript script-builder

install:
	install -d $(DESTDIR)$(PREFIX)/bin
	install erlamsa $(DESTDIR)$(PREFIX)/bin
	install erlamsa_daemon $(DESTDIR)$(PREFIX)/bin
	install priv/exec-port $(DESTDIR)$(PREFIX)/bin
	install priv/erlserial $(DESTDIR)$(PREFIX)/bin
	install -d $(DESTDIR)$(PREFIX)/lib/erlamsa/procket
	install priv/procket $(DESTDIR)$(PREFIX)/lib/erlamsa/procket
	install priv/procket.so $(DESTDIR)$(PREFIX)/lib/erlamsa/procket
	install priv/procket_drv.so $(DESTDIR)$(PREFIX)/lib/erlamsa/procket

