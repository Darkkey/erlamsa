ifeq ($(PREFIX),)
    PREFIX := /usr/local
endif

default: compile test

test:
	./rebar eu

compile:
	echo "-define(GITVER, \"; commit: `git log -1 --format=%cd --date=local` `git rev-parse HEAD`\")." > src/version.hrl
	echo "-define(PREFIXDIR, \"$(PREFIX)\")." >> src/version.hrl
	mkdir -p priv
	./rebar get-deps
	./rebar compile
	cp deps/erlexec/priv/*/exec-port priv/
	cp deps/erlserial/priv/bin/serial priv/erlserial
	cp -r deps/procket/priv/procket* priv/
	escript script-builder

fast:
	./rebar compile skip_deps=true
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

