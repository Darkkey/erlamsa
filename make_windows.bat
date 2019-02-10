@echo off
rem disabling procket
copy rebar.config rebar.config.bak
copy rebar.config.win rebar.config
echo %% > src/dependencies.hrl
echo %% > src/version.hrl

rem now compiling
escript rebar get-deps compile eunit escriptize skip_deps=true
