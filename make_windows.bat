@echo off
rem reuires erc, rebar3 and escript in %PATH

rem disabling procket
copy rebar.config rebar.config.bak
copy rebar.config.win rebar.config
echo %% > src/dependencies.hrl
echo %% > src/version.hrl

rem now compiling
rebar3 get-deps compile eunit skip_deps=true
escript script-builder
