@echo off
rem reuires erc, rebar3 and escript in %PATH

rem disabling procket
copy rebar.config rebar.config.bak
copy rebar.config.win rebar.config
echo %% > src/dependencies.hrl
echo %% > src/version.hrl

rem now compiling
escript rebar3.win compile eunit 
escript script-builder
