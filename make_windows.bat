@echo off
rem disabling procket
echo %% > src/dependencies.hrl

rem now compiling
escript rebar get-deps compile eunit escriptize skip_deps=true
