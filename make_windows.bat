rem disabling procket
echo > src/dependencies.hrl

rem now compiling
escript rebar get-deps compile eunit skip_deps=true
