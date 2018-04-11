rem disabling procket
echo > src/dependencies.hrl

rem now compiling
escript rebar clean compile eunit skip_deps=true