-module(erlamsa_app).

-behaviour(application).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

sleep() ->
	timer:sleep(infinity).

prepare_auxproc(Dict) ->
    [
		erlamsa_logger:get_supervisor_opts(Dict),
		erlamsa_fsupervisor:get_supervisor_opts(Dict),
		erlamsa_monitor:get_supervisor_opts(Dict)	
	].

start_behaviour(Dict) -> 
    case maps:get(mode, Dict, stdio) of
        genfuzz -> %%TODO: make genfuzz supevisable
            {[], fun () -> erlamsa_gfcomms:start(Dict), sleep() end};
        proxy ->     	
            {[erlamsa_fuzzproxy:get_supervisor_opts(Dict)], fun sleep/0};
        stdio ->    		
            {[], fun () -> erlamsa_main:fuzzer(Dict), timer:sleep(1) end};
        httpsvc ->
            {[erlamsa_httpsvc:get_supervisor_opts(Dict)], fun sleep/0};
        faas ->
            io:format("Mode not supported yet!"),
            exit(normal);
        _Else -> 
            io:format("Invalid mode."),
            exit(normal)
	end.

start(escript, Dict) ->
	AuxProcesses = prepare_auxproc(Dict),
	{MainProcess, StartFunc} = start_behaviour(Dict),
	erlamsa_sup:start_link(lists:flatten([AuxProcesses|MainProcess])),
	%% Profiler should not be supervised
	erlamsa_profiler:start(maps:get(debug, Dict, nil), [erlamsa_logger:get_pid()]),
	StartFunc();
start(_StartType, _StartArgs) ->
    throw("Unknown start type!~n").

stop(_State) ->
    ok.
