#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable 

%% -sname erlamsa_cmd
-mode(compile).

sleep() ->
	timer:sleep(infinity).

main(Args) ->
	RuntimeDir = filename:dirname(escript:script_name()),
	true = code:add_pathz(RuntimeDir ++ "/ebin"),
	true = erlamsa_cmdparse:load_deps(RuntimeDir),
    Dict = erlamsa_cmdparse:parse(Args),
	erlamsa_logger:start(Dict),
	erlamsa_fsupervisor:start(Dict),
    case maps:get(mode, Dict, stdio) of
		genfuzz ->
			erlamsa_gfcomms:start(Dict),
			sleep();
    	proxy ->     	
    		erlamsa_fuzzproxy:start(Dict),
    		sleep();
    	stdio ->    		
    		erlamsa_main:fuzzer(Dict), timer:sleep(1);
		httpsvc ->
			erlamsa_httpsvc:start(maps:get(svchost, Dict, "localhost"), 
								  maps:get(svcport, Dict, 17771)								  
								),
			sleep();
		faas ->
			io:format("Mode not supported yet!");
    	_Else -> 
    		io:format("Invalid mode.")
    end.
