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
	true = code:add_pathz(RuntimeDir ++ "/deps/procket/ebin"),
    Dict = erlamsa_cmdparse:parse(Args),
    case maps:get(mode, Dict, stdio) of
		genfuzz ->
			erlamsa_gfcomms:start(Dict),
			sleep();
    	proxy ->     	
    		erlamsa_fuzzproxy:start(Dict),
    		sleep();
    	stdio ->    		
    		erlamsa_main:fuzzer(Dict), timer:sleep(1);
		faas ->
			io:format("Mode not supported yet!");
    	_Else -> 
    		io:format("Invalid mode.")
    end.
