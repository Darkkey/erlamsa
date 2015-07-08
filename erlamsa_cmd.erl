#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable 

%% -sname erlamsa_cmd
-mode(compile).

sleep() ->
	timer:sleep(5000000),
	sleep().

main(Args) ->
	true = code:add_pathz(filename:dirname(escript:script_name()) 
                       ++ "/ebin"),
    Dict = erlamsa_cmdparse:parse(Args),
    case maps:get(mode, Dict, stdio) of
    	proxy ->     	
    		erlamsa_fuzzproxy:start(Dict),
    		sleep();
    	stdio ->    		
    		erlamsa_main:fuzzer(Dict), timer:sleep(1);
    	_Else -> 
    		io:format("Mode not supported yet!")
    end.
