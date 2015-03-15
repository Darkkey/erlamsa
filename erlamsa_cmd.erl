#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -name erlamsa -sname erlamsa_cmd
-mode(compile).

sleep() ->
	timer:sleep(5000000),
	sleep().

main(Args) ->
	true = code:add_pathz(filename:dirname(escript:script_name()) 
                       ++ "/src"),
    Dict = erlamsa_cmdparse:parse(Args),
    case maps:get(mode, Dict, stdio) of
    	proxy -> 
    		erlamsa_fuzzproxy:start(Dict),
    		sleep();
    	_Else -> io:format("Mode not supported yet!")
    end.
