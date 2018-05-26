-module(erlamsa_httpsvc).           
-export([get_supervisor_opts/1]).

get_supervisor_opts(Opts) ->								
	#{id => inets,
	start => {inets, start, 
    [
        httpd, 
        [{port, maps:get(svcport, Opts, 17771)}, 
        {server_name, maps:get(svchost, Opts, "localhost")}, 
        {document_root, "."}, 
        {modules,[mod_esi]},
        {server_root, "."}, 
        {erl_script_alias, {"/erlamsa", [erlamsa_esi]}}],
        stand_alone
    ]},
	restart => permanent,
	shutdown => brutal_kill,
	type => worker,
	modules => [inets]}.