-module(erlamsa_httpsvc).           
-export([start/2]).

start(Host, Port) -> 
    inets:start(),
    erlamsa_logger:log(info, "Starting HTTP service on ~s:~p", [Host, Port]),       
    inets:start(httpd, [{port, Port}, 
                        {server_name, Host}, 
                        {document_root, "."}, 
                        {modules,[mod_esi]},
                        {server_root, "."}, 
                        {erl_script_alias, {"/erlamsa", [erlamsa_esi]}}]).