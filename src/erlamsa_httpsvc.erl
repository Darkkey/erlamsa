-module(erlamsa_httpsvc).           
-export([start/2]).

start(Host, Port) -> 
    inets:start(),
    inets:start(httpd, [{port, Port}, {server_name, Host}, {document_root, "."}, {modules,[mod_esi]},{server_root, "."}, {erl_script_alias, {"/erlamsa", [erlamsa_esi]}}]).