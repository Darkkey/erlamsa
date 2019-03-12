-module(erlamsa_daemon).

-export([main/1]).
    
main(Args) ->    
    Dict = erlamsa_cmdparse:parse(Args), 
    erlamsa_app:main(Dict).

