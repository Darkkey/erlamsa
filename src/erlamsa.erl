-module(erlamsa).

-export([main/1]).
    
main(Args) ->    
    {Osfamily, _} = os:type(),
    ServiceMode = length([Z || Z <- Args, Z =:= "-D"]),
    Dict = erlamsa_cmdparse:parse(Args), %% check correctness of input args before proceed
    case {Osfamily, ServiceMode} of
        {unix, 1} ->
            CmdLst = [escript:script_name() ++ "_daemon" | Args],
            Cmd = lists:foldr(fun (X, Acc) -> [X, " " | Acc] end, [], CmdLst),
            os:cmd(Cmd);
        _Else ->
            erlamsa_app:main(Dict)
    end.

