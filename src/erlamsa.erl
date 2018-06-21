-module(erlamsa).

-export([main/1]).

main(Args) ->
    RuntimeDir = filename:dirname(escript:script_name()),
    true = code:add_pathz(RuntimeDir ++ "/ebin"),
    {Osfamily, _} = os:type(),
    ServiceMode = length([Z || Z <- Args, Z =:= "-D"]),
    Dict = erlamsa_cmdparse:parse(Args), %% check correctness of input args before proceed
    case {Osfamily, ServiceMode} of
        {unix, 1} ->
            CmdLst = ["erl -noshell -smp enable -detached", "-pz",
                      RuntimeDir ++ "/ebin"] ++
                      erlamsa_utils:get_deps_dirs(RuntimeDir) ++
                      ["-run erlamsa_app main -extra" | Args],
            Cmd = lists:foldr(fun (X, Acc) -> [X, " " | Acc] end, [], CmdLst),
            os:cmd(Cmd);
        _Else ->
            true = erlamsa_utils:load_deps(RuntimeDir),
            erlamsa_app:main(Dict)
    end.

