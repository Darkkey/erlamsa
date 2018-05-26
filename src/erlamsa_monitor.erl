-module(erlamsa_monitor).           
-export([get_supervisor_opts/1, monitors/0]).

get_supervisor_opts(Opts) ->
    get_monitors(maps:get(monitor, Opts, nil)).

monitors() ->
    [{r2, erlamsa_mon_r2},
     {cdb, erlamsa_mon_cdb}].

get_monitors(nil) ->
    [];
get_monitors({InMonName, Params}) ->
    lists:foldl(
        fun ({MonName, ModName}, Acc) when MonName == InMonName-> 
                [#{id => ModName,
                start => {ModName, start, [Params]},
                restart => temporary,
                shutdown => brutal_kill,
                type => worker,
                modules => [ModName]} | Acc];
            (_, Acc) -> Acc 
        end,
        [],
        monitors()
    ).
    



