-module(erlamsa_monitor).           
-export([start/1, monitors/0]).

monitors() ->
    [{r2, fun erlamsa_mon_r2:start/1},
     {cdb, fun erlamsa_mon_cdb:launch/1}].

start_monitor(nil) ->
    ok;
start_monitor({InMonName, Params}) ->
    L = length(
        lists:map(
            fun ({MonName, Func}) when MonName == InMonName-> Func(Params);
                (_) -> ok end,
            monitors()
        ))
    . %%TODO: check and write message if it's not true.

start(Opts) when is_list(Opts) ->
    start(maps:from_list(Opts));
start(Opts) ->
    start_monitor(maps:get(monitor, Opts, nil)).



