-module(erlamsa_fsupervisor).
-export([start/1, fsupervisor/2, get_supervisor_opts/1, get_fuzzing_output/1, launch_fuzzing_process/2]).

get_supervisor_opts(Opts) ->
	#{id => ?MODULE,
	start => {?MODULE, start, [Opts]},
	restart => permanent,
	shutdown => brutal_kill,
	type => worker,
	modules => [?MODULE]}.

launch_fuzzing_process(Dict, ParentPid) ->
    Output = erlamsa_utils:extract_function(erlamsa_main:fuzzer(maps:put(parentpid, ParentPid, Dict))),
    ParentPid ! {ok, self(), Output}.

get_fuzzing_output(Dict) ->
    Pid = spawn(erlamsa_fsupervisor, launch_fuzzing_process, [Dict, self()]),
    global:send(fuzzing_supervisor, {add, Pid}),
    receive
        {ok, Pid, Output} ->
            Output
    after 
        3600000 ->
            <<>>
    end.

stop_process(_, _, undefined) ->
    ok;
stop_process(Pid, Time, _ProcessInfo) ->
    io:format("Stopped possibly bugged process: ~p (launched @ ~p, now is ~p)~n", 
                [Pid, Time, erlang:system_time(seconds)]),
    erlang:exit(Pid, kill).

update_list(Delta, H = {Time, Pid}, NewList) -> 
    CurrentTime = erlang:system_time(seconds),
    if 
        CurrentTime - Time > Delta ->
            stop_process(Pid, Time, process_info(Pid)),
            NewList;
        true ->
            [H|NewList]
    end.

handle_command(List) ->    
    receive
        {add, NewFuzzerProcessPid} -> 
            List ++ [{erlang:system_time(seconds), NewFuzzerProcessPid}]            
    after 
        100 ->
            List
    end.

start(Dict) ->
    Pid = spawn(erlamsa_fsupervisor, fsupervisor, 
                    [maps:get(maxrunningtime, Dict, 30), []]),
    global:register_name(fuzzing_supervisor, Pid),
    {ok, Pid}.

fsupervisor(Delta, []) ->
    NewList = handle_command([]),
    fsupervisor(Delta, NewList);
fsupervisor(Delta, [H|T]) ->
    NewList = update_list(Delta, H, handle_command(T)),
    fsupervisor(Delta, NewList).