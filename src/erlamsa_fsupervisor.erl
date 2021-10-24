% Copyright (c) 2014-2018 Alexander Bolshev aka dark_k3y
%
% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:
%
% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.
%
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
% SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR
% THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%
%%%-------------------------------------------------------------------
%%% @author dark_k3y
%%% @doc
%%% Fuzzing supervisor
%%% @end
%%%-------------------------------------------------------------------

-module(erlamsa_fsupervisor).
-export([start/1, start_node/1, fsupervisor/2, get_supervisor_opts/1, 
         get_fuzzing_output/1, get_fuzzing_output/3,
         launch_fuzzing_process/2]).

-include("erlamsa.hrl").

-type fsupervisor_entry() :: {integer(), pid()}.
-type fsupervisor_queue() :: list(fsupervisor_entry()).

-spec get_supervisor_opts(options()) -> supervisor:child_spec().
get_supervisor_opts(Opts) ->
    #{id => ?MODULE,
    start => {?MODULE, start, [Opts]},
    restart => permanent,
    shutdown => brutal_kill,
    type => worker,
    modules => [?MODULE]}.

-spec launch_fuzzing_process(options(), pid()) -> ok.
launch_fuzzing_process(Dict, ParentPid) ->
    Output = erlamsa_utils:extract_function(
                erlamsa_main:fuzzer(maps:put(parentpid, ParentPid, Dict))),
    ParentPid ! {fuzzing_ok, self(), Output}.

-spec notify_supervisor(pid(), tuple()) -> ok.
notify_supervisor(fuzzing_supervisor, Msg) ->
    global:send(fuzzing_supervisor, Msg);
notify_supervisor(Pid, Msg) ->
    Pid ! Msg.

-spec get_fuzzing_output(options()) -> binary().
get_fuzzing_output(Dict) ->
    {FuzzNode, _} = erlamsa_app:get_free_node(),
    get_fuzzing_output(FuzzNode, fuzzing_supervisor, Dict).

%% TODO: return error when timeout?
-spec get_fuzzing_output(list(), pid(), options()) -> binary().
get_fuzzing_output(Node, SupervisorPid, Dict) ->
    %io:format("In get_fuzzing_output: ~p ~p ~p~n", [{self(), Node}, SupervisorPid, Dict]),
    AwaitPid = 
        case Node of
            local ->
                Pid = spawn(erlamsa_fsupervisor, launch_fuzzing_process, [Dict, self()]),
                notify_supervisor(SupervisorPid, {add, Pid}),
                Pid;
            RemotePid ->
                RemotePid ! {fuzz_opts, self(), maps:put(parentpid, self(), Dict)},
                RemotePid
        end,
    receive
        {fuzzing_ok, AwaitPid, Output} ->
            Output;
        {fuzzing_ok, Output} ->
            Output
    after
        90000 ->
            <<>>
    end.

-spec stop_process(pid(), integer(), undefined | [] | list()) -> true.
stop_process(_, _, undefined) ->
    true;
stop_process(Pid, Time, _ProcessInfo) ->
    erlamsa_logger:log(info, "Stopped possibly bugged process: ~p (launched @ ~p, now is ~p)~n",
                [Pid, Time, erlang:system_time(seconds)]),
    erlang:exit(Pid, kill).

-spec update_list(integer(), fsupervisor_entry(), fsupervisor_queue()) -> fsupervisor_queue().
update_list(Delta, H = {Time, Pid}, NewList) ->
    CurrentTime = erlang:system_time(seconds),
    if
        CurrentTime - Time > Delta ->
            stop_process(Pid, Time, erlang:process_info(Pid)),
            NewList;
        true ->
            [H|NewList]
    end.

-spec handle_command(fsupervisor_queue()) -> fsupervisor_queue().
handle_command(List) ->
    receive
        {add, NewFuzzerProcessPid} ->
            List ++ [{erlang:system_time(seconds), NewFuzzerProcessPid}]
    after
        100 ->
            List
    end.

-spec start(options()) -> {ok, pid()}.
start(Dict) ->
    Pid = spawn(erlamsa_fsupervisor, fsupervisor,
                    [maps:get(maxrunningtime, Dict, 30), []]),
    global:register_name(fuzzing_supervisor, Pid),
    {ok, Pid}.

-spec start_node(options()) -> {ok, pid()}.
start_node(Dict) ->
    Pid = spawn(erlamsa_fsupervisor, fsupervisor,
                    [maps:get(maxrunningtime, Dict, 30), []]),
    {ok, Pid}.

-spec fsupervisor(integer(), fsupervisor_queue()) -> no_return().
fsupervisor(Delta, []) ->
    NewList = handle_command([]),
    fsupervisor(Delta, NewList);
fsupervisor(Delta, [H|T]) ->
    NewList = update_list(Delta, H, handle_command(T)),
    fsupervisor(Delta, NewList).