% Copyright (c) 2011-2014 Aki Helin
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
%%% Main application file: implementation of process.
%%% @end
%%%-------------------------------------------------------------------

-module(erlamsa_app).

-behaviour(application).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("erlamsa.hrl").

%% Module types
-type start_type() :: escript | direct | remote | atom().

%% Application callbacks
-export([main/0, main/1, start/2, stop/1,  node_keepalive/5,
         loop_service/1, loop_node/2, call/2, fuzz/1, fuzz/2,
         get_free_node/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec main() -> no_return().
main() ->
    Dict = erlamsa_cmdparse:parse(init:get_plain_arguments()),
	erlamsa_app:start(escript, Dict).

-spec main(list(string())) -> no_return().
main(Dict) ->
	erlamsa_app:start(escript, Dict).

-spec get_supervisor_opts(options()) -> supervisor:child_spec().
get_supervisor_opts(Opts) ->
    [#{id => ?MODULE,
    start => {?MODULE, start, [Opts]},
    restart => permanent,
    shutdown => brutal_kill,
    type => worker,
    modules => [?MODULE]}].

-spec sleep() -> ok.
sleep() ->
    timer:sleep(infinity), ok.

-spec prepare_auxproc(options()) -> list(supervisor:child_spec()).
prepare_auxproc(Dict) ->
    ExtRnd = maps:get(ext_rnd, Dict, fun(_Dict) -> [] end),
    [
        erlamsa_logger:get_supervisor_opts(Dict),
        erlamsa_fsupervisor:get_supervisor_opts(Dict),
        erlamsa_monitor:get_supervisor_opts(Dict),
        ExtRnd(Dict)
    ].

-spec create_pid_file(list() | nil) -> ok.
create_pid_file(nil) -> ok;
create_pid_file(FileName) ->
    file:write_file(FileName, os:getpid()).

-spec start_behaviour(options()) -> {list(supervisor:child_spec()), fun()}.
start_behaviour(Dict) ->
    create_pid_file(maps:get(pidfile, Dict, nil)),
    case maps:get(mode, Dict, stdio) of
        genfuzz -> %%TODO: make genfuzz supevisable
            {[], fun () -> erlamsa_gfcomms:start(Dict), run(Dict), sleep() end};
        proxy ->
            {[erlamsa_fuzzproxy:get_supervisor_opts(Dict)], fun () -> run(Dict), sleep() end};
        stdio ->
            {[], fun () -> erlamsa_main:fuzzer(Dict), timer:sleep(1) end};
        httpsvc ->
            {[erlamsa_httpsvc:get_supervisor_opts(Dict), erlamsa_cmanager:get_supervisor_opts(Dict)], 
            fun () -> erlamsa_httpsvc:prepare_ets(Dict), run(Dict), sleep() end};
        faas ->
            io:format("Mode not supported yet!"),
            exit(normal);
        _Else ->
            io:format("Invalid mode."),
            exit(normal)
    end.

-spec init_storage(map()) -> ok.
init_storage(Dict) ->
    case maps:get(logger_mnesia, Dict, nil) =:= maps:get(cloudsvc, Dict, nil) of
        true -> ok;
        false -> 
            Dir = maps:get(mnesia_dir, Dict), 
            application_controller:set_env(mnesia, dir, Dir),
            mnesia:create_schema([node()]),
            mnesia:start()
    end.

%TODO: refactor a little, to avoid duplicating code
% Erlamsa has 4 possible starting modes:
% - escript: called from escript standalone application
% - direct: called from local erlang shell
% - remote: "client" mode -- data will be send to remote fuzzing node
% - node: operates as a remote fuzzing node
-spec start(start_type(), options()) -> ok.
start(escript, Dict) ->
    ets:new(global_config, [public, named_table, bag]),
    init_storage(Dict),
    AuxProcesses = prepare_auxproc(Dict),
    {MainProcess, StartFunc} = start_behaviour(Dict),
    erlamsa_sup:start_link(lists:flatten([AuxProcesses | MainProcess])),
    %% Profiler should not be supervised
    erlamsa_profiler:start(maps:get(debug, Dict, nil), [erlamsa_logger:get_pid()]),
    StartFunc();
start(direct, Dict) ->
    ets:new(global_config, [named_table, bag]),
    AuxProcesses = prepare_auxproc(Dict),
    MainProcess = get_supervisor_opts(Dict),
    erlamsa_sup:start_link(lists:flatten([AuxProcesses | MainProcess])),
    %% Profiler should not be supervised
    erlamsa_profiler:start(maps:get(debug, Dict, nil), [erlamsa_logger:get_pid()]);
start(remote, {Pid, Node}) ->
    {Pid, Node} ! {ping, self()};
start(node, Node) ->
    spawn(?MODULE, loop_node, [Node, maps:new()]),
    timer:sleep(infinity);
start(_StartType, _StartArgs) ->
    throw("Unknown start type!~n").

-spec run(options()) -> {ok, pid()}.
run(Opts) ->
    Pid = spawn(?MODULE, loop_service, [Opts]),
    register(erlamsa, Pid),  
    global:register_name(erlamsa, Pid),
    {ok, Pid}.

-spec loop_service(options()) -> no_return().
loop_service(Opts) -> 
    erlamsa_rnd:seed(now()),
    timer:send_interval(?NODES_CHECKTIMER, {cleanup_nodes}),
    loop(fuzzing_supervisor, [{local, infinity}], Opts).

-spec node_keepalive(term(), pid(), pid(), list() | undefined, list() | undefined) -> true.
node_keepalive(_, _, _, undefined, _) -> true;
node_keepalive(_, _, _, _, undefined) -> true;
node_keepalive(Node, SupervisorPid, AppPid, _, _) ->
    {erlamsa, Node} ! {join, AppPid},
    timer:sleep(?NODE_KEEPALIVE),
    node_keepalive(Node, SupervisorPid, AppPid, 
                   erlang:process_info(SupervisorPid), 
                   erlang:process_info(AppPid)).

-spec loop_node(term(), options()) -> no_return().
loop_node(Node, Opts) -> 
    {erlamsa, Node} ! {join, self()},
    {ok, SupervisorPid} = erlamsa_fsupervisor:start_node(Opts),
    erlamsa_rnd:seed(now()),
    spawn(?MODULE, node_keepalive, [Node, SupervisorPid, self(), 
          erlang:process_info(SupervisorPid), erlang:process_info(self())]),
    loop(SupervisorPid, [{local, infinity}], Opts).

%TODO: this (using 'erlamsa !') allows only single depth of nodes tree...
-spec get_free_node() -> pid() | atom().
get_free_node() ->
    erlamsa ! {get_node, self()},
    receive
        {node, Node} -> Node 
    end.

-spec update_node_time(pid(), list({pid(), integer()}), list({pid(), integer()}) | []) -> list({pid(), integer()}).
update_node_time(Node, [{Node, _}|T], Acc) ->
    Acc ++ [{Node, erlang:system_time(seconds)}|T];
update_node_time(Node, [], Acc) ->
    erlamsa_logger:log(info, "New fuzzing node appeared: ~p", [Node]),
    [{Node, erlang:system_time(seconds)} | Acc];
update_node_time(Node, [H|T], Acc) ->
    update_node_time(Node, T, [H|Acc]).

-spec remove_outdated_nodes(integer(), list({pid(), integer()}), list({pid(), integer()}) | []) -> list({pid(), integer()}).
remove_outdated_nodes(CurrentTime, [{Node, Time} | T], Acc) when CurrentTime - Time > ?NODE_ALIVE_DELTA ->
    erlamsa_logger:log(info, "Removing potentially down node ~p", [Node]),
    remove_outdated_nodes(CurrentTime, T, Acc);
remove_outdated_nodes(_CurrentTime, [], Acc) -> 
    Acc;
remove_outdated_nodes(CurrentTime, [H|T], Acc) -> 
    remove_outdated_nodes(CurrentTime, T, [H|Acc]).

-spec loop(pid(), list(), options()) -> no_return().
loop(SupervisorPid, Nodes, Opts) ->    
    NewNodes = receive
        {fuzz, Client, Data} ->
            %io:format("Fuzzing for ~p: ~p~n", [Client, Data]),
            NewOpts = erlamsa_utils:get_direct_fuzzing_opts(Data, Opts),
            {FuzzNode, _} = erlamsa_rnd:rand_elem(Nodes),
            Client ! {fuzzing_ok, erlamsa_fsupervisor:get_fuzzing_output(FuzzNode, SupervisorPid, NewOpts)},
            Nodes;
        {fuzz_opts, Client, NewOpts} ->
            %io:format("Fuzzing for ~p: ~p~n", [Client, NewOpts]),
            {FuzzNode, _} = erlamsa_rnd:rand_elem(Nodes),
            Client ! {fuzzing_ok, erlamsa_fsupervisor:get_fuzzing_output(FuzzNode, SupervisorPid, NewOpts)},
            Nodes;
        {ping, Client} -> 
            %io:format("got ping from ~p~n", [Client]),
            Client ! {ping_ack, self()},
            Nodes;
        {join, Client} ->
            %io:format("register new child node ~p~n", [Client]),
            Client ! {joined},
            update_node_time(Client, Nodes, []);
        {get_nodes, Client} ->
            %io:format("get_nodes request ~p~p~n", [Client, Nodes]),
            Client ! {nodes, Nodes},
            Nodes;
        {get_node, Client} ->
            %io:format("get_node request ~p~p~n", [Client, Nodes]),
            Client ! {node, erlamsa_rnd:rand_elem(Nodes)},
            Nodes;
        {cleanup_nodes} -> 
            %io:format("timer_event~n"),
            Tmp = remove_outdated_nodes(erlang:system_time(seconds),Nodes,[]),
            %io:format("Nodes: ~p~n", [Tmp]),
            Tmp
    end,
    loop(SupervisorPid, NewNodes, Opts).

-spec call(pid(), binary()) -> binary().
call(Node, Data) ->
    global:send(Node, {fuzz, self(), Data}),
    receive
        {fuzzing_ok, Result} -> Result
    end.

-spec fuzz(binary()) -> binary().
fuzz(Data) ->
    fuzz(Data, maps:new()).

-spec fuzz(binary(), map()) -> binary().
fuzz(Data, Dict) ->
    Opts = erlamsa_utils:get_direct_fuzzing_opts(Data, Dict),
    MutatedData = erlamsa_utils:extract_function(erlamsa_main:fuzzer(Opts)),
    MutatedData.

-spec stop(any()) -> ok.
stop(_State) ->
    ok.
