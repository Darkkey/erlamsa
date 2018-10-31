-module(erlamsa_gfcomms).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

-include("erlamsa.hrl").

-export([start/1]).

start(Opts) when is_list(Opts) ->
    start(maps:from_list(Opts));
start(Opts) ->
    erlamsa_rnd:seed(now()),
    Log = erlamsa_logger:build_logger(Opts),
    %% Workers = maps:get(workers, Opts, 10), %% TODO: workers count
    Generate = erlamsa_utils:make_fuzzer(maps:get(external_gen, Opts, nil)),
    {_Proto, Port} = maps:get(input_endpoint, Opts),
    Verbose = erlamsa_utils:verb(stdout, maps:get(verbose, Opts, 0)),
    start_server(Port, Opts, Generate, Verbose, Log).
    
start_server(Port, Opts, Generate, Verbose, Log) ->
    Pid = spawn_link(fun() ->
        {ok, Listen} = gen_tcp:listen(Port, [binary, {active, false}]),
        spawn(fun() -> server_tcp(Listen, Opts, Generate, Verbose, Log) end),
        timer:sleep(infinity)
        end),
    {ok, Pid}.
 
server_tcp(ListenSocket, Opts, Generate, Verbose, Log) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> server_tcp(ListenSocket, Opts, Generate, Verbose, Log) end),
    Log("new connect", []),
    loop_tcp(Socket, Opts, Generate, Verbose, Log).
 
loop_tcp(Socket, Opts, Generate, Verbose, Log) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            Log("Read from target: ~p",[Data]),
            {ok, ToWrite} = Generate(tcp, Data, 
                maps:put(write_socket, 
                    fun(DataToWrite) -> 
                        Log("Written to target: ~p", [DataToWrite]),
                        gen_tcp:send(Socket, DataToWrite)
                    end
                    , Opts)),
            Log("Written to target: ~p",[ToWrite]),
            gen_tcp:send(Socket, ToWrite),
            loop_tcp(Socket, Opts, Generate, Verbose, Log);
        {tcp_closed, Socket} ->
            Log("Connection to target closed.",[]),
            gen_tcp:close(Socket)
    end.