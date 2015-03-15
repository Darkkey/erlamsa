% idea and part of original code from http://beezari.livejournal.com/191194.html

%%%-------------------------------------------------------------------
%%% @author dark_k3y
%%% @doc
%%% Very simple fuzzing proxy
%%% @end
%%%-------------------------------------------------------------------

-module(erlamsa_fuzzproxy).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

-include("erlamsa.hrl").

% API
-export([start/1, server_tcp/4]).

start(Opts) when is_list(Opts) ->
    start(maps:from_list(Opts));
start(Opts) ->
    Log = erlamsa_logger:build_logger(Opts),
    Workers = maps:get(workers, Opts, 10),
    Verbose = erlamsa_utils:verb(stdout, maps:get(verbose, Opts, 0)),
    {LocalPort, _, _} = maps:get(proxy_address, Opts),
    case gen_tcp:listen(LocalPort, [binary, {active, true}, {packet,0}]) of
        {ok, ListenSock} ->
            start_servers(Workers, ListenSock, Opts, Verbose, Log),
            {ok, Port} = inet:port(ListenSock),
            Port;
        {error,Reason} ->
            {error,Reason}
    end.


start_servers(0, _ListenSock, _Opts, _Verbose, _Log) -> ok;
start_servers(Workers, ListenSock, Opts, Verbose, Log) ->
    spawn(?MODULE, server_tcp,[ListenSock, Opts, Verbose, Log]),
    start_servers(Workers - 1, ListenSock, Opts, Verbose, Log).

server_tcp(ListenSock, Opts, Verbose, Log) ->
    {_, DHost, DPort} = maps:get(proxy_address, Opts),
    Verbose(io_lib:format("Proxy worker process started ~n", [])),
    random:seed(now()),
    case gen_tcp:accept(ListenSock) of
        {ok, ClientSocket} ->
            Verbose(io_lib:format("Got connect ~n", [])),
            Log("new connect(c->s)", []),
            case gen_tcp:connect(DHost, DPort,
				 [binary, {packet,0}, {active, true}]) of
		          {ok, ServerSocket} ->
		              loop_tcp(ClientSocket, ServerSocket, Opts, Verbose, Log), 
                      server_tcp(ListenSock, Opts, Verbose, Log);
		          E ->
		              io:format("Error: connect to server failed!~n"),
		          E
		      end,
		  ok;
        Other ->
            io:format("Error: accept returned ~w~n",[Other]),
            ok
    end.

loop_tcp(ClientSocket, ServerSocket, Opts, Verbose, Log) ->
    {ProbToClient, ProbToServer} = maps:get(proxy_probs, Opts, {0.0, 0.0}),
    _Log = maps:get(logger, Opts, fun (X) -> X end),
    inet:setopts(ClientSocket, [{active,once}]), 
    receive
        {tcp, ClientSocket, Data} ->
	        Verbose(io_lib:format("read tcp client->server ~p ~n",[Data])),
            Log("from client(c->s): ~p", [Data]),
            Ret = fuzz(ProbToServer, Opts, Data), 
            Verbose(io_lib:format("wrote tcp client->server ~p ~n",[Ret])),            
            gen_tcp:send(ServerSocket, Ret),
            Log("from fuzzer(c->s): ~p", [Data]),
            loop_tcp(ClientSocket, ServerSocket, Opts, Verbose, Log); 
        {tcp, ServerSocket, Data} ->
            Verbose(io_lib:format("read tcp server->client ~p ~n", [Data])),
            Log("from server(s->c): ~p", [Data]),
            Ret = fuzz(ProbToClient, Opts, Data),
            Verbose(io_lib:format("wrote tcp server->client ~p ~n", [Ret])),
            gen_tcp:send(ClientSocket, Ret),
            Log("from server(s->c): ~p", [Data]),
            loop_tcp(ClientSocket, ServerSocket, Opts, Verbose, Log);
        {tcp_closed, ClientSocket} ->
            gen_tcp:close(ServerSocket),
            Verbose(io_lib:format("client socket ~w closed [~w]~n", [ClientSocket, self()])),
            Log("client close (c->s)", []),
            ok;
        {tcp_closed, ServerSocket}->
            gen_tcp:close(ClientSocket),
            Verbose(io_lib:format("server socket ~w closed [~w]~n",[ServerSocket,self()])),
            Log("server close (s->c)", []),
            ok	
    end.


fuzz(Prob, Opts, Data) ->
    call_fuzzer(Prob, erlamsa_rnd:rand_float(), Opts, Data).

call_fuzzer(Prob, Rnd, _Opts, Data) when Rnd >= Prob -> Data;
call_fuzzer(_Prob, _Rnd, Opts, Data) ->
    NewOpts = maps:put(paths, [direct], 
               maps:put(output, return, 
                maps:put(input, Data, 
                  Opts))),
    erlamsa_utils:extract_function(erlamsa_main:fuzzer(NewOpts)).

