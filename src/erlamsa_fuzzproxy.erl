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
-export([start/1, server_tcp/4, loop_udp/7]).

listen(http, LocalPort) ->
    listen(tcp, LocalPort);
listen(tcp, LocalPort) ->
    gen_tcp:listen(LocalPort, [binary, {active, true}, {reuseaddr, true}, {packet,0}]);
listen(udp, LocalPort) ->
    gen_udp:open(LocalPort, [binary, {active, true}, {reuseaddr, true}]);
listen(_, _P) ->
    {error, "Unsupported fuzzing layer."}.

start(Opts) when is_list(Opts) ->
    start(maps:from_list(Opts));
start(Opts) ->
    Log = erlamsa_logger:build_logger(Opts),
    Workers = maps:get(workers, Opts, 10),
    Verbose = erlamsa_utils:verb(stdout, maps:get(verbose, Opts, 0)),
    {StrProto, LocalPort, _, _, _} = maps:get(proxy_address, Opts),
    Proto = list_to_atom(StrProto),
    case listen(Proto, LocalPort) of
        {ok, ListenSock} ->
            start_servers(transport(Proto), Workers, ListenSock, Opts, Verbose, Log),
            {ok, Port} = inet:port(ListenSock),
            Port;
        {error,Reason} ->
            io:format("~s", [Reason]),
            {error,Reason}
    end.

transport(http) -> tcp;
transport(Tr) -> Tr.

start_servers(tcp, Workers, ListenSock, Opts, Verbose, Log) ->
    start_tcp_servers(Workers, ListenSock, Opts, Verbose, Log);
start_servers(udp, _Workers, ListenSock, Opts, Verbose, Log) ->
    Verbose(io_lib:format("Proxy worker process started, socket id ~p ~n", [ListenSock])),
    Pid = spawn(?MODULE, loop_udp, [ListenSock, init_clientsocket, [], 0, Opts, Verbose, Log]),
    gen_udp:controlling_process(ListenSock, Pid).

start_tcp_servers(0, _ListenSock, _Opts, _Verbose, _Log) -> ok;
start_tcp_servers(Workers, ListenSock, Opts, Verbose, Log) ->
    spawn(?MODULE, server_tcp,[ListenSock, Opts, Verbose, Log]),
    start_tcp_servers(Workers - 1, ListenSock, Opts, Verbose, Log).

server_tcp(ListenSock, Opts, Verbose, Log) ->
    {Proto, _, _, DHost, DPort} = maps:get(proxy_address, Opts),
    Verbose(io_lib:format("Proxy worker process started ~n", [])),
    random:seed(now()),
    case gen_tcp:accept(ListenSock) of
        {ok, ClientSocket} ->
            Verbose(io_lib:format("Got connect ~n", [])),
            Log("new connect(c->s)", []),
            case gen_tcp:connect(DHost, DPort,
				 [binary, {packet,0}, {active, true}]) of
		          {ok, ServerSocket} ->
		              loop_tcp(Proto, ClientSocket, ServerSocket, Opts, Verbose, Log),
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

loop_udp(SrvSocket, init_clientsocket, ClientHost, ClientPort, Opts, Verbose, Log) ->
    random:seed(now()),
    {"udp", _LPort, ClSocketPort, _, _} = maps:get(proxy_address, Opts),
    {ok, ClSocket} = listen(udp, ClSocketPort),
    gen_udp:controlling_process(ClSocket, self()),
    loop_udp(SrvSocket, ClSocket, ClientHost, ClientPort, Opts, Verbose, Log);
loop_udp(SrvSocket, ClSocket, ClientHost, ClientPort, Opts, Verbose, Log) ->
    {"udp", _, _, ServerHost, ServerPort} = maps:get(proxy_address, Opts),
    {ProbToClient, ProbToServer} = maps:get(proxy_probs, Opts, {0.0, 0.0}),
    receive
        {udp, SrvSocket, Host, Port, Data} = Msg ->
            Verbose(io_lib:format("read udp client->server ~p ~p ~n",[Data, Msg])),
            {_, Ret} = fuzz(udp, ProbToServer, Opts, Data),
            Verbose(io_lib:format("wrote udp client->server ~p ~n",[Ret])),
            gen_udp:send(ClSocket, ServerHost, ServerPort, Ret),
            loop_udp(SrvSocket, ClSocket, Host, Port, Opts, Verbose, Log);
        {udp, ClSocket, ServerHost, ServerPort, Data} = Msg ->
            Verbose(io_lib:format("read udp server:->client ~p ~p ~n",[Data, Msg])),
            case ClientHost of
                [] ->
                    loop_udp(SrvSocket, ClSocket, [], 0, Opts, Verbose, Log);
                _Else ->
                    {_, Ret} = fuzz(udp, ProbToClient, Opts, Data),
                    gen_udp:send(SrvSocket, ClientHost, ClientPort, Ret),
                    Verbose(io_lib:format("wrote udp server->client ~p ~n",[Ret])),
                    loop_udp(SrvSocket, ClSocket, ClientHost, ClientPort, Opts, Verbose, Log)
            end
    end.


loop_tcp(Proto, ClientSocket, ServerSocket, Opts, Verbose, Log) ->
    {ProbToClient, ProbToServer} = maps:get(proxy_probs, Opts, {0.0, 0.0}),
    _Log = maps:get(logger, Opts, fun (X) -> X end), %% ????
    inet:setopts(ClientSocket, [{active,once}]),
    receive
        {tcp, ClientSocket, Data} ->
	        Verbose(io_lib:format("read tcp client->server ~p ~n",[Data])),
            Log("from client(c->s): ~p", [Data]),
            {Res, Ret} = fuzz(Proto, ProbToServer, Opts, Data),
            Verbose(io_lib:format("wrote tcp client->server ~p (fuzzing = ~p) ~n",[Ret, Res])),
            gen_tcp:send(ServerSocket, Ret),
            Log("from fuzzer(c->s) [~p]: ~p", [Res, Ret]),
            loop_tcp(Proto, ClientSocket, ServerSocket, Opts, Verbose, Log);
        {tcp, ServerSocket, Data} ->
            Verbose(io_lib:format("read tcp server->client ~p ~n", [Data])),
            Log("from server(s->c): ~p", [Data]),
            {Res, Ret} = fuzz(Proto, ProbToClient, Opts, Data),
            Verbose(io_lib:format("wrote tcp server->client ~p (fuzzing = ~p) ~n", [Ret, Res])),
            gen_tcp:send(ClientSocket, Ret),
            Log("from fuzzer(s->c) [~p]: ~p", [Res, Ret]),
            loop_tcp(Proto, ClientSocket, ServerSocket, Opts, Verbose, Log);
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

%% TODO: Stubs
extract_http(Data) ->
    {"POST", [], Data}.
pack_http(_Query, _Hdr, Data) ->
    Data.

fuzz(Proto, Prob, Opts, Data) ->
    case maps:get(external, Opts, nil) of
        nil -> fuzz(Proto, Prob, erlamsa_rnd:rand_float(), Opts, Data);
        Module -> 
            erlang:apply(list_to_atom(Module), fuzzer, [Proto, Data, maps:put(fuzzprob, Prob, Opts)])
    end.

fuzz(_, Prob, Rnd, _Opts, Data) when Rnd >= Prob -> {nofuzz, Data};
fuzz(http, Prob, _Rnd, Opts, Data) ->
    {Query, HTTPHeaders, HTTPData} = extract_http(Data),
    {ok, pack_http(Query, HTTPHeaders, call_fuzzer(Prob, Opts, HTTPData))};
fuzz(_Proto, Prob, _Rnd, Opts, Data) ->
    {ok, call_fuzzer(Prob, Opts, Data)}.

call_fuzzer(_Prob, Opts, Data) ->
    NewOpts = maps:put(paths, [direct],
               maps:put(output, return,
                maps:put(input, Data,
                  Opts))),
    erlamsa_utils:extract_function(erlamsa_main:fuzzer(NewOpts)).
