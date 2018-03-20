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
    Log("udp proxy worker process started, socket id ~p", [ListenSock]),
    Pid = spawn(?MODULE, loop_udp, [ListenSock, init_clientsocket, [], 0, Opts, Verbose, Log]),
    gen_udp:controlling_process(ListenSock, Pid).

start_tcp_servers(0, _ListenSock, _Opts, _Verbose, _Log) -> ok;
start_tcp_servers(Workers, ListenSock, Opts, Verbose, Log) ->
    spawn(?MODULE, server_tcp,[ListenSock, Opts, Verbose, Log]),
    start_tcp_servers(Workers - 1, ListenSock, Opts, Verbose, Log).

server_tcp(ListenSock, Opts, Verbose, Log) ->
    {Proto, _, _, DHost, DPort} = maps:get(proxy_address, Opts),
    Log("tcp proxy worker process started", []),
    random:seed(now()),
    case gen_tcp:accept(ListenSock) of
        {ok, ClientSocket} ->
            Log("new connect(c->s)", []),
            case gen_tcp:connect(DHost, DPort,
				 [binary, {packet,0}, {active, true}]) of
		          {ok, ServerSocket} ->
		              loop_tcp(list_to_atom(Proto), ClientSocket, ServerSocket, Opts, Verbose, Log),
                      server_tcp(ListenSock, Opts, Verbose, Log);
		          E ->
		              io:format("Error: connect to server failed!~n"),
                      Log("error: worker connect to server failed!", []),
		          E
		      end,
		  ok;
        Other ->
            io:format("Error: accept returned ~w~n",[Other]),
            Log("error: accept returned ~p", [Other]),
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
            Log("read udp client->server ~p ~p ~n",[Data, Msg]),
            {_, Ret} = fuzz(udp, ProbToServer, Opts, Data),
            Log("wrote udp client->server ~p ~n",[Ret]),
            gen_udp:send(ClSocket, ServerHost, ServerPort, Ret),
            loop_udp(SrvSocket, ClSocket, Host, Port, Opts, Verbose, Log);
        {udp, ClSocket, ServerHost, ServerPort, Data} = Msg ->
            Log("read udp server:->client ~p ~p ~n",[Data, Msg]),
            case ClientHost of
                [] ->
                    loop_udp(SrvSocket, ClSocket, [], 0, Opts, Verbose, Log);
                _Else ->
                    {_, Ret} = fuzz(udp, ProbToClient, Opts, Data),
                    gen_udp:send(SrvSocket, ClientHost, ClientPort, Ret),
                    Log("wrote udp server->client ~p ~n",[Ret]),
                    loop_udp(SrvSocket, ClSocket, ClientHost, ClientPort, Opts, Verbose, Log)
            end
    end.


loop_tcp(Proto, ClientSocket, ServerSocket, Opts, Verbose, Log) ->
    {ProbToClient, ProbToServer} = maps:get(proxy_probs, Opts, {0.0, 0.0}),
    inet:setopts(ClientSocket, [{active,once}]),
    receive
        {tcp, ClientSocket, Data} ->
	        Log("from client(c->s): ~p", [Data]),
            {Res, Ret} = fuzz(Proto, ProbToServer, Opts, Data),
            gen_tcp:send(ServerSocket, Ret),
            Log("from fuzzer(c->s) [fuzzing = ~p]: ~p", [Res, Ret]),
            loop_tcp(Proto, ClientSocket, ServerSocket, Opts, Verbose, Log);
        {tcp, ServerSocket, Data} ->
            Log("from server(s->c): ~p", [Data]),
            {Res, Ret} = fuzz(Proto, ProbToClient, Opts, Data),
            gen_tcp:send(ClientSocket, Ret),
            Log("from fuzzer(s->c) [fuzzing = ~p]: ~p", [Res, Ret]),
            loop_tcp(Proto, ClientSocket, ServerSocket, Opts, Verbose, Log);
        {tcp_closed, ClientSocket} ->
            gen_tcp:close(ServerSocket),
            Log("client close (c->s)", []),
            ok;
        {tcp_closed, ServerSocket}->
            gen_tcp:close(ClientSocket),
            Log("server close (s->c)", []),
            ok
    end.


extract_http(Data, Log) ->
    case erlang:decode_packet(http, Data, []) of 
        {ok, Query, Rest} ->
            extract_http_headers(Rest, Log, [Query]);
        Err ->
            Log("Invalid HTTP packet?: ~p~n", [Err]),
            {ok, [], Data}
    end.
extract_http_headers(Data, Log, Acc) ->
    io:format("Incoming:~p~n", [Data]),
    case erlang:decode_packet(httph, Data, []) of
        {ok, http_eoh, Rest} ->
            {ok, Acc, Rest};
        {ok, Hdr, Rest} ->
            extract_http_headers(Rest, Log, [Hdr | Acc]);
        {more, undefined} ->
            {more, Acc, Data};
        Err ->
            Log("Error parsing HTTP header: ~p~n", [Err]),
            {ok, Acc, Data}
    end.

pack_http(more, Headers, Data, Log) ->
    pack_http_packet(Headers, Data, Log, []);
pack_http(ok, Headers, Data, Log) ->
    pack_http_packet(Headers, Data, Log, [[10, 13]]).

pack_http_packet([{http_header, _, 'Content-Length', _, _}|T], Data, Log, Acc) ->
    Len = size(Data),
    pack_http_packet(T, Data, Log, [list_to_binary(io_lib:format("Content-Length: ~p~c~n", [Len, 13])) | Acc]);
pack_http_packet([{http_header, _, HdrName, _, HdrValue}|T], Data, Log, Acc) ->
    pack_http_packet(T, Data, Log, [list_to_binary(io_lib:format("~s: ~s~c~n", [atom_to_list(HdrName), HdrValue, 13])) | Acc]);
pack_http_packet([{http_response, {VerMajor, VerMinor}, Code, Status}|T], Data, Log, Acc) ->
    pack_http_packet(T, Data, Log, [list_to_binary(io_lib:format("HTTP ~p.~p ~p ~s~c~n", [VerMajor, VerMinor, Code, Status, 13])) | Acc]);
pack_http_packet([{http_error, ErrHdr}|T], Data, Log, Acc) ->
    pack_http_packet(T, Data, Log, [ErrHdr | Acc]);   
pack_http_packet([], Data, Log, Acc) ->
    Hdr = list_to_binary(Acc),
    <<Hdr/binary, Data/binary>>.

fuzz(Proto, Prob, Opts, Data) ->
    case maps:get(external, Opts, nil) of
        nil -> fuzz(Proto, Prob, erlamsa_rnd:rand_float(), Opts, Data);
        Module -> 
            erlang:apply(list_to_atom(Module), fuzzer, [Proto, Data, maps:put(fuzzprob, Prob, Opts)])
    end.

fuzz(_, Prob, Rnd, _Opts, Data) when Rnd >= Prob -> {nofuzz, Data};
fuzz(http, Prob, _Rnd, Opts, Data) -> 
    Log = erlamsa_logger:build_logger(Opts),
    {Status, HTTPHeaders, HTTPData} = extract_http(Data, Log),
    {ok, pack_http(Status, HTTPHeaders, call_fuzzer(Prob, Opts, HTTPData), Log)};
fuzz(_Proto, Prob, _Rnd, Opts, Data) ->
    {ok, call_fuzzer(Prob, Opts, Data)}.

call_fuzzer(_Prob, Opts, Data) ->
    NewOpts = maps:put(paths, [direct],
               maps:put(output, return,
                maps:put(input, Data,
                  Opts))),
    erlamsa_utils:extract_function(erlamsa_main:fuzzer(NewOpts)).
