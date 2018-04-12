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
-export([start/1, server_tcp/3, loop_udp/6]).

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
    Workers = maps:get(workers, Opts, 10),
    Verbose = erlamsa_utils:verb(stdout, maps:get(verbose, Opts, 0)),
    {StrProto, LocalPort, _, _, _} = maps:get(proxy_address, Opts),
    Proto = list_to_atom(StrProto),
    case listen(Proto, LocalPort) of
        {ok, ListenSock} ->
            start_servers(transport(Proto), Workers, ListenSock, Opts, Verbose),
            {ok, Port} = inet:port(ListenSock),
            Port;
        {error,Reason} ->
            io:format("~s", [Reason]),
            {error,Reason}
    end.

transport(http) -> tcp;
transport(Tr) -> Tr.

start_servers(tcp, Workers, ListenSock, Opts, Verbose) ->
    start_tcp_servers(Workers, ListenSock, Opts, Verbose);
start_servers(udp, _Workers, ListenSock, Opts, Verbose) ->
    erlamsa_logger:log(info, "udp proxy worker process started, socket id ~p", [ListenSock]),
    Pid = spawn(?MODULE, loop_udp, [ListenSock, init_clientsocket, [], 0, Opts, Verbose]),
    gen_udp:controlling_process(ListenSock, Pid).

start_tcp_servers(0, _ListenSock, _Opts, _Verbose) -> ok;
start_tcp_servers(Workers, ListenSock, Opts, Verbose) ->
    spawn(?MODULE, server_tcp,[ListenSock, Opts, Verbose]),
    start_tcp_servers(Workers - 1, ListenSock, Opts, Verbose).

server_tcp(ListenSock, Opts, Verbose) ->
    {Proto, _, _, DHost, DPort} = maps:get(proxy_address, Opts),
    erlamsa_logger:log(info, "tcp proxy worker process started", []),
    random:seed(now()),
    case gen_tcp:accept(ListenSock) of
        {ok, ClientSocket} ->
            erlamsa_logger:log(info, "new connect(c->s)", []),
            case gen_tcp:connect(DHost, DPort,
				 [binary, {packet,0}, {active, true}]) of
		          {ok, ServerSocket} ->
		              loop_tcp(list_to_atom(Proto), ClientSocket, ServerSocket, Opts, Verbose),
                      server_tcp(ListenSock, Opts, Verbose);
		          E ->
		              io:format("Error: connect to server failed!~n"),
                      erlamsa_logger:log(info, "error: worker connect to server failed!", []),
		          E
		      end,
		  ok;
        Other ->
            io:format("Error: accept returned ~w~n",[Other]),
            erlamsa_logger:log(info, "error: accept returned ~p", [Other]),
            ok
    end.

loop_udp(SrvSocket, init_clientsocket, ClientHost, ClientPort, Opts, Verbose) ->
    random:seed(now()),
    {"udp", _LPort, ClSocketPort, _, _} = maps:get(proxy_address, Opts),
    {ok, ClSocket} = listen(udp, ClSocketPort),
    gen_udp:controlling_process(ClSocket, self()),
    loop_udp(SrvSocket, ClSocket, ClientHost, ClientPort, Opts, Verbose);
loop_udp(SrvSocket, ClSocket, ClientHost, ClientPort, Opts, Verbose) ->
    {"udp", _, _, ServerHost, ServerPort} = maps:get(proxy_address, Opts),
    {ProbToClient, ProbToServer} = maps:get(proxy_probs, Opts, {0.0, 0.0}),
    receive
        {udp, SrvSocket, Host, Port, Data} ->
            erlamsa_logger:log_data(info, "from udp client(c->s ~p:~p)",[Host, Port], Data),
            {Res, Ret} = fuzz(udp, ProbToServer, Opts, Data),
            erlamsa_logger:log_data(info, "from fuzzer(c->s) [fuzzing = ~p]", [Res], Ret),
            gen_udp:send(ClSocket, ServerHost, ServerPort, Ret),
            loop_udp(SrvSocket, ClSocket, Host, Port, Opts, Verbose);
        {udp, ClSocket, ServerHost, ServerPort, Data} ->
            erlamsa_logger:log_data(info, "from udp server(s->c, ~p:~p)",[ServerHost, ServerPort], Data),
            case ClientHost of
                [] ->
                    loop_udp(SrvSocket, ClSocket, [], 0, Opts, Verbose);
                _Else ->
                    {Res, Ret} = fuzz(udp, ProbToClient, Opts, Data),
                    gen_udp:send(SrvSocket, ClientHost, ClientPort, Ret),
                    erlamsa_logger:log_data(info, "from fuzzer(c->s) [fuzzing = ~p]",[Res], Ret),
                    loop_udp(SrvSocket, ClSocket, ClientHost, ClientPort, Opts, Verbose)
            end
    end.


loop_tcp(Proto, ClientSocket, ServerSocket, Opts, Verbose) ->
    {ProbToClient, ProbToServer} = maps:get(proxy_probs, Opts, {0.0, 0.0}),
    inet:setopts(ClientSocket, [{active,once}]),
    receive
        {tcp, ClientSocket, Data} ->
	        erlamsa_logger:log_data(info, "from client(c->s)", [], Data),
            {Res, Ret} = fuzz(Proto, ProbToServer, Opts, Data),
            gen_tcp:send(ServerSocket, Ret),
            erlamsa_logger:log_data(info, "from fuzzer(c->s) [fuzzing = ~p]", [Res], Ret),
            loop_tcp(Proto, ClientSocket, ServerSocket, Opts, Verbose);
        {tcp, ServerSocket, Data} ->
            erlamsa_logger:log_data(info, "from server(s->c): ~p", [Data]),
            {Res, Ret} = fuzz(Proto, ProbToClient, Opts, Data),
            gen_tcp:send(ClientSocket, Ret),
            erlamsa_logger:log_data(info, "from fuzzer(s->c) [fuzzing = ~p]", [Res], Ret),
            loop_tcp(Proto, ClientSocket, ServerSocket, Opts, Verbose);
        {tcp_closed, ClientSocket} ->
            gen_tcp:close(ServerSocket),
            erlamsa_logger:log(info, "client close (c->s)", []),
            ok;
        {tcp_closed, ServerSocket}->
            gen_tcp:close(ClientSocket),
            erlamsa_logger:log(info, "server close (s->c)", []),
            ok
    end.


extract_http(Data) ->
    case erlang:decode_packet(http, Data, []) of 
        {ok, Query, Rest} ->
            extract_http_headers(Rest, [Query]);
        Err ->
            erlamsa_logger:log(info, "Invalid HTTP packet?: ~p~n", [Err]),
            {ok, [], Data}
    end.
extract_http_headers(Data, Acc) ->
    %%io:format("Incoming:~p~n", [Data]),
    case erlang:decode_packet(httph, Data, []) of
        {ok, http_eoh, Rest} ->
            {ok, Acc, Rest};
        {ok, Hdr, Rest} ->
            extract_http_headers(Rest, [Hdr | Acc]);
        {more, undefined} ->
            {more, Acc, Data};
        Err ->
            erlamsa_logger:log(info, "Error parsing HTTP header: ~p~n", [Err]),
            {ok, Acc, Data}
    end.

pack_http(more, Headers, Data) ->
    pack_http_packet(Headers, Data, []);
pack_http(ok, Headers, Data) ->
    pack_http_packet(Headers, Data, [[10, 13]]).

pack_http_packet([{http_header, _, 'Content-Length', _, _}|T], Data, Acc) ->
    Len = size(Data),
    pack_http_packet(T, Data, [list_to_binary(io_lib:format("Content-Length: ~p~c~n", [Len, 13])) | Acc]);
pack_http_packet([{http_header, _, HdrName, _, HdrValue}|T], Data, Acc) ->
    pack_http_packet(T, Data, [list_to_binary(io_lib:format("~s: ~s~c~n", [atom_to_list(HdrName), HdrValue, 13])) | Acc]);
pack_http_packet([{http_response, {VerMajor, VerMinor}, Code, Status}|T], Data, Acc) ->
    pack_http_packet(T, Data, [list_to_binary(io_lib:format("HTTP ~p.~p ~p ~s~c~n", [VerMajor, VerMinor, Code, Status, 13])) | Acc]);
pack_http_packet([{http_error, ErrHdr}|T], Data, Acc) ->
    pack_http_packet(T, Data, [ErrHdr | Acc]);   
pack_http_packet([], Data, Acc) ->
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
    {Status, HTTPHeaders, HTTPData} = extract_http(Data),
    {ok, pack_http(Status, HTTPHeaders, call_fuzzer(Prob, Opts, HTTPData))};
fuzz(_Proto, Prob, _Rnd, Opts, Data) ->
    {ok, call_fuzzer(Prob, Opts, Data)}.

call_fuzzer(_Prob, Opts, Data) ->
    NewOpts = maps:put(paths, [direct],
               maps:put(output, return,
                maps:put(input, Data,
                  Opts))),
    erlamsa_utils:extract_function(erlamsa_main:fuzzer(NewOpts)).
