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
-export([get_supervisor_opts/1, start/1, start_fuzzproxy/1, server_tcp/4, loop_udp/7]).

get_supervisor_opts(Opts) ->
	#{id => ?MODULE,
	start => {?MODULE, start, [Opts]},
	restart => permanent,
	shutdown => brutal_kill,
	type => worker,
	modules => [?MODULE]}.

start(Opts) -> 
    Pid = spawn(?MODULE, start_fuzzproxy, [Opts]),
    {ok, Pid}.

listen(http, LocalPort) ->
    listen(tcp, LocalPort);
listen(tcp, LocalPort) ->
    gen_tcp:listen(LocalPort, [binary, {active, true}, {reuseaddr, true}, {packet,0}]);
listen(udp, LocalPort) ->
    gen_udp:open(LocalPort, [binary, {active, true}, {reuseaddr, true}]);
listen(_, _P) ->
    {error, "Unsupported fuzzing layer."}.

start_fuzzproxy(Opts) ->
    Workers = maps:get(workers, Opts, 10),
    Verbose = erlamsa_utils:verb(stdout, maps:get(verbose, Opts, 0)),
    ProxyAddrList = maps:get(proxy_address, Opts),
    lists:map( 
        fun(L) ->
            {StrProto, LocalPort, _, _, _} = L,
            Proto = list_to_atom(StrProto),
            case listen(Proto, LocalPort) of
                {ok, ListenSock} ->
                    {ok, Port} = inet:port(ListenSock),
                    start_servers(transport(Proto), Workers, ListenSock, L, Opts, Verbose),
                    Port;
                {error,Reason} ->
                    io:format("~s", [Reason]),
                    {error,Reason}
            end
        end, ProxyAddrList),
    timer:sleep(infinity).

transport(http) -> tcp;
transport(Tr) -> Tr.

start_servers(tcp, Workers, ListenSock, Endpoint, Opts, Verbose) ->
    length([spawn(?MODULE, server_tcp, [ListenSock, Endpoint, Opts, Verbose]) || _N <- lists:seq(1, Workers)]);
start_servers(udp, _Workers, ListenSock, Endpoint, Opts, Verbose) ->
    erlamsa_logger:log(info, "udp proxy worker process started, socket id ~p, bind to ~s:~d", [ListenSock]),
    Pid = spawn(?MODULE, loop_udp, [ListenSock, Endpoint, init_clientsocket, [], 0, Opts, Verbose]),
    gen_udp:controlling_process(ListenSock, Pid).

server_tcp(ListenSock, Endpoint, Opts, Verbose) ->
    {Proto, LPort, _, DHost, DPort} = Endpoint,
    {ProbToClient, ProbToServer} = maps:get(proxy_probs, Opts, {0.0, 0.0}),
    ByPass = maps:get(bypass, Opts, 0),
    DescentCoeff = maps:get(descent_coeff, Opts, 1),
    erlamsa_logger:log(info, "tcp proxy worker process started, listening on ~p (port :~p)", [ListenSock, LPort]),
    random:seed(now()),
    case gen_tcp:accept(ListenSock) of
        {ok, ClientSocket} ->
            erlamsa_logger:log(info, "initiating new connection to ~s:~p(c->s), sockets: l:~p/c:~p", [inet:ntoa(DHost), DPort, ListenSock, ClientSocket]),
            case gen_tcp:connect(DHost, DPort,
				 [binary, {packet,0}, {active, true}]) of
		          {ok, ServerSocket} ->
		              loop_tcp(list_to_atom(Proto), ClientSocket, ServerSocket, 
                        {ProbToClient/DescentCoeff, ProbToServer/DescentCoeff, DescentCoeff}, 
                        {ByPass, 0,0},
                        Opts, Verbose),
                      spawn(?MODULE, server_tcp, [ListenSock, Endpoint, Opts, Verbose]);
		          E ->
		              io:format("Error: connect to server failed!~n"),
                      erlamsa_logger:log(info, "error: worker connect to server ~s:~p failed!", [inet:ntoa(DHost), DPort]),
		          E
		      end,
		  ok;
        Other ->
            io:format("Error: accept returned ~w~n",[Other]),
            erlamsa_logger:log(info, "error: accept returned ~p", [Other]),
            ok
    end.

raise_prob(0.0, _) -> 0.0;
raise_prob(Prob, 1.0) -> Prob;
raise_prob(Prob, DC) -> 
    erlamsa_logger:log(debug, "increasing Prob from ~p to ~p", [Prob, Prob + Prob/DC]),
    Prob + Prob/DC.

%%TODO: check for memory consumption and tail recursion correctness
%%FIXME: fuzzing for multiple endpoints?
loop_udp(SrvSocket, Endpoint, init_clientsocket, ClientHost, ClientPort, Opts, Verbose) ->
    random:seed(now()),
    {"udp", _LPort, ClSocketPort, _, _} = Endpoint,
    {ok, ClSocket} = listen(udp, ClSocketPort),
    gen_udp:controlling_process(ClSocket, self()),
    loop_udp(SrvSocket, ClSocket, Endpoint, ClientHost, ClientPort, Opts, Verbose);
loop_udp(SrvSocket, ClSocket, Endpoint, ClientHost, ClientPort, Opts, Verbose) ->
    {"udp", _, _, ServerHost, ServerPort} = maps:get(proxy_address, Opts),
    {ProbToClient, ProbToServer} = maps:get(proxy_probs, Opts, {0.0, 0.0}),
    {NewHost, NewPort} = 
        receive
            {udp, SrvSocket, Host, Port, Data} ->
                erlamsa_logger:log_data(info, "from udp client(c->s ~p:~p)",[Host, Port], Data),
                {Res, Ret} = fuzz(udp, ProbToServer, Opts, Data),
                erlamsa_logger:log_data(info, "from fuzzer(c->s) [fuzzing = ~p]", [Res], Ret),
                gen_udp:send(ClSocket, ServerHost, ServerPort, Ret),
                {Host, Port};
            {udp, ClSocket, ServerHost, ServerPort, Data} ->
                erlamsa_logger:log_data(info, "from udp server(s->c, ~p:~p)",[ServerHost, ServerPort], Data),
                case ClientHost of
                    [] ->
                        {[], 0};
                    _Else ->
                        {Res, Ret} = fuzz(udp, ProbToClient, Opts, Data),
                        gen_udp:send(SrvSocket, ClientHost, ClientPort, Ret),
                        erlamsa_logger:log_data(info, "from fuzzer(c->s) [fuzzing = ~p]",[Res], Ret),
                        {ClientHost, ClientPort}
                end
        end,
    loop_udp(SrvSocket, ClSocket, Endpoint, NewHost, NewPort, Opts, Verbose).

loop_tcp(Proto, ClientSocket, ServerSocket, {ProbToClient, ProbToServer, DescentCoeff}, {ByPass, NC, NS}, Opts, Verbose) ->
    inet:setopts(ClientSocket, [{active,once}]),
    receive
        {tcp, ClientSocket, Data} ->
	        erlamsa_logger:log_data(info, "from client(c->s)", [], Data),
            {Res, Ret} = fuzz(Proto, ProbToServer, ByPass, NC, Opts, Data),
            gen_tcp:send(ServerSocket, Ret),
            erlamsa_logger:log_data(info, "from fuzzer(c->s) [fuzzing = ~p]", [Res], Ret),
            loop_tcp(Proto, ClientSocket, ServerSocket, 
                    {ProbToClient, raise_prob(ProbToServer, DescentCoeff),  DescentCoeff}, {ByPass, NC+1, NS},
                    Opts, Verbose);
        {tcp, ServerSocket, Data} ->
            erlamsa_logger:log_data(info, "from server(s->c)", [], Data),
            {Res, Ret} = fuzz(Proto, ProbToClient, ByPass, NS, Opts, Data),
            gen_tcp:send(ClientSocket, Ret),
            erlamsa_logger:log_data(info, "from fuzzer(s->c) [fuzzing = ~p]", [Res], Ret),
            loop_tcp(Proto, ClientSocket, ServerSocket, 
                    {raise_prob(ProbToClient, DescentCoeff), ProbToServer, DescentCoeff}, {ByPass, NC, NS+1},
                    Opts, Verbose);
        {tcp_closed, ClientSocket} ->
            gen_tcp:close(ServerSocket),
            erlamsa_logger:log(info, "client close (c->s), c:~p ", [ClientSocket]),
            ok;
        {tcp_closed, ServerSocket}->
            gen_tcp:close(ClientSocket),
            erlamsa_logger:log(info, "server close (s->c)m l:~p/c:~p ", [ServerSocket, ClientSocket]),
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

fuzz(_Proto, _Prob, ByPass, N, _Opts, Data) when N < ByPass ->
    erlamsa_logger:log(debug, "Packet No. ~p < ~p, bypassing data", [N, ByPass]),
    {nofuzz, Data};
fuzz(Proto, Prob, _ByPass, _N, Opts, Data) ->
    fuzz(Proto, Prob, Opts, Data).

fuzz(Proto, Prob, Opts, Data) ->
    case maps:get(external, Opts, nil) of
        nil -> fuzz_rnd(Proto, Prob, erlamsa_rnd:rand_float(), Opts, Data);
        Module -> 
            erlang:apply(list_to_atom(Module), fuzzer, [Proto, Data, maps:put(fuzzprob, Prob, Opts)])
    end.

fuzz_rnd(_, Prob, Rnd, _Opts, Data) when Rnd >= Prob -> {nofuzz, Data};
fuzz_rnd(http, Prob, _Rnd, Opts, Data) -> 
    {Status, HTTPHeaders, HTTPData} = extract_http(Data),
    {ok, pack_http(Status, HTTPHeaders, call_fuzzer(Prob, Opts, HTTPData))};
fuzz_rnd(_Proto, Prob, _Rnd, Opts, Data) ->
    {ok, call_fuzzer(Prob, Opts, Data)}.

call_fuzzer(_Prob, Opts, Data) ->
    NewOpts = maps:put(paths, [direct],
               maps:put(output, return,
                maps:put(input, Data,
                  Opts))),
    erlamsa_fsupervisor:get_fuzzing_output(NewOpts).
