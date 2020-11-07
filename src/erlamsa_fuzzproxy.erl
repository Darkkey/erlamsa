% idea and part of original code from http://beezari.livejournal.com/191194.html
% Heavily updated and rewited:
% Copyright (c) 2014-2019 Alexander Bolshev aka dark_k3y
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
%%% TCP/HTTP/UDP Fuzzing proxy.
%%% @end
%%%-------------------------------------------------------------------

-module(erlamsa_fuzzproxy).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

-include("erlamsa.hrl").

% API
-export([get_supervisor_opts/1, start/1, start_fuzzproxy/1, server_stream/5, loop_udp/7, loop_serial/5]).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Fuzzing probability helper functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_proxy_probs(Opts) ->
    maps:get(proxy_probs, Opts, {0.0, 0.0}).

raise_prob(0.0, _) -> 0.0;
raise_prob(Prob, 1.0) -> Prob;
raise_prob(Prob, DC) ->
    erlamsa_logger:log(decision, "increasing Prob from ~p to ~p", [Prob, Prob + Prob/DC]),
    Prob + Prob/DC.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Proxy code:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
proxy_listen(udp, Port, _Dict) -> erlamsa_netutils:listen(udp, Port,
                            [binary, {active, true}, {reuseaddr, true}]);
proxy_listen(ssl, Port, Dict) -> erlamsa_netutils:listen(ssl, Port, 
                            [binary, {reuseaddr, true}, {active, true}, {packet, 0},
                            {certfile, maps:get(certfile, Dict, "cert.pem")}, 
                            {keyfile, maps:get(keyfile, Dict, "key.pem")}]);
proxy_listen(tcp, Port, _Dict) -> erlamsa_netutils:listen(tcp, Port, 
                            [binary, {reuseaddr, true}, {active, true}, {packet, 0}]).

bitrate_to_maxdelay(Speed) -> 
    trunc(math:ceil(Speed / 8 / 1000)).

start_fuzzproxy(Opts) ->
    Verbose = erlamsa_utils:verb(stdout, maps:get(verbose, Opts, 0)),
    ProxyAddrList = maps:get(proxy_address, Opts),
    lists:map(
        fun(L) ->
            {StrProto, _, _, _, _} = L,
            case list_to_atom(StrProto) of
                serial -> 
                    {StrProto, Port1, Speed1Str, Port2, Speed2Str} = L,
                    Speed1 = list_to_integer(Speed1Str), 
                    Speed2 = list_to_integer(Speed2Str), 
                    start_servers(serial, 1, {Port1, Port2}, 
                        {Speed1, Speed2}, Opts, Verbose);
                Proto ->
                    {StrProto, LocalPort, _, DHost, DPort} = L,
                    Workers = maps:get(workers, Opts, 10),
                    erlamsa_netutils:set_routing_ip(tcp, DHost, DPort),
                    ProtoTransport = erlamsa_netutils:transport(Proto),
                    erlamsa_netutils:netserver_start(ProtoTransport, maps:get(httpproxy, Opts, false)),
                    case proxy_listen(ProtoTransport, LocalPort, Opts) of
                        {ok, ListenSock} ->
                            {ok, Port} = erlamsa_netutils:port(ProtoTransport, ListenSock),
                            start_servers(ProtoTransport, Workers, ListenSock, L, Opts, Verbose),
                            Port;
                        {error, Reason} ->
                            io:format("Error: ~s", [Reason]),
                            {error, Reason}
                    end
                end
        end, ProxyAddrList),
    timer:sleep(infinity).

spawn_stream_worker(ProtoTransport, ListenSock, Endpoint, Opts, Verbose) ->
    spawn(?MODULE, server_stream, [ProtoTransport, ListenSock, Endpoint, Opts, Verbose]).

start_servers(serial, _Workers, Ports, Delay, Opts, Verbose) ->
    erlamsa_logger:log(info, "serial proxy worker process started, socket ids ~p", [Ports]),
    spawn(?MODULE, loop_serial, [init_ports, Ports, Delay, Opts, Verbose]);
start_servers(udp, _Workers, ListenSock, Endpoint, Opts, Verbose) ->
    erlamsa_logger:log(info, "udp proxy worker process started, socket id ~p",
                        [ListenSock]),
    Pid = spawn(?MODULE, loop_udp, [ListenSock, Endpoint, init_clientsocket, [], 0, Opts, Verbose]),
    gen_udp:controlling_process(ListenSock, Pid);
start_servers(ProtoTransport, Workers, ListenSock, Endpoint, Opts, Verbose) ->
    length([spawn_stream_worker(ProtoTransport, ListenSock, Endpoint, Opts, Verbose) || _N <- lists:seq(1, Workers)]).

server_stream(ProtoTransport, ListenSock, Endpoint, Opts, Verbose) ->
    {_, LPort, _, _, _} = Endpoint,
    StandaloneProxy = maps:get(httpproxy, Opts, false),
    erlamsa_logger:log(info, "tcp proxy worker process started, listening on ~p (port :~p)",
                        [erlamsa_netutils:socknum(ProtoTransport, ListenSock), LPort]),
    erlamsa_rnd:seed(now()),
    Res = erlamsa_netutils:accept(ProtoTransport, ListenSock),
    stream_setup_connection(StandaloneProxy, Res, ProtoTransport, ListenSock, Endpoint, Opts, Verbose, <<>>),
    spawn_stream_worker(ProtoTransport, ListenSock, Endpoint, Opts, Verbose).

stream_setup_connection(true, {ok, ClientSocket}, ProtoTransport, ListenSock, {Proto, LHost, LPort, _, _}, Opts, Verbose, _) ->
    erlamsa_netutils:setopts(ProtoTransport, ClientSocket, [{active, once}]),
    receive
        {ProtoTransport, ClientSocket, Data} ->
            case erlang:decode_packet(http, Data, []) of
                {ok,{http_request,"CONNECT",{scheme,THost,TPort},_},_} ->
                    erlamsa_logger:log(info, "standalone HTTP proxy, establishing connection to ~p://~s:~s(c->s), socket ~p",
                             [ssl, THost, TPort, erlamsa_netutils:socknum(ProtoTransport, ListenSock)]),
                    erlamsa_netutils:send(ProtoTransport, ClientSocket, <<"HTTP/1.0 200 Connection established", 13, 10, 13, 10>>),
                    erlamsa_netutils:setopts(ProtoTransport, ClientSocket, [{active, false}]),
                    {ok, TLSSocket} = ssl:handshake(ClientSocket, 
                                                    [{certfile, maps:get(certfile, Opts, "cert.pem")}, 
                                                     {keyfile, maps:get(keyfile, Opts, "key.pem")}]),
                    erlamsa_logger:log(info, "standalone HTTP proxy, upgraded connection socket ~p to TLS socket ~p",
                             [erlamsa_netutils:socknum(ProtoTransport, ListenSock), erlamsa_netutils:socknum(ssl, TLSSocket)]),
                    stream_setup_connection(false, {ok, TLSSocket}, ssl, 
                                                    ListenSock, {Proto, LHost, LPort, THost, list_to_integer(TPort)}, Opts, Verbose, <<>>);
                {ok,{http_request,_,{absoluteURI,http,THost,MayBeTPort,"/"},_},_} ->
                    TPort = erlamsa_netutils:get_default_http_port(MayBeTPort),
                    stream_setup_connection(false, {ok, ClientSocket}, ProtoTransport, 
                                                    ListenSock, {Proto, LHost, LPort, THost, TPort}, Opts, Verbose, Data);
                _Else -> %%some error
                    ok
            end            
    end;
stream_setup_connection(false, {ok, ClientSocket}, ProtoTransport, ListenSock, {Proto, _, _, DHost, DPort}, Opts, Verbose, OptData) ->
    {ProbToClient, ProbToServer} = get_proxy_probs(Opts),
    ByPass = maps:get(bypass, Opts, 0),
    DescentCoeff = maps:get(descent_coeff, Opts, 1),
    erlamsa_logger:log(info, "initiating new connection to ~p://~s:~p(c->s), sockets: l:~p/c:~p",
                             [ProtoTransport, erlamsa_netutils:host2str(DHost), DPort, 
                                erlamsa_netutils:socknum(ProtoTransport, ListenSock), 
                                erlamsa_netutils:socknum(ProtoTransport, ClientSocket)]),
    case erlamsa_netutils:connect(ProtoTransport, DHost, DPort, 
                                    [binary, {packet, 0}, {active, true}]) of
            {ok, ServerSocket} ->  
                case OptData of  %% if we're in standalone proxy mode and have some-prerecived data
                    <<>> -> ok;
                    ElseData -> 
                        erlamsa_logger:log_data(info, "from client(c->s)", [], ElseData),
                        erlamsa_netutils:send(ProtoTransport, ServerSocket, ElseData)
                end, 
                loop_stream(list_to_atom(Proto), ProtoTransport, ClientSocket, ServerSocket,
                {ProbToClient/DescentCoeff, ProbToServer/DescentCoeff, DescentCoeff},
                {ByPass, 0, 0},
                Opts, Verbose);
            E ->
                io:format("Error: connect to server failed!~n"),
                erlamsa_logger:log(info, "error: worker connect to server ~s:~p failed!",
                                        [erlamsa_netutils:host2str(DHost), DPort]),
            E
    end;
stream_setup_connection(_, Err,_,_,_,_,_,_) ->
    erlamsa_logger:log(warning, "error: accept returned ~p", [Err]).
            


report_fuzzing(nofuzz, Direction, Ret) ->
    erlamsa_logger:log_data(debug, "from fuzzer(~s) [fuzzing = nofuzz, skipped]", [Direction], Ret);
report_fuzzing(Res, Direction, Ret) ->
    erlamsa_logger:log_data(info, "from fuzzer(~s) [fuzzing = ~p]", [Direction, Res], Ret).

loop_serial(init_ports, {Serial1, Serial2}, {Speed1, Speed2}, Opts, Verbose) ->
    SerialPid1 = serial:start([{serialexe, erlamsa_utils:get_portsdir() ++ "erlserial"} | [{open, Serial1}, {speed, Speed1}]]),
    SerialPid2 = serial:start([{serialexe, erlamsa_utils:get_portsdir() ++ "erlserial"} | [{open, Serial2}, {speed, Speed2}]]),
    loop_serial(running, {SerialPid1, SerialPid2}, 
                         lists:min([bitrate_to_maxdelay(Speed1), bitrate_to_maxdelay(Speed2)]), 
                         Opts, Verbose);    
loop_serial(_State, {SerialPid1, SerialPid2}, Delay, Opts, Verbose) ->
    {ProbToClient, ProbToServer} = get_proxy_probs(Opts),
    receive
        {data, SerialPid1, Bytes} ->
            erlamsa_logger:log_data(info, "from serial(1->2)", [], Bytes),
            {Res, Ret} = fuzz(udp, ProbToClient, Opts, Bytes),
            report_fuzzing(Res, "1->2", Ret),
            SerialPid2 ! {send, Bytes};
        {data, SerialPid2, Bytes} ->
            erlamsa_logger:log_data(info, "from serial(2->1)", [], Bytes),
            {Res, Ret} = fuzz(udp, ProbToServer, Opts, Bytes),
            report_fuzzing(Res, "2->1", Ret),
            SerialPid1 ! {send, Bytes}
    after Delay -> 
            ok = ok    
    end,
    loop_serial(running, {SerialPid1, SerialPid2}, Delay, Opts, Verbose).

loop_udp(SrvSocket, Endpoint, init_clientsocket, ClientHost, ClientPort, Opts, Verbose) ->
    erlamsa_rnd:seed(now()),
    {"udp", _LPort, ClSocketPort, _, _} = Endpoint,
    {ok, ClSocket} = proxy_listen(udp, ClSocketPort, Opts),
    gen_udp:controlling_process(ClSocket, self()),
    loop_udp(SrvSocket, ClSocket, Endpoint, ClientHost, ClientPort, Opts, Verbose);
loop_udp(SrvSocket, ClSocket, Endpoint, ClientHost, ClientPort, Opts, Verbose) ->
    {"udp", _, _, ServerHost, ServerPort} = Endpoint,
    {ProbToClient, ProbToServer} = get_proxy_probs(Opts),
    {NewHost, NewPort} =
        receive
            {udp, SrvSocket, Host, Port, Data} ->
                erlamsa_logger:log_data(info, "from udp client(c->s ~p:~p)", [Host, Port], Data),
                {Res, Ret} = fuzz(udp, ProbToServer, Opts, Data),
                report_fuzzing(Res, "c->s", Ret),
                %% erlamsa_logger:log_data(info, "from fuzzer(c->s) [fuzzing = ~p]", [Res], Ret),
                gen_udp:send(ClSocket, ServerHost, ServerPort, Ret),
                {Host, Port};
            {udp, ClSocket, ServerHost, ServerPort, Data} ->
                erlamsa_logger:log_data(info, "from udp server(s->c, ~p:~p)",
                                        [ServerHost, ServerPort], Data),
                case ClientHost of
                    [] ->
                        {[], 0};
                    _Else ->
                        {Res, Ret} = fuzz(udp, ProbToClient, Opts, Data),
                        gen_udp:send(SrvSocket, ClientHost, ClientPort, Ret),
                        report_fuzzing(Res, "s->c", Ret),
                        %%erlamsa_logger:log_data(info, "from fuzzer(c->s) [fuzzing = ~p]",
                        %%                        [Res], Ret),
                        {ClientHost, ClientPort}
                end
        end,
    loop_udp(SrvSocket, ClSocket, Endpoint, NewHost, NewPort, Opts, Verbose).

loop_stream(Proto, ProtoTransport, ClientSocket, ServerSocket, {ProbToClient, ProbToServer, DescentCoeff},
        {ByPass, NC, NS}, Opts, Verbose) ->
    erlamsa_netutils:setopts(ProtoTransport, ClientSocket, [{active, once}]),
    ProtoTransportClosed = erlamsa_netutils:closed(ProtoTransport), 
    ProtoTransportClosed = erlamsa_netutils:closed(ProtoTransport), 
    receive
        {ProtoTransport, ClientSocket, Data} ->
            erlamsa_logger:log_data(info, "from client(c->s)", [], Data),
            {Res, Ret} = fuzz(Proto, ProbToServer, ByPass, NC, maps:put(conndirection, ctos, Opts), Data),
            erlamsa_netutils:send(ProtoTransport, ServerSocket, Ret),
            report_fuzzing(Res, "c->s", Ret),
            %% erlamsa_logger:log_data(info, "from fuzzer(c->s) [fuzzing = ~p]", [Res], Ret),
            loop_stream(Proto, ProtoTransport, ClientSocket, ServerSocket,
                    {ProbToClient, raise_prob(ProbToServer, DescentCoeff),  DescentCoeff},
                    {ByPass, NC+1, NS}, Opts, Verbose);
        {ProtoTransport, ServerSocket, Data} ->
            erlamsa_logger:log_data(info, "from server(s->c)", [], Data),
            {Res, Ret} = fuzz(Proto, ProbToClient, ByPass, NS, maps:put(conndirection, stoc, Opts), Data),
            erlamsa_netutils:send(ProtoTransport, ClientSocket, Ret),
            report_fuzzing(Res, "s->c", Ret),
            %% erlamsa_logger:log_data(info, "from fuzzer(s->c) [fuzzing = ~p]", [Res], Ret),
            loop_stream(Proto, ProtoTransport, ClientSocket, ServerSocket,
                    {raise_prob(ProbToClient, DescentCoeff), ProbToServer, DescentCoeff},
                    {ByPass, NC, NS+1}, Opts, Verbose);
        {ProtoTransportClosed, ClientSocket} ->
            erlamsa_netutils:close(ProtoTransport, ServerSocket),
            erlamsa_logger:log(info, "client close (c->s), c:~p ", 
                                    [erlamsa_netutils:socknum(ProtoTransport, ClientSocket)]),
            ok;
        {ProtoTransportClosed, ServerSocket} ->
            erlamsa_netutils:close(ProtoTransport, ClientSocket),
            erlamsa_logger:log(info, "server close (s->c)m l:~p/c:~p ",
                                [erlamsa_netutils:socknum(ProtoTransport, ServerSocket), 
                                 erlamsa_netutils:socknum(ProtoTransport, ClientSocket)]),
            ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Fuzzing helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fuzz(Proto, Prob, Opts, Data) ->
    CallFuzzer = case maps:get(external_fuzzer, Opts, nil) of
        nil -> fun call_fuzzer/3;
        Module -> fun (P, D, O) -> erlang:apply(list_to_atom(Module), fuzzer, [P, D, O]) end
    end, 
    fuzz_rnd(Proto, Prob, erlamsa_rnd:rand_float(), CallFuzzer, Opts, Data).

fuzz(_Proto, _Prob, ByPass, N, _Opts, Data) when N < ByPass ->
    erlamsa_logger:log(decision, "Packet No. ~p < ~p, bypassing data", [N, ByPass]),
    {nofuzz, Data};
fuzz(Proto, Prob, _ByPass, _N, Opts, Data) ->
    fuzz(Proto, Prob, Opts, Data).

fuzz_rnd(_, Prob, Rnd, _CallFuzzer, _Opts, Data) when Rnd >= Prob -> {nofuzz, Data};
fuzz_rnd(http, Prob, _Rnd, CallFuzzer, Opts, Data) ->
    {Status, HTTPHeaders, HTTPData} = erlamsa_netutils:extract_http(Data),
    {ok, NewHTTPData} = CallFuzzer(Prob, Opts, HTTPData),
    {ok, erlamsa_netutils:pack_http(Status, HTTPHeaders, NewHTTPData)};
fuzz_rnd(http2, Prob, _Rnd, CallFuzzer, Opts, Data) ->
    {ok, erlamsa_http2:fuzz_http2(Prob, CallFuzzer, Opts, Data)};
    %%fuzz_http2(http2_fuzz_stream(fun (HTTPData) -> io:format("Fuzzing:~p~n", [HTTPData]), HTTPData end), Data);
fuzz_rnd(_Proto, Prob, _Rnd, CallFuzzer, Opts, Data) ->
    {ok, CallFuzzer(Prob, Opts, Data)}.

call_fuzzer(_Prob, Opts, Data) ->
    erlamsa_fsupervisor:get_fuzzing_output(erlamsa_utils:get_direct_fuzzing_opts(Data, Opts)).
