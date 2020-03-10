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
-export([get_supervisor_opts/1, start/1, start_fuzzproxy/1, server_stream/5, loop_udp/7]).

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

start_fuzzproxy(Opts) ->
    Workers = maps:get(workers, Opts, 10),
    Verbose = erlamsa_utils:verb(stdout, maps:get(verbose, Opts, 0)),
    ProxyAddrList = maps:get(proxy_address, Opts),
    lists:map(
        fun(L) ->
            {StrProto, LocalPort, _, DHost, DPort} = L,
            erlamsa_netutils:set_routing_ip(tcp, DHost, DPort),
            Proto = list_to_atom(StrProto),
            ProtoTransport = erlamsa_netutils:transport(Proto),
            erlamsa_netutils:netserver_start(ProtoTransport),
            case proxy_listen(ProtoTransport, LocalPort, Opts) of
                {ok, ListenSock} ->
                    {ok, Port} = erlamsa_netutils:port(ProtoTransport, ListenSock),
                    start_servers(ProtoTransport, Workers, ListenSock, L, Opts, Verbose),
                    Port;
                {error, Reason} ->
                    io:format("Error: ~s", [Reason]),
                    {error, Reason}
            end
        end, ProxyAddrList),
    timer:sleep(infinity).

spawn_stream_worker(ProtoTransport, ListenSock, Endpoint, Opts, Verbose) ->
    spawn(?MODULE, server_stream, [ProtoTransport, ListenSock, Endpoint, Opts, Verbose]).

start_servers(udp, _Workers, ListenSock, Endpoint, Opts, Verbose) ->
    erlamsa_logger:log(info, "udp proxy worker process started, socket id ~p",
                        [ListenSock]),
    Pid = spawn(?MODULE, loop_udp, [ListenSock, Endpoint, init_clientsocket, [], 0, Opts, Verbose]),
    gen_udp:controlling_process(ListenSock, Pid);
start_servers(ProtoTransport, Workers, ListenSock, Endpoint, Opts, Verbose) ->
    length([spawn_stream_worker(ProtoTransport, ListenSock, Endpoint, Opts, Verbose) || _N <- lists:seq(1, Workers)]).

server_stream(ProtoTransport, ListenSock, Endpoint, Opts, Verbose) ->
    {Proto, LPort, _, DHost, DPort} = Endpoint,
    {ProbToClient, ProbToServer} = get_proxy_probs(Opts),
    ByPass = maps:get(bypass, Opts, 0),
    DescentCoeff = maps:get(descent_coeff, Opts, 1),
    erlamsa_logger:log(info, "tcp proxy worker process started, listening on ~p (port :~p)",
                        [erlamsa_netutils:socknum(ProtoTransport, ListenSock), LPort]),
    erlamsa_rnd:seed(now()),
    Res = erlamsa_netutils:accept(ProtoTransport, ListenSock),
    case Res of
        {ok, ClientSocket} ->
            erlamsa_logger:log(info,
                                "initiating new connection to ~s:~p(c->s), sockets: l:~p/c:~p",
                                [inet:ntoa(DHost), DPort, 
                                 erlamsa_netutils:socknum(ProtoTransport, ListenSock), 
                                 erlamsa_netutils:socknum(ProtoTransport, ClientSocket)]),
            case erlamsa_netutils:connect(ProtoTransport, DHost, DPort, 
                                          [binary, {packet, 0}, {active, true}]) of
                  {ok, ServerSocket} ->
                      loop_stream(list_to_atom(Proto), ProtoTransport, ClientSocket, ServerSocket,
                        {ProbToClient/DescentCoeff, ProbToServer/DescentCoeff, DescentCoeff},
                        {ByPass, 0, 0},
                        Opts, Verbose),
                      spawn_stream_worker(ProtoTransport, ListenSock, Endpoint, Opts, Verbose);
                  E ->
                      io:format("Error: connect to server failed!~n"),
                      erlamsa_logger:log(info, "error: worker connect to server ~s:~p failed!",
                                                [inet:ntoa(DHost), DPort]),
                  E
              end,
          ok;
        Other ->
            erlamsa_logger:log(warning, "error: accept returned ~p", [Other]),
            ok
    end.

report_fuzzing(nofuzz, Ret) ->
    erlamsa_logger:log_data(debug, "from fuzzer(c->s) [fuzzing = nofuzz, skipped]", [], Ret);
report_fuzzing(Res, Ret) ->
    erlamsa_logger:log_data(info, "from fuzzer(c->s) [fuzzing = ~p]", [Res], Ret).

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
                report_fuzzing(Res, Ret),
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
                        report_fuzzing(Res, Ret),
                        %%erlamsa_logger:log_data(info, "from fuzzer(c->s) [fuzzing = ~p]",
                        %%                        [Res], Ret),
                        {ClientHost, ClientPort}
                end
        end,
    loop_udp(SrvSocket, ClSocket, Endpoint, NewHost, NewPort, Opts, Verbose).

loop_stream(Proto, ProtoTransport, ClientSocket, ServerSocket, {ProbToClient, ProbToServer, DescentCoeff},
        {ByPass, NC, NS}, Opts, Verbose) ->
    erlamsa_netutils:setopts(ProtoTransport, ClientSocket, [{active, once}]),
    ProtoTansportClosed = erlamsa_netutils:closed(ProtoTransport), 
    receive
        {ProtoTransport, ClientSocket, Data} ->
            erlamsa_logger:log_data(info, "from client(c->s)", [], Data),
            {Res, Ret} = fuzz(Proto, ProbToServer, ByPass, NC, Opts, Data),
            erlamsa_netutils:send(ProtoTransport, ServerSocket, Ret),
            report_fuzzing(Res, Ret),
            %% erlamsa_logger:log_data(info, "from fuzzer(c->s) [fuzzing = ~p]", [Res], Ret),
            loop_stream(Proto, ProtoTransport, ClientSocket, ServerSocket,
                    {ProbToClient, raise_prob(ProbToServer, DescentCoeff),  DescentCoeff},
                    {ByPass, NC+1, NS}, Opts, Verbose);
        {ProtoTransport, ServerSocket, Data} ->
            erlamsa_logger:log_data(info, "from server(s->c)", [], Data),
            {Res, Ret} = fuzz(Proto, ProbToClient, ByPass, NS, Opts, Data),
            erlamsa_netutils:send(ProtoTransport, ClientSocket, Ret),
            report_fuzzing(Res, Ret),
            %% erlamsa_logger:log_data(info, "from fuzzer(s->c) [fuzzing = ~p]", [Res], Ret),
            loop_stream(Proto, ProtoTransport, ClientSocket, ServerSocket,
                    {raise_prob(ProbToClient, DescentCoeff), ProbToServer, DescentCoeff},
                    {ByPass, NC, NS+1}, Opts, Verbose);
        {ProtoTansportClosed, ClientSocket} ->
            erlamsa_netutils:close(ProtoTransport, ServerSocket),
            erlamsa_logger:log(info, "client close (c->s), c:~p ", 
                                    [erlamsa_netutils:socknum(ProtoTransport, ClientSocket)]),
            ok;
        {ProtoTansportClosed, ServerSocket} ->
            erlamsa_netutils:close(ProtoTransport, ClientSocket),
            erlamsa_logger:log(info, "server close (s->c)m l:~p/c:~p ",
                                [erlamsa_netutils:socknum(ProtoTransport, ServerSocket), 
                                 erlamsa_netutils:socknum(ProtoTransport, ClientSocket)]),
            ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HTTP Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extract_http(Data) ->
    case erlang:decode_packet(http, Data, []) of
        {ok, Query, Rest} ->
            extract_http_headers(Rest, [Query]);
        Err ->
            erlamsa_logger:log(warning, "Invalid HTTP query?: ~p~n", [Err]),
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
            erlamsa_logger:log(warning, "Error parsing HTTP header: ~p~n", [Err]),
            {ok, Acc, Data}
    end.

pack_http(more, Headers, Data) ->
    pack_http_packet(Headers, Data, []);
pack_http(ok, Headers, Data) ->
    pack_http_packet(Headers, Data, [[10, 13]]).

pack_http_packet([{http_header, _, 'Content-Length', _, _}|T], Data, Acc) ->
    Len = size(Data),
    pack_http_packet(T, Data,
                    [list_to_binary(io_lib:format("Content-Length: ~p~c~n", [Len, 13])) | Acc]);
pack_http_packet([{http_header, _, HdrName, _, HdrValue}|T], Data, Acc) ->
    pack_http_packet(T, Data,
                    [list_to_binary(
                        io_lib:format("~s: ~s~c~n",
                                      [atom_to_list(HdrName), HdrValue, 13]))
                    | Acc]);
pack_http_packet([{http_request, Method, {abs_path, Path}, {VerMajor, VerMinor}}|T], Data, Acc) ->
    pack_http_packet(T, Data,
                    [list_to_binary(
                        io_lib:format("~s ~s HTTP/~p.~p~p~n",
                                        [Method, Path, VerMajor, VerMinor, 13]))
                    | Acc]);
pack_http_packet([{http_response, {VerMajor, VerMinor}, Code, Status}|T], Data, Acc) ->
    pack_http_packet(T, Data,
                    [list_to_binary(
                        io_lib:format("HTTP/~p.~p ~p ~s~c~n",
                                        [VerMajor, VerMinor, Code, Status, 13]))
                    | Acc]);
pack_http_packet([{http_error, ErrHdr}|T], Data, Acc) ->
    pack_http_packet(T, Data, [ErrHdr | Acc]);
pack_http_packet([], Data, Acc) ->
    Hdr = list_to_binary(Acc),
    <<Hdr/binary, Data/binary>>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Fuzzing helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fuzz(_Proto, _Prob, ByPass, N, _Opts, Data) when N < ByPass ->
    erlamsa_logger:log(decision, "Packet No. ~p < ~p, bypassing data", [N, ByPass]),
    {nofuzz, Data};
fuzz(Proto, Prob, _ByPass, _N, Opts, Data) ->
    fuzz(Proto, Prob, Opts, Data).

fuzz(Proto, Prob, Opts, Data) ->
    case maps:get(external_fuzzer, Opts, nil) of
        nil -> fuzz_rnd(Proto, Prob, erlamsa_rnd:rand_float(), Opts, Data);
        Module ->
            erlang:apply(
                list_to_atom(Module), fuzzer, [Proto, Data, maps:put(fuzzprob, Prob, Opts)]
            )
    end.

fuzz_rnd(_, Prob, Rnd, _Opts, Data) when Rnd >= Prob -> {nofuzz, Data};
fuzz_rnd(http, Prob, _Rnd, Opts, Data) ->
    {Status, HTTPHeaders, HTTPData} = extract_http(Data),
    {ok, pack_http(Status, HTTPHeaders, call_fuzzer(Prob, Opts, HTTPData))};
fuzz_rnd(_Proto, Prob, _Rnd, Opts, Data) ->
    {ok, call_fuzzer(Prob, Opts, Data)}.

call_fuzzer(_Prob, Opts, Data) ->
    erlamsa_fsupervisor:get_fuzzing_output(erlamsa_utils:get_direct_fuzzing_opts(Data, Opts)).
