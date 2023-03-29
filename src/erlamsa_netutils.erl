% Copyright (c) 2011-2014 Aki Helin
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
%%% Common network functions
%%% @end
%%%-------------------------------------------------------------------

-module(erlamsa_netutils).
-author("dark_k3y").

-include("erlamsa.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

%% API
-export([transport/1, netserver_start/2, listen/3, port/2, socknum/2, 
         accept/2, peername/2, sockname/2, host2str/1, connect/4, connect/5,
         setopts/3, send/3, recv/4, closed/1, close/2, 
         set_routing_ip/3, get_default_http_port/1, extract_http/1, pack_http/3]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Network interface functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Erlang is using ssl abbrev. istead of tls, so convert.
transport(http2s) -> ssl;
transport(https) -> ssl;
transport(http) -> tcp;
transport(http2) -> tcp;
transport(cansockd_isotp) -> tcp;
transport(cansockd) -> tcp;
transport(tls) -> ssl;
transport(Tr) -> Tr.

netserver_start(ssl, _Forced) -> ssl:start();
netserver_start(_Proto, true) -> ssl:start();
netserver_start(_Proto, false) -> ok.

listen(ssl, LocalPort, Opts) ->
    ssl:listen(LocalPort, Opts);
listen(tcp, LocalPort, Opts) ->
    gen_tcp:listen(LocalPort, Opts);
listen(udp, LocalPort, Opts) ->
    gen_udp:open(LocalPort, Opts);
listen(_, _P, _) ->
    {error, "Unsupported fuzzing layer."}.

port(ssl, Sock) -> {_, Port} = ssl:sockname(Sock), {ok, Port};
port(_Proto, Sock) -> inet:port(Sock).

socknum(ssl, {sslsocket, nil, {EPort, _}}) ->  EPort;
socknum(ssl, {sslsocket, {gen_tcp, EPort, _, _}, _}) ->  EPort;
socknum(_ProtoTransport, ListenSock) -> ListenSock.

accept(ssl, Sock) -> 
    case ssl:transport_accept(Sock) of 
        {ok, TLSTransportSocket} -> 
            case ssl:handshake(TLSTransportSocket) of
                ok -> TLSTransportSocket;
                Fail -> Fail
            end;
        Else -> 
            Else
    end;
accept(tcp, Sock) -> gen_tcp:accept(Sock).

host2str(DHost) when is_list(DHost) -> DHost;
host2str(DHost) -> inet:ntoa(DHost).

sockname(ssl, Sock) -> ssl:sockname(Sock);
sockname(tcp, Sock) -> inet:sockname(Sock).

peername(ssl, Sock) -> ssl:peername(Sock);
peername(tcp, Sock) -> inet:peername(Sock).

connect(tcp, DHost, DPort, Opts) -> gen_tcp:connect(DHost, DPort, Opts);
connect(ssl, DHost, DPort, Opts) -> ssl:connect(DHost, DPort, Opts).

connect(tcp, DHost, DPort, Opts, Timeout) -> gen_tcp:connect(DHost, DPort, Opts, Timeout);
connect(ssl, DHost, DPort, Opts, Timeout) -> ssl:connect(DHost, DPort, Opts, Timeout).

setopts(tcp, ClientSocket, Opts) -> inet:setopts(ClientSocket, Opts);
setopts(ssl, ClientSocket, Opts) -> ssl:setopts(ClientSocket, Opts).

send(tcp, Sock, Data) -> gen_tcp:send(Sock, Data);
send(ssl, Sock, Data) -> ssl:send(Sock, Data).

recv(tcp, Sock, Size, Timeout) -> gen_tcp:recv(Sock, Size, Timeout);
recv(ssl, Sock, Size, Timeout) -> ssl:recv(Sock, Size, Timeout).

closed(tcp) -> tcp_closed;
closed(ssl) -> ssl_closed.

close(tcp, Sock) -> io:format("~n~n~n~n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@~n~n~n"), gen_tcp:close(Sock);
close(ssl, Sock) -> ssl:close(Sock).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Routing info functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_routing_ip(tcp, inet:ip_address(), integer()) -> {inet:ip_address(), integer()} | {}.
get_routing_ip(tcp, Host, Port) ->
    case gen_tcp:connect(Host, Port, [binary, {packet, 0}]) of
        {ok, Sock} -> 
            {ok, {SIp, _}} = inet:sockname(Sock), gen_tcp:close(Sock), SIp;
        _Else -> 
            {ok, IFs} = inet:getif(),
            {Ip, _, _} = hd(IFs),
            Ip
    end.

-spec set_routing_ip(tcp, inet:ip_address(), integer()) -> ok.
set_routing_ip(Proto, Host, Port) ->
    spawn(fun() -> 
        IP = get_routing_ip(Proto, Host, Port),
        ets:insert(global_config, [{cm_host, IP}]) 
    end),
    ok;
set_routing_ip(_, _Host, _Port) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HTTP Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_default_http_port(undefined) -> 80;
get_default_http_port(Else) when is_list(Else) -> list_to_integer(Else);
get_default_http_port(Else) -> Else.

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

