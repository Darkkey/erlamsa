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
%%% Output to real world
%%% @end
%%%-------------------------------------------------------------------
-module(erlamsa_out).
-author("dark_k3y").

-include("erlamsa.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

%% API
-export([output/3, string_outputs/1, flush/1]).

%% convert list-of-bvecs to one big binary
-spec flush(list(nil | binary())) -> any().
flush([]) -> <<>>;
flush([H|T]) when is_binary(H) -> R = flush(T), <<H/binary, R/binary>>.

%% last member find
-spec last_output(any()) -> any().
%% TODO: FIXME: ugly fix to avoid crashes on [<< >> | { .. }] <-- need to be fixed somewhere upper
last_output([B|L]) when is_list(L), is_binary(B) ->
    last_output(L);
last_output(L) when is_list(L) -> 
    last_output(lists:last(L));
last_output(F) when is_function(F) -> last_output(F());
last_output(X) -> X.

-spec output(lazy_list_of_bins(), output_dest(), fun()) -> {fun(), meta(), integer(), binary()}.
output([], _Fd, _Post) ->
    erlamsa_utils:error({fderror, "Something wrong happened with output FD."});
output(Ll, Fd, Post) ->
    {NLl, NewFd, Data, N} = blocks_port(Ll, Fd, Post),
    %% (ok? (and (pair? ll) (tuple? (car ll)))) ;; all written?
    %% ^-- do we really need to check this?
    {Muta, Meta} = last_output(NLl),
    FlushedData = flush(Data),
    close_port(FlushedData, NewFd),
    {Muta, Meta, N, FlushedData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% STDIO Streams
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec stdout_stream(non_neg_integer(), meta()) -> {fun(), output_dest(), meta_list()}.
stdout_stream(_, Meta) -> {fun stdout_stream/2, stdout, [{output, stdout} | Meta]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal return output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec return_stream(non_neg_integer(), meta()) -> {fun(), output_dest(), meta_list()}.
return_stream(_, Meta) -> {fun return_stream/2, return, [{output, return} | Meta]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% File(s) output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec build_name(list(binary()), integer(), list(list())) -> string().
build_name([H], _N, Acc) ->
    lists:flatten(lists:reverse([binary_to_list(H)|Acc]));
build_name([H|T], N, Acc) ->
    build_name(T, N, [[binary_to_list(H), N]|Acc]).

-spec file_writer(string()) -> fun().
file_writer(Str) ->
    {ok, SRe} = re:compile("%n"),
    Tokens = re:split(Str, SRe),
    fun F(N, Meta) ->
        Filename = build_name(Tokens, integer_to_list(N), []),
        {Res, Fd} = file:open(Filename, [write, raw, binary]),
        case Res of
            ok -> {F, Fd, [{output, file} | Meta]};
            _Else ->
                %% TODO: add printing filename, handling -r and other things...
                Err = lists:flatten(io_lib:format("Error opening file '~s'", [Filename])),
                erlamsa_utils:error(Err)
        end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Serial output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec serial_writer(list()) -> fun().
serial_writer(Options) ->
    SerialPort = serial:start(Options),  
    fun F(_N, Meta) ->
        {F, {serial,
            fun (Data) -> SerialPort ! {send, Data}, ok end,
            fun () -> ok end
        }, [{output, serial} | Meta]}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exec output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec exec_writer(string()) -> fun().
exec_writer(App) ->
    %%TODO: FIXME: select correct path
    exec:start([{portexe, "priv/exec-port"}]),
    Watcher = spawn(fun W() -> 
                        receive 
                            {Stream, Pid, Data} -> 
                                erlamsa_logger:log_data(info, "received from ~p on ~p", [Pid, Stream], Data);
                            Unknown -> 
                                erlamsa_logger:log(info, "received: ~p", [Unknown])                                
                        end, W()
                    end),  
    fun F(_N, Meta) ->
        {ok, Pid, OsPid} = exec:run(App, [stdin, monitor, {stdout, Watcher}, {stderr, Watcher}]),
        erlamsa_logger:log(info, "new process is executed [OS Pid = ~p]", [OsPid]),
        erlamsa_monitor:send_pid(OsPid),
        {F, {exec,
            fun (Data) -> 
                exec:send(Pid, Data)
            end,
            fun () -> 
                exec:send(Pid, eof), %% close stdin                  
                receive
                    {'DOWN', OsPid, process, Pid, Reason} ->
                        {LogStatus, LogLevel} = case Reason of 
                            normal -> {ok, info}; 
                            {exit_status, Status} -> {exec:status(Status), finding} 
                        end,
                        erlamsa_logger:log(LogLevel, "process ~p exit with reason ~p", [OsPid, LogStatus])
                after ?EXEC_SLEEP -> 
                    exec:stop(Pid)               
                end
            end
        }, [{output, exec} | Meta]}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Rawsocket client output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec rawsock_writer(inet:ip_address(), list()) -> fun().
rawsock_writer(Addr, Options) ->
    fun F(_N, Meta) ->
        {Res, FD} = procket:open(0, Options),
        case Res of
            ok ->
                {ok, Sock} = gen_udp:open(0, [binary, {fd, FD}]),
                {F, {net,
                    fun (Data) -> gen_udp:send(Sock, Addr, 0, Data) end,
                    fun () -> gen_udp:close(Sock), procket:close(FD), ok end
                }, [{output, ipsock} | Meta]};
            Else ->
                Err = lists:flatten(
                        io_lib:format("Error creating raw socket to ip://~s with options ~p: ~p",
                        [Addr, Options, Else])),
                erlamsa_utils:error(Err)
        end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generic Steam (TCP or TLS) Server Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec streamlisten_writer(tcp | tls, inet:port_number(), {string(), string()}, fun()) -> fun().
streamlisten_writer(Transport, LocalPort, {CertFile, KeyFile}, PostProcess) ->
    ExtraOpts =  case Transport of
        ssl -> [{certfile, CertFile}, {keyfile, KeyFile}];
        _Else -> []
    end,
    fun F(_N, Meta) ->
        erlamsa_netutils:netserver_start(Transport),
        {Res, ListenSock} = erlamsa_netutils:listen(Transport, LocalPort,
                                            [binary, {active, false},
                                            {reuseaddr, true}, {packet, 0},
                                            {send_timeout, 5000} | ExtraOpts]
                                          ),
        case Res of
            ok -> {F, {net,
                fun (Data) ->                    
                    case erlamsa_netutils:accept(Transport, ListenSock) of
                        {ok, ClientSock} ->
                            {ok, {IP, _}} = erlamsa_netutils:sockname(Transport, ClientSock), 
                            ets:insert(global_config, [{cm_host, IP}]),
                            {ok, {Address, Port}} = erlamsa_netutils:peername(Transport, ClientSock), 
                            TmpRecv = erlamsa_netutils:recv(Transport, ClientSock, 0, infinity),
                            erlamsa_logger:log(info, "~p client connect from ~p:~p: ~p",
                                            [Transport, Address, Port, TmpRecv]),
                            erlamsa_netutils:send(Transport, ClientSock, PostProcess(Data)), timer:sleep(1),
                            erlamsa_netutils:close(Transport, ClientSock),
                            %% FIXME: should we close listen socket here? is it ok?
                            erlamsa_netutils:close(Transport, ListenSock);
                        {error, {options,{Option1,Option2,{error,Result}}}} ->
                            Err = lists:flatten(io_lib:format("~p error: option ~p value ~p is incorrect: ~p",
                                                  [Transport, Option1, Option2, Result])),
                            erlamsa_utils:error({cantconnect, Err});
                        {error, closed} -> ok; %% listen socket is already closed, do nothing
                        {error, Error} ->
                            erlamsa_logger:log(info, "~p client connect error: ~p",
                                        [Transport, Error])
                    end
                end,
                fun () -> erlamsa_netutils:close(Transport, ListenSock), ok end
                },
                [{output, Transport} | Meta]};
            Else ->
                Err = lists:flatten(io_lib:format("Error listening on ~p port ~p: '~s'",
                                                  [Transport, LocalPort, Else])),
                erlamsa_utils:error({cantconnect, Err})
        end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generic Network Stream (TCP or TLS) Client output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec streamsock_writer(tcp | tls, inet:ip_address(), inet:port_number(), fun()) -> fun().
streamsock_writer(Transport, Addr, Port, Maker) ->
    erlamsa_netutils:set_routing_ip(tcp, Addr, Port),
    fun F(_N, Meta) ->
        erlamsa_netutils:netserver_start(Transport),
        {Res, Sock} = erlamsa_netutils:connect(Transport, Addr, Port, [binary, {active, true}], ?TCP_TIMEOUT),
        case Res of
            ok -> {F, {net,
                fun (Data) -> Packet = Maker(Data), erlamsa_netutils:send(Transport, Sock, Packet) end,
                %% TODO: ugly timeout before closing..., should be in another thread
                fun () -> timer:sleep(50), erlamsa_netutils:close(Transport, Sock), ok end
                }, [{output, Transport} | Meta]};
            _Else ->
                Err = lists:flatten(io_lib:format("Error opening ~p socket to ~s:~p '~s'",
                                                  [Transport, Addr, Port, Sock])),
                erlamsa_utils:error({cantconnect, Err})
        end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UDP Server Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec udplisten_writer(inet:port_number()) -> fun().
udplisten_writer(LocalPort) ->
    fun F(_N, Meta) ->
        {Res, Sock} = gen_udp:open(LocalPort, [binary, {active, false},
                                               {reuseaddr, true}, {ip, {0, 0, 0, 0}}
                                              ]
                                  ),
        case Res of
            ok -> {F, {net,
                fun (Data) ->
                    {ok, {Address, Port, TmpPacket}} = gen_udp:recv(Sock, 0, infinity),
                    erlamsa_logger:log(info, "udp message received from ~p:~p: ~p",
                                             [Address, Port, TmpPacket]),
                    gen_udp:send(Sock, Address, Port, Data)
                end,
                fun () -> gen_udp:close(Sock) end
                }, [{output, udpsock} | Meta]};
            _Else ->
                Err = lists:flatten(io_lib:format("Error listen on udp port ~p: '~s'",
                                                  [LocalPort, Sock])),
                erlamsa_utils:error(Err)
        end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UDP Client Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec udpsock_writer(inet:ip_address(), inet:port_number(), inet:port_number(), list()) -> fun().
%% TODO: FIXME: whether this is correct?
udpsock_writer(Addr, PortFrom, PortTo, Options) ->
    fun F(_N, Meta) ->
        {Res, Sock} = gen_udp:open(PortFrom, [binary, {active, true}, {reuseaddr, true} | Options]),
        case Res of
            ok -> {F, {net,
                fun (Data) -> gen_udp:send(Sock, Addr, PortTo, Data) end,
                fun () -> gen_udp:close(Sock) end
                }, [{output, udpsock} | Meta]};
            _Else ->
                Err = lists:flatten(io_lib:format("Error opening udp port ~p: '~s'",
                                                  [PortFrom, Sock])),
                erlamsa_utils:error(Err)
        end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HTTP protocol helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec make_http_server_reply(string()) -> fun().
make_http_server_reply(ContentType) ->
    fun (Data) ->
        Len = size(Data),
        Headers = list_to_binary(
                    io_lib:format("HTTP/1.1 200 OK\r\nContent-type: ~s\r\nContent-Length: ~p\r\n\r\n", 
                                    [ContentType, Len])
                    ),
        <<Headers/binary, Data/binary>>
    end.

-spec make_http_writer(atom(), inet:ip_address(), inet:port_number(), string(),
                       string(), string(), string(), list(string())) -> fun().
make_http_writer(Transport, Host, Port, Path, Query, Type, Param, Options) ->
    Maker = create_http_header(Host, Type, Param, Path, Query, http_headers(Options, [])),
    streamsock_writer(Transport, Host, Port, Maker).

-spec http_writer(atom(), tuple(), fun()) -> fun().
http_writer(Transport, {[], Port, _Path, _Query, []}, Opts) ->
    http_writer(Transport, {[], Port, _Path, _Query, "application/octet-stream"}, Opts);
http_writer(Transport,{[], Port, _Path, _Query, ContentType}, Opts) ->
    streamlisten_writer(Transport, Port, Opts, make_http_server_reply(ContentType));
http_writer(Transport, {Host, Port, Path, Query, []}, _) ->
    make_http_writer(Transport, Host, Port, Path, Query, "GET", "", []);
http_writer(Transport, {Host, Port, Path, Query, ["GET", Param|T]}, _) ->
    make_http_writer(Transport, Host, Port, Path, Query, "GET", Param, T);
http_writer(Transport, {Host, Port, Path, Query, [Type | T]}, _) ->
    make_http_writer(Transport, Host, Port, Path, Query, Type, "", T).

-spec make_host_header(string()) -> binary().
make_host_header(Host) ->
    list_to_binary("Host: " ++ Host ++ [13, 10]).

-spec create_http_reply_headers(list(), binary(), binary()) -> fun().
create_http_reply_headers(First, HostHeader, Headers) ->
    fun (Data) ->
        flush([list_to_binary(First), Data, list_to_binary(" HTTP/1.1"), <<13, 10>>,
                HostHeader, Headers, <<13, 10>>
              ])
    end.

-spec create_http_header(string(), string(), string(), string(), string(), string()) -> fun().
create_http_header(Host, "GET", "", Path, Query, Headers) ->
    First = "GET " ++ Path ++ Query,
    HostHeader = make_host_header(Host),
    create_http_reply_headers(First, HostHeader, Headers);
create_http_header(Host, "GET", Param, Path, Query, Headers) ->
    First = "GET " ++ Path ++ Query ++ "&" ++ Param ++ "=",
    HostHeader = make_host_header(Host),
    create_http_reply_headers(First, HostHeader, Headers);
create_http_header(Host, "POST", _, Path, Query, Headers) ->
    First = "POST " ++ Path ++ Query,
    HostHeader = make_host_header(Host),
    fun (Data) ->
        CL = "Content-length: " ++ integer_to_list(byte_size(Data)) ++ [13, 10],
        flush([list_to_binary(First), list_to_binary(" HTTP/1.1"), <<13, 10>>,
                Headers, HostHeader, list_to_binary(CL), <<13, 10>>, Data])
    end.

http_headers([H|T], Acc) ->
    http_headers(T, [list_to_binary(H), <<13, 10>> | Acc]);
http_headers([], Acc) ->
    flush(Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Output specification parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% TODO: FIXME: using of cert and key file from cmd options
-spec string_outputs(#{}) -> fun().
string_outputs(Opts) ->
    case maps:get(output, Opts, "-") of
        "-" -> fun stdout_stream/2;
        return -> fun return_stream/2;
        {tcp, {Port}} -> streamlisten_writer(tcp, list_to_integer(Port), {nil, nil}, fun (A) -> A end);
        {tls, {Port}} -> streamlisten_writer(ssl, list_to_integer(Port), 
                                                {maps:get(certfile, Opts, "cert.pem"), 
                                                 maps:get(keyfile, Opts, "key.pem")}, 
                                                fun (A) -> A end);
        {udp, {Port}} -> udplisten_writer(list_to_integer(Port));
        {tcp, {Addr, Port}} -> streamsock_writer(tcp, Addr, list_to_integer(Port), fun (A) -> A  end);
        {tls, {Addr, Port}} -> streamsock_writer(ssl, Addr, list_to_integer(Port), fun (A) -> A  end);
        {udp, {StrIfAddr, PortFrom, Addr = "255.255.255.255", PortTo}} -> 
            {ok, IfAddr} = inet:getaddr(StrIfAddr, inet), %%TODO: ugly, refactor in distinct function
            udpsock_writer(Addr, list_to_integer(PortFrom), list_to_integer(PortTo), [{broadcast, true}, {ip, IfAddr}]);
        {udp, {StrIfAddr, PortFrom, Addr, PortTo}} -> 
            {ok, IfAddr} = inet:getaddr(StrIfAddr, inet),
            udpsock_writer(Addr, list_to_integer(PortFrom), list_to_integer(PortTo), [{ip, IfAddr}]);
        {udp, {PortFrom, Addr, PortTo}} -> udpsock_writer(Addr, list_to_integer(PortFrom), list_to_integer(PortTo), []);
        {udp, {Addr, Port}} -> udpsock_writer(Addr, 0, list_to_integer(Port), []);
        %% RAW IP output <-- fuzzed packet is inside proper IP packet
        {ip, {Addr, Proto}} -> rawsock_writer(Addr,
                                              [{protocol, list_to_integer(Proto)},
                                               {type, raw}, {family, inet}]
                                             );
        %% RAW output <-- fuzzed packet are on layer 3
        {raw, {Addr, Iface}} -> rawsock_writer(Addr,
                                               [{protocol, raw}, {interface, Iface},
                                                {type, raw}, {family, inet}]
                                              );
        {http, Params} -> 
            http_writer(tcp, Params, {nil, nil});
        {https, Params} -> 
            http_writer(ssl, Params, {maps:get(certfile, Opts, "cert.pem"), 
                                      maps:get(keyfile, Opts, "key.pem")});
        {serial, {Port, Speed}} ->
            serial_writer([{open, Port}, {speed, list_to_integer(Speed)}]);
        {exec, App} ->
            exec_writer(App);
        Str -> file_writer(Str)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PORTS Operations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% write blocks to port
-spec blocks_port(lazy_list_of_bins(), output_dest(), fun())
    -> {lazy_list_of_bins(), list(), non_neg_integer()}.
blocks_port(Ll, Fd, Post) -> blocks_port(Ll, Fd, [], 0, Post).

%% TODO: UGLY, need rewrite and handle errors
-spec blocks_port(lazy_list_of_bins(), output_dest(), list(), non_neg_integer(), fun())
    -> {lazy_list_of_bins(), list(), non_neg_integer()}.
blocks_port([], Fd, Data, N, _) -> {[], Fd, Data, N};
blocks_port([Ll], Fd, Data, N, Post) when is_function(Ll) -> blocks_port(Ll(), Fd, Data, N, Post);
blocks_port(Ll = [H|T], Fd, Data, N, Post) when is_binary(H) ->
    {Res, NewData} = write_really(Post(H), Fd),
    case Res of
        ok when is_binary(H) -> blocks_port(T, Fd, [NewData|Data], N + byte_size(H), Post);
        _Else -> {Ll, Fd, Data, N}
    end;
blocks_port(Ll, Fd, Data, N, _) -> {Ll, Fd, Data, N}.

%% closes the port (and possibly writes the data in case of HTTP)
-spec close_port(binary(), output_dest()) -> ok | {'error', file:posix() | badarg | terminated}.
close_port(_, stdout) -> ok;
close_port(_, return) -> ok;
close_port(_, {net, _Writer, Closer}) -> Closer();
close_port(_, {exec, _Writer, Closer}) -> Closer();
close_port(_, {serial, _Writer, Closer}) -> Closer();
close_port(Data, {http, Final}) -> Final(Data);
close_port(_, Fd) -> file:close(Fd).

%% write to Fd or stdout
%% TODO: UGLY, need rewrite and handle errors, also rewrite spec
-spec write_really(binary(), output_dest()) -> {any(), binary()}.
write_really(Data, return) -> {ok, Data};
write_really(Data, stdout) -> {file:write(standard_io, Data), <<>>};
write_really(Data, {http, _Final}) -> {ok, Data};
write_really(Data, {https, _Final}) -> {ok, Data};
write_really(Data, {net, Writer, _Closer}) -> {Writer(Data), <<>>};
write_really(Data, {exec, Writer, _Closer}) -> {Writer(Data), <<>>};
write_really(Data, {serial, Writer, _Closer}) -> {Writer(Data), <<>>};
write_really(Data, Fd) -> {file:write(Fd, Data), <<>>}.
