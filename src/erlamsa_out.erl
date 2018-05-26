%%%-------------------------------------------------------------------
%%% @author dark_k3y
%%% @doc
%%% Output to real-world
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

-spec output(lazy_list_of_bins(), output_dest(), fun()) -> {fun(), meta(), integer(), binary()}.
output([], _Fd, _Post) ->
    erlamsa_utils:error({fderror, "Something wrong happened with output FD."});
output(Ll, Fd, Post) ->
    {NLl, NewFd, Data, N} = blocks_port(Ll, Fd, Post),
    %% (ok? (and (pair? ll) (tuple? (car ll)))) ;; all written? <-- do we really need to check this?
    {Muta, Meta} = erlamsa_utils:last(NLl),
    FlushedData = flush(Data),
    close_port(FlushedData, NewFd),
    {Muta, Meta, N, FlushedData}.

-spec stdout_stream(non_neg_integer(), meta()) -> {fun(), output_dest(), meta_list()}.
stdout_stream(_, Meta) -> {fun stdout_stream/2, stdout, [{output, stdout} | Meta]}.

-spec return_stream(non_neg_integer(), meta()) -> {fun(), output_dest(), meta_list()}.
return_stream(_, Meta) -> {fun return_stream/2, return, [{output, return} | Meta]}.

-spec build_name(list(binary()), integer(), list(list())) -> string().
build_name([H], _N, Acc) -> 
    lists:flatten(lists:reverse([binary_to_list(H)|Acc]));
build_name([H|T], N, Acc) ->     
    build_name(T, N, [[binary_to_list(H),N]|Acc]).

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
                Err = lists:flatten(io_lib:format("Error opening file '~s'", [Filename])),  %% TODO: add printing filename, handling -r and other things...
                erlamsa_utils:error(Err)
        end
    end.

-spec ipsock_writer(inet:ip_address(), non_neg_integer()) -> fun().
ipsock_writer(Addr, Proto) ->
    fun F(_N, Meta) ->
        {Res, FD} = procket:open(0, [{protocol, Proto}, {type, raw}, {family, inet}]),
        case Res of 
            ok ->  
                {ok, Sock} = gen_udp:open(0, [binary, {fd, FD}]),        
                {F, {net, 
                    fun (Data) -> gen_udp:send(Sock, Addr, 0, Data) end,                
                    fun () -> gen_udp:close(Sock), procket:close(FD), ok end 
                }, [{output, ipsock} | Meta]};
            Else ->
                Err = lists:flatten(io_lib:format("Error creating raw socket to ip://~s protocol no. ~p: ~p", [Addr, Proto, Else])), 
                erlamsa_utils:error(Err)
        end
    end.

-spec rawsock_writer(inet:ip_address(), list()) -> fun().
rawsock_writer(Addr, Iface) ->
    fun F(_N, Meta) ->
        {Res, FD} = procket:open(0, [{protocol, raw}, {interface, Iface}, {type, raw}, {family, inet}]),
        case Res of 
            ok ->          
                {F, {net, 
                    fun (Data) -> procket:sendto(FD, Data) end,                
                    fun () -> procket:close(FD), ok end 
                }, [{output, rawsock} | Meta]};
            Else ->
                Err = lists:flatten(io_lib:format("Error creating raw socket to raw://~s on interface ~s: ~p", [Addr, Iface, Else])), 
                erlamsa_utils:error(Err)
        end
    end.    

-spec tcpsock_writer(inet:ip_address(), inet:port_number()) -> fun().
tcpsock_writer(Addr, Port) ->
    fun F(_N, Meta) ->
        {Res, Sock} = gen_tcp:connect(Addr, Port, [binary, {active, true}], ?TCP_TIMEOUT),
        case Res of 
            ok -> {F, {net, 
                fun (Data) -> gen_tcp:send(Sock, Data) end,                
                fun () -> timer:sleep(50), gen_tcp:close(Sock), ok end %% TODO: ugly timeout before closing..., should be in another thread
                }, [{output, tcpsock} | Meta]};
            _Else -> 
                Err = lists:flatten(io_lib:format("Error opening tcp socket to ~s:~p '~s'", [Addr, Port, Sock])), 
                erlamsa_utils:error({cantconnect, Err})
        end
    end.
     
-spec udpsock_writer(inet:ip_address(), inet:port_number()) -> fun().
udpsock_writer(Addr, Port) ->
    fun F(_N, Meta) ->
        {Res, Sock} = gen_udp:open(Port, [binary, {active, true}, {reuseaddr, true}]),
        case Res of 
            ok -> {F, {net, 
                fun (Data) -> gen_udp:send(Sock, Addr, Port, Data) end,
                fun () -> gen_udp:close(Sock) end
                }, [{output, udpsock} | Meta]};
            _Else -> 
                Err = lists:flatten(io_lib:format("Error opening udp port ~p: '~s'", [Port, Sock])), 
                erlamsa_utils:error(Err)
        end
    end.

-spec http_writer(tuple()) -> fun().   
http_writer({Host, Port, Path, Query, []}) -> 
    make_http_writer(Host, Port, Path, Query, "GET", "", []);
http_writer({Host, Port, Path, Query, ["GET",Param|T]}) -> 
    make_http_writer(Host, Port, Path, Query, "GET", Param, T);
http_writer({Host, Port, Path, Query, [Type | T]}) -> 
    make_http_writer(Host, Port, Path, Query, Type, "", T).

-spec make_host_header(string()) -> binary(). 
make_host_header(Host) ->
    list_to_binary("Host: " ++ Host ++ [13,10]).

-spec create_http_header_maker(string(), string(), string(), string(), string(), string()) -> binary(). 
create_http_header_maker(Host, "GET", "", Path, Query, Headers) ->
    First = "GET " ++ Path ++ Query,  
    HostHeader = make_host_header(Host),
    fun (Data) ->
        flush([list_to_binary(First), Data, list_to_binary(" HTTP/1.1"), <<13,10>>, HostHeader, Headers, <<13,10>>])
    end;
create_http_header_maker(Host, "GET", Param, Path, Query, Headers) ->
    First = "GET " ++ Path ++ Query ++ "&" ++ Param ++ "=",    
    HostHeader = make_host_header(Host),
    fun (Data) ->
        flush([list_to_binary(First), Data, list_to_binary(" HTTP/1.1"), <<13,10>>, HostHeader, Headers, <<13,10>>])
    end;
create_http_header_maker(Host, "POST", _, Path, Query, Headers) ->
    First = "POST " ++ Path ++ Query,   
    HostHeader = make_host_header(Host), 
    fun (Data) ->
        CL = "Content-length: " ++ integer_to_list(byte_size(Data)) ++ [13,10],    
        flush([list_to_binary(First), list_to_binary(" HTTP/1.1"), <<13,10>>, Headers, HostHeader, list_to_binary(CL), <<13,10>>, Data])
    end.

http_headers([H|T], Acc) ->
    http_headers(T, [list_to_binary(H), <<13,10>> | Acc]);
http_headers([], Acc) ->
    flush(Acc).
    
%% TODO: FIXME: check correct format of data (http domain name?)
-spec make_http_writer(inet:ip_address(), inet:port_number(), string(), string(), string(), string(), list(string())) -> fun().
make_http_writer(Host, Port, Path, Query, Type, Param, Options) ->
    Maker = create_http_header_maker(Host, Type, Param, Path, Query, http_headers(Options, [])), %% TODO:fixme with re
    fun F(_N, Meta) -> 
        {F, {http, 
            fun (Data) -> 
                Packet = Maker(Data),
                {ok, Sock} = gen_tcp:connect(Host, Port, [binary, {active, true}], ?TCP_TIMEOUT),
                gen_tcp:send(Sock, Packet),
                gen_tcp:close(Sock)
            end
        },
        [{output, http} | Meta]}
    end.

-spec string_outputs(string()) -> fun().
string_outputs(Str) ->
    case Str of
        "-" -> fun stdout_stream/2;
        return -> fun return_stream/2;
        {tcp, {Addr, Port}} -> tcpsock_writer(Addr, list_to_integer(Port));
        {udp, {Addr, Port}} -> udpsock_writer(Addr, list_to_integer(Port));
        {ip, {Addr, Proto}} -> ipsock_writer(Addr, list_to_integer(Proto));
        {raw, {Addr, Iface}} -> rawsock_writer(Addr, Iface); 
        {http, Params} -> http_writer(Params);
        _Else -> file_writer(Str)
    end.

%% write blocks to port
-spec blocks_port(lazy_list_of_bins(), output_dest(), fun()) -> {lazy_list_of_bins(), list(), non_neg_integer()}.
blocks_port(Ll, Fd, Post) -> blocks_port(Ll, Fd, [], 0, Post).

%% TODO: UGLY, need rewrite and handle errors
-spec blocks_port(lazy_list_of_bins(), output_dest(), list(), non_neg_integer(), fun()) -> {lazy_list_of_bins(), list(), non_neg_integer()}.
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
close_port(Data, {http, Final}) -> Final(Data);
close_port(_, Fd) -> file:close(Fd).

%% write to Fd or stdout
%% TODO: UGLY, need rewrite and handle errors, also rewrite spec
%%write_really(Data, stdout) -> io:put_chars("\n\n********************* write_really stdout: "), io:write({byte_size(Data)}), io:put_chars(Data), io:put_chars("\n\n"), ok;
%%write_really(Data, Fd) -> io:put_chars("\n\n********************* write_really file: "), file:write(Fd, {byte_size(Data)}), io:put_chars(Data), io:put_chars("\n\n"), ok.
-spec write_really(binary(), output_dest()) -> {any(), binary()}.
write_really(Data, return) -> {ok, Data};
write_really(Data, stdout) -> {file:write(standard_io, Data), <<>>};
write_really(Data, {http, _Final}) -> {ok, Data};
write_really(Data, {net, Writer, _Closer}) -> {Writer(Data), <<>>};
write_really(Data, Fd) -> {file:write(Fd, Data), <<>>}.
