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
%%% Command line args parser.
%%% @end
%%%-------------------------------------------------------------------

-module(erlamsa_cmdparse).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

-include("erlamsa.hrl").

% API
-export([parse/1, usage/0, parse_seed/1, string_to_actions/3]).

about() ->
    "Erlamsa is an erlang port of famous Radamsa fuzzer. Radamsa is
a general purpose fuzzer. It modifies given sample data in ways,
which might expose errors in programs intended to process the data.
Erlamsa use same fuzzing engine as Radamsa, however extends functionality
with additional features like proxy, JSON fuzzing service, generational
fuzzing, SGML fuzzer, structure/format detectors and more.

Radamsa was written by Aki Helin at OUSPG.
Erlamsa is written by Alexander Bolshev (@dark_k3y).~n".

inputs() ->
    [
        {"filename1.txt filename2.txt ...", "data will be read from file(s) with specified name(s)"},
        {"-i proto://lport:rhost:rport", "erlamsa will work in fuzzing proxy mode, listenting on lport and sending fuzzed data to rhost:port; tcp,udp,tls,http(s) are supported"},
        {"-i proto://proxy:lport", "erlamsa will work in standalone http proxy mode, listenting on lport and fuzzing data that goes over proxy"},
        {"-H addr:port", "erlamsa will listen on <addr:port> for HTTP POST queries with data, sending fuzzing result in reply, fuzzing options are passed via HTTP headers"}
    ].

outputs() ->
    [
        {"filename_iter%n.txt", "data will be written to files with template filename_iter%n.txt, where %n will be replaced by current interation number"},
        {"tcp://ipaddr:port", "send fuzzed data to remote tcp <port> located at <ipaddr>"},
        {"udp://[[ifaceip:]srcport:]ipaddr:port", "send fuzzed data to remote udp <port> located at <ipaddr> (listening on <ifaceip>:<srcport>)"},
        {"[tcp|udp]://:port", "listens on tcp or udp <port> and sends fuzzed data upon client connect/send message"},
        {"http://:port,[Content-Type]", "simple HTTP server on <port> that sends fuzzed data upon client's request"},
        {"http://addr[:port]/path?params,[GET|POST],header1,...", "send fuzzed data to remote http host located at addr"},
        {"[https|tls]://...", "same as http and tcp, but data is TLS-wrapped"},
        {"ip://ipaddr:proto", "send fuzzed data to remote host located at <ipaddr> using protocol no. <proto> on top of IP (Linux & OS X)"},
        {"raw://ipaddr:iface", "send fuzzed data to remote host located at <ipaddr> raw protocol, outgoing interface is specified with <iface> (Linux only)"},
        {"serial://device,baud", "send fuzzed data to serial <device> (e.g. /dev/ttyUSB0) using <baud> (e.g. 57600) (Linux & OS X)"},
        {"exec://apppathwithargs", "execute app from apppath and send fuzzed data to its stdin (Linux & OS X)"}
        ].

%% GF-base modes:

cmdline_optsspec() ->
[
    {ascent 	, $A,	"ascent",		{float, 1.0},			"<arg>, fuzzing proxy: use ascent/descent coefficient for fuzzing probability (TCP/HTTP only)"},
    {about		, $a, 	"about", 		undefined, 				"what is this thing"},
    {blockscale , $b, 	"blockscale", 	{float, 1.0},			"<arg>, increase/decrease default min (256 bytes) fuzzed blocksize multiplier"},
    {bypass		, $B,	"bypass",		{integer, 0},			"<arg>, fuzzing proxy: bypass first <arg> packets before start fuzzing (TCP/HTTP only)"},
    {certfile	, undefined,
                        "certfile",		string,			        "<arg>, certificate file for fuzzing TLS-based communications"},
    {cloudsvc	, undefined,
                        "cloudsvc",		{string, "nil"},	    "<arg>, activate cloudservice with <arg> as a management token"},                        
    {debug		, $d,	"debug",		undefined,				"run in debug/profiler mode, activates verbose"},
    {detach	    , $D,	"detach",		undefined,			    "detach from console after start (service mode)"},
    {external	, $e,   "external", 	string,					"external pre/post/generation/mutation module"},
    {faildelay	, undefined,
                        "faildelay", 	{integer, 0},			"<arg>, additional delay (in ms.) after failed attempt to output data to the network"},
    {generators , $g,	"generators",	{string, erlamsa_gen:tostring(erlamsa_gen:generators())},
                                                                "<arg>, which data generators to use"},
    {genfuzz	, $G,	"genfuzz",		float,					"<arg>, activate generation-based fuzzer, arg is base probablity"},
    {help		, $h, 	"help", 		undefined, 				"show this thing"},
    % ,{heXinput	, $X, 	"hexinput",		undefined,    			"treat input data as a hex string"},
    {hexoutput	, undefined, 	
                        "hexoutput",	undefined,    			"output data in logs as hex"},
    {httpsvc    , $H,   "httpservice",  string,				    "<arg>, run as HTTP service on <host:port>, e.g.: 127.0.0.1:17771"},
    {input		, $i, 	"input",		string, 				"<arg>, special input, e.g. proto://lport:[udpclientport:]rhost:rport (fuzzing proxy) or proto://:port, proto://host:port for data endpoint (generation mode)"},
    {keyfile	, undefined,
                        "keyfile",		string,			        "<arg>, key file for fuzzing TLS-based communications"},
    {list		, $l,	"list",			undefined,				"list i/o options, monitors, mutations, patterns and generators"},
    {logger		, $L,	"logger",		string,					"<arg>, logger options, e.g. level=critical..debug, file=filename, csv=filename.csv, mnesia=dir or stdout (-) or stderr (-err)"},
    {maxfails,    undefined,
                        "maxfails",		{integer, 10},		    "<arg>, maximum failed attempts to output data to the network before giving up"},
    {maxrunningtime
                , undefined, "maxrunningtime",
                                        {integer, 30}, 		    "<arg>, maximum running time for fuzzing instance (service/proxy modes only)"},
    {meta		, $M, 	"meta",			{string, "nil"},		"<arg>, save metadata about fuzzing process to this file or stdout (-) or stderr (-err)"},
    {mnesia		, undefined, 	
                        "mnesia",		{string, "./mnesia"},	"<arg>, mnesia directory for logging or cloud service"},
    {mutations  , $m,   "mutations",	{string, erlamsa_mutations:tostring(erlamsa_mutations:mutations())},
                                                                "<arg>, which mutations to use"},
    {count		, $n, 	"count",		{integer, 1},			"<arg>, how many outputs to generate (number or inf)"},
    {noiolog   	, undefined,
                        "no-io-logging",undefined,				"disable logging of incoming and outgoing data"},
    {monitor	, $O,   "monitor",      string,					"+-<arg>, add/remove monitor (use additional -O for each monitor"},
    {output		, $o, 	"output",		{string, "-"}, 			"<arg>, output pattern, e.g. /tmp/fuzz-%n.foo, -, [proto]://192.168.0.1:80 or [proto]://:80 [-]"},
    {patterns	, $p,	"patterns",		{string, erlamsa_patterns:tostring(erlamsa_patterns:patterns())},
                                                                "<arg>, which mutation patterns to use"},
    {pidfile	, undefined,
                        "pidfile",		string,                 "<arg>, PID file name"},
    {proxyprob	, $P,	"probability",	string,					"<arg>, activate fuzzing proxy mode, param is fuzzing probability in form of s->c,c->s e.g.: 0.5,0.5"},
    {recursive  , $r,	"recursive",	undefined, 				"include files in subdirectories"},
    {seed		, $s, 	"seed",			string, 				"<arg>, random seed in erlang format: int,int,int or source:device for an external source of entropy (e.g. binary file)"},
    {sleep		, $S, 	"sleep",		{integer, 0},			"<arg>, sleep time (in ms.) between output iterations"},
    {stroutput	, undefined, 	
                        "stroutput",	undefined,    			"force output data in logs as ASCII-strings"},
    {verbose	, $v,	"verbose",		{integer, 0},			"be more verbose, show some progress during generation"},
    {version	, $V, 	"version",		undefined, 				"show program version"},
    {workers	, $w, 	"workers",		integer,    			"<arg>, number of working threads (1 for standalone, 10 for proxy/fass)"},
    {workers_same_seed, undefined, 
                        "workers-same-seed", undefined, 		"run all workers with the same seed, each to generate n/w samples"}
].

usage() ->
    getopt:usage(cmdline_optsspec(), "erlamsa", "[file ...]").

fail(Reason) ->
    io:format("Error: ~s~n~n", [Reason]),
    usage(),
    halt(1).

show_list(Dict) ->
    IOToStr =
            fun({N, D}, Acc) ->
                [io_lib:format("    ~s: ~s~n",[N,D])|Acc]
            end,
    Is = lists:foldl(IOToStr, [], inputs()),
    Os = lists:foldl(IOToStr, [], outputs()),
    Mons = lists:foldl(
            fun({N, _M, D}, Acc) ->
                [io_lib:format("    ~-6s: ~s~n",[atom_to_list(N),D])|Acc]
            end
        ,[], erlamsa_monitor:monitors()),
    Gs = lists:foldl(
            fun({N, _P, D}, Acc) ->
                [io_lib:format("    ~-6s: ~s~n",[atom_to_list(N),D])|Acc]
            end
        ,[],
        lists:sort(fun ({_,N1,_}, {_,N2,_}) -> N1 =< N2 end,
            erlamsa_gen:generators())),
    CustomMutas = erlamsa_utils:make_mutas(maps:get(external_mutations, Dict, nil)),
    Ms = lists:foldl(fun({_,_,_,N,D}, Acc) ->
                        [io_lib:format("    ~-3s: ~s~n",[atom_to_list(N),D])|Acc]
                     end, [],
                     lists:sort(fun ({_,_,_,N1,_}, {_,_,_,N2,_}) -> N1 >= N2 end,
                     erlamsa_mutations:mutations(CustomMutas))),
    Ps = lists:foldr(fun({_,_,N,D}, Acc) ->
                        [io_lib:format("    ~-3s: ~s~n",[atom_to_list(N),D])|Acc]
                     end, [],
                     erlamsa_patterns:patterns()),
    io:format("Inputs ~n~s~nOutputs (-o)~n~s~n", [Is, Os]),
    io:format("Monitors (-O, see README for info on parameters) ~n~s~n", [Mons]),
    io:format("Generators (-g)~n~s~nMutations (-m)~n~s~nPatterns (-p)~n~s", [Gs, Ms, Ps]).

set_defaults(Dict) ->
    Workers =
        case maps:get(mode, Dict, stdio) of
            genfuzz ->   3;
            proxy   -> 	10;
            stdio   ->   1;
            faas 	->	10;
            _Else   ->	 1
        end,

    maps:put(workers, maps:get(workers, Dict, Workers),
        maps:put(monitor, maps:get(monitor, Dict, erlamsa_monitor:default()),
            Dict)).

parse_actions(List, OptionName, Default, Dict) ->
    case string_to_actions(List, atom_to_list(OptionName), Default) of
        {ok, L} ->
            maps:put(OptionName, L, Dict);
        {fail, Reason} ->
            fail(Reason)
    end.

-spec resolve_host(string()) -> inet:ip_address().
resolve_host(Host) ->
    case inet:getaddr(Host, inet) of
        {ok, Ip} ->
            Ip;
        {error, Reason} ->
            throw(Reason)
    end.

-spec port_check(tcp, integer(), string()) -> free | busy.
port_check(tcp, PortNum, Host) ->
    try gen_tcp:listen(list_to_integer(PortNum), [{ip, resolve_host(Host)}]) of
        {ok, LSocket} ->
            gen_tcp:close(LSocket),
            ok;
        {error, eaddrinuse} ->
            io:format("Could not listen on port ~s: alreasy in use. ~n", [PortNum]),
            halt(1);
        {error, Reason} ->
            io:format("Invalid host/port: ~s:~s, error: ~p~n", [Host, PortNum, Reason]),
            halt(1)
    catch
        error:badarg ->
            io:format("Invalid port specifiction: '~s'~n", [PortNum]),
            halt(1);
        nxdomain ->
            io:format("Invalid hort specifiction or unexisting domain: '~s'~n", [Host]),
            halt(1)
    end.

-spec string_to_actions(string(), string(), [tuple()]) -> {ok, [tuple()]} | {fail, string()}.
string_to_actions(Lst, What, DefaultLst) ->
    Tokens = string:tokens(Lst, ","),
    Default = maps:from_list(DefaultLst),
    try
        {ok, string_to_action_loop([string:tokens(X, "=") || X <- Tokens], Default, [])}
    catch
        error:badarg ->
            {fail, "Invalid " ++ What ++ " list specification!"};
        notfound ->
            {fail, "No such " ++ What ++ "!"}
    end.

-spec string_to_action_loop([list(string())], maps:map(), list()) -> [{atom(), non_neg_integer()}].
string_to_action_loop([H|T], Default, Acc) ->
    Name = list_to_atom(hd(H)),
    Item = process_action(Name, H, maps:get(Name, Default, notfound)),
    string_to_action_loop(T, Default, [Item | Acc]);
string_to_action_loop([], _Default, Acc) -> Acc.

process_action(_Name, _, notfound) ->
    throw(notfound);
process_action(Name, [_, Pri], _DefaultPri) ->
    {Name, list_to_integer(Pri)};
process_action(Name, _, DefaultPri) ->
    {Name, DefaultPri}.

parse_logger_opts(LogOpts, Dict) ->
    parse_logger_opt(string:tokens(LogOpts, ","), Dict).

parse_logger_opt(["-"|T], Dict) ->
    parse_logger_opt(T, maps:put(logger_stdout, stdout, Dict));
parse_logger_opt(["stdout"|T], Dict) ->
    parse_logger_opt(T, maps:put(logger_stdout, stdout, Dict));
parse_logger_opt(["stderr"|T], Dict) ->
    parse_logger_opt(T, maps:put(logger_stderr, stderr, Dict));
parse_logger_opt(["-err"|T], Dict) ->
    parse_logger_opt(T, maps:put(logger_stderr, stderr, Dict));
parse_logger_opt([], Dict) ->
    Dict;
parse_logger_opt([LogOpt|T], Dict) ->
    case string:tokens(LogOpt, "=") of
        ["level", Lvl] ->
            parse_logger_opt(T, maps:put(logger_level,
                                         list_to_atom(string:to_lower(Lvl)), Dict));
        ["file", FName] ->
            parse_logger_opt(T, maps:put(logger_file, FName, Dict));
        ["csv", FName] ->
            parse_logger_opt(T, maps:put(logger_csv, FName, Dict));
        ["syslog", Uri] ->
            [Host, SPort] = string:tokens(Uri, ":"),
            IP = inet:ntoa(Host),
            Port = list_to_integer(SPort),
            parse_logger_opt(T, maps:put(logger_syslog, {IP, Port}, Dict));
        ["mnesia"] ->
            parse_logger_opt(T, maps:put(logger_mnesia, true, Dict));
        _Else -> fail(io_lib:format("invalid logger specification: '~s'", [LogOpt]))
    end.

parse_proxyprob_opts(ProxyProbOpts, Dict) ->
    case string:tokens(ProxyProbOpts, ",") of
        [SC, CS] ->
            %%TODO: in future, allow set different probabilities for different proxies
            %%maps:put(proxy_probs, [{list_to_float(SC), list_to_float(CS)} | maps:get(proxy_probs, Dict, [])], Dict);
            maps:put(proxy_probs, {list_to_float(SC), list_to_float(CS)}, Dict);
        _Else -> fail(io_lib:format("invalid proxy fuzzing probability specification: '~s'", [ProxyProbOpts]))
    end.

parse_input_opts(InputOpts, Dict) ->
    case string:tokens(InputOpts, ":") of
        [Proto, "//proxy", ListenPort] ->
            maps:put(proxy_address,
                [{Proto,
                list_to_integer(ListenPort),
                ?DEFAULT_UDPPROXY_CLIENTPORT,
                nil, nil} | maps:get(proxy_address, Dict, [])],
                maps:put(mode, proxy, maps:put(httpproxy, true, Dict)));
        [Proto, ListenPort] ->
            maps:put(input_endpoint, {Proto,
                list_to_integer(hd(string:tokens(ListenPort, "/")))}, Dict);
        [Proto, LPortD, RHost, RPort] ->
            maps:put(proxy_address,
                [{Proto,
                list_to_integer(hd(string:tokens(LPortD, "/"))),
                ?DEFAULT_UDPPROXY_CLIENTPORT,
                erlamsa_utils:resolve_addr(RHost),
                list_to_integer(RPort)}
                | maps:get(proxy_address, Dict, [])],
            maps:put(mode, proxy, Dict));
        ["serial", [$/, $/ | Port1], Opt1, Port2, Opt2] ->
                maps:put(proxy_address,
                [{"serial", Port1, Opt1, Port2, Opt2}
                | maps:get(proxy_address, Dict, [])],
                maps:put(mode, proxy, Dict));
        ["udp", LPortD, UDPClientPort, RHost, RPort] ->
                maps:put(proxy_address,
                [{"udp",
                list_to_integer(hd(string:tokens(LPortD, "/"))),
                UDPClientPort,
                erlamsa_utils:resolve_addr(RHost),
                list_to_integer(RPort)}
                | maps:get(proxy_address, Dict, [])],
                maps:put(mode, proxy, Dict));
        _Else -> fail(io_lib:format("invalid input specification: '~s'", [InputOpts]))
    end.

parse_sock_addr(SockType, Addr) ->
    Tokens = string:tokens(Addr, ":"),
    case length(Tokens) of
        N when N > 0, N < 5 ->
            {SockType, list_to_tuple(Tokens)};
        _Else ->
            fail(io_lib:format("invalid socket address specification: '~s'", [Addr]))
    end.

parse_serial_addr(Addr) ->
    Tokens = string:tokens(Addr, ","),
    case length(Tokens) of
        N when N > 1, N < 5 ->
            {serial, list_to_tuple(Tokens)};
        _Else ->
            fail(io_lib:format("invalid serial address specification: '~s'", [Addr]))
    end.

parse_exec(App) ->
    {exec, App}.

%% TODO: catch exception in case of incorrect...
parse_http_addr(URL) ->
    Tokens = string:tokens(URL, ","),
    {ok, {Scheme, _UserInfo, Host, Port, Path, Query}} = http_uri:parse(hd(Tokens)),
    {Scheme, {Host, Port, Path, Query, tl(Tokens)}}.

parse_url([<<"udp">>|T], _URL) ->
    parse_sock_addr(udp, binary_to_list(hd(T)));
parse_url([<<"ip">>|T], _URL) ->
    parse_sock_addr(ip, binary_to_list(hd(T)));
parse_url([<<"raw">>|T], _URL) ->
    parse_sock_addr(raw, binary_to_list(hd(T)));
parse_url([<<"tcp">>|T], _URL) ->
    parse_sock_addr(tcp, binary_to_list(hd(T)));
parse_url([<<"tls">>|T], _URL) ->
    parse_sock_addr(tls, binary_to_list(hd(T)));
parse_url([<<"serial">>|T], _URL) ->
    parse_serial_addr(binary_to_list(hd(T)));
parse_url([<<"exec">>|T], _URL) ->
    parse_exec(binary_to_list(hd(T)));
parse_url([<<"http">>|_T], URL) ->
    parse_http_addr(URL);
parse_url([<<"http2">>|_T], URL) ->
    parse_http_addr(URL);
parse_url([<<"https">>|_T], URL) ->
    parse_http_addr(URL);
%parse_url([<<"exec">>|_T], URL) ->
%    parse_exec(URL);
parse_url(_, URL) ->
    fail(io_lib:format("invalid output URL specification: '~s'", [URL])).

parse_output(Output) ->
    {ok, SRe} = re:compile(":\\/\\/"),
    Tokens = re:split(Output, SRe),
    case length(Tokens) of
        1 ->
            Output; %% file or stdout
        2 ->
            parse_url(Tokens, Output); %% url
        _Else ->
            fail(io_lib:format("invalid output specification: '~s'", [Output]))
    end.

convert_metapath("nil") -> nil;
convert_metapath("stdout") -> stdout;
convert_metapath("-") -> stdout;
convert_metapath("stderr") -> stderr;
convert_metapath("-err") -> stderr;
convert_metapath(Path) -> Path.

% convert_possiblenil("nil") -> nil;
% convert_possiblenil(Arg) -> Arg.

convert_token("nil") -> nil;
convert_token(Arg) -> erlamsa_cmanager:decode_token(Arg).

parse_seed(Seed) ->
    list_to_tuple([list_to_integer(X) || X <- string:tokens(Seed, ",")]).

parse_seed_opt("source:" ++ Source, Dict) ->
        maps:put(seed, erlamsa_rnd_ext:get_seed(),
                maps:put(ext_rnd, fun(Opts) -> erlamsa_rnd_ext:get_supervisor_opts(Opts) end,
                        maps:put(ext_rnd_source, Source, Dict)));
parse_seed_opt(Seed, Dict) ->
    try
        maps:put(seed, parse_seed(Seed), Dict)
    catch
        error:badarg ->
            fail("Invalid seed format! Usage: int,int,int")
    end.

parse_monitor(Lst = [H | T]) when length(Lst) > 2 ->
    parse_monitor([H, string:join(T, ":")]);
parse_monitor(Lst) when length(Lst) =/= 2 ->
    fail("Incorrect monitor specification! Usage: +/-monitor:params");
parse_monitor(Lst) ->
    AddRemove = hd(hd(Lst)), MonitorName = tl(hd(Lst)), MonitorParams = hd(tl(Lst)),
    {Action, Name, Params} = case AddRemove of
        $+ -> {plus, list_to_atom(MonitorName), MonitorParams};
        $! -> {minus, list_to_atom(MonitorName), ""};
        C -> {plus, list_to_atom([C | MonitorName]), MonitorParams}
    end,
    case lists:foldl(fun ({N, _F}, Acc) when Name == N -> [Name | Acc]; (_, Acc) -> Acc end,
                erlamsa_monitor:monitors(), []) of
        [] -> fail(io_lib:format("Unknown monitor name: ~p", [Name]));
        _Else -> {Action, Name, Params}
    end.

addmodule2external(ListName, Module, Dict) ->
    maps:put(ListName, lists:flatten([Module | maps:get(ListName, Dict, [])]), Dict).

parse_external([], Dict) ->
    Dict;
parse_external([Module|T], Dict) ->
    {Type, _} = erlang:apply(list_to_atom(Module), capabilities, []),
    NewDict = case Type of
        mutations -> addmodule2external(external_mutations, Module, Dict);
        post -> maps:put(external_post, Module, Dict);
        generator -> maps:put(external_generator, Module, Dict);
        fuzzer -> maps:put(external_fuzzer, Module, Dict);
        monitor -> addmodule2external(external_monitors, Module, Dict);
        logger -> addmodule2external(external_loggers, Module, Dict);
        pattern -> addmodule2external(external_patterns, Module, Dict);
        _Else -> fail(io_lib:format("Incorrect module capability: ~p", [Type]))
    end,
    parse_external(T, NewDict).

parse(Args) ->
    case getopt:parse(cmdline_optsspec(), Args) of
        {ok, {Opts, Files}} -> parse_tokens(Opts, Files);
        _Else -> usage(), halt(1)
    end.

parse_tokens(Opts, []) ->
    parse_opts(Opts, maps:put(paths, ["-"], maps:new()));
parse_tokens(Opts, Paths) ->
    parse_opts(Opts, maps:put(paths, Paths, maps:new())).

parse_opts([help|_T], _Dict) ->
    usage(),
    halt(0);
parse_opts([{httpsvc, HostPort}|T], Dict) ->
    case string:tokens(HostPort, ":") of
        [Host, Port] ->
            port_check(tcp, Port, Host),
            parse_opts(T, maps:put(svchost, Host,
                    maps:put(svcport, list_to_integer(Port),
                        maps:put(mode, httpsvc, Dict))));
        _ -> fail("Invalid listen endpoint format, usage host:port")
    end;
parse_opts([version|_T], _Dict) ->
    io:format("Erlamsa ~s ~s~n", [?VERSION, ?GITVER]),
    halt(0);
parse_opts([about|_T], _Dict) ->
    io:format(about(), []),
    halt(0);
parse_opts([list|_T], Dict) ->
    show_list(Dict),
    halt(0);
parse_opts([{monitor, MonitorSpec}|T], Dict) ->
    %%Syntax is monitor_name:params
    %%TODO: In future replace with string:split(MonitorSpec, ":", leading)
    Monitor = parse_monitor(string:tokens(MonitorSpec, ":")),
    parse_opts(T, maps:put(monitor,
                            [Monitor | maps:get(monitor, Dict, erlamsa_monitor:default())],
                            Dict));
parse_opts([{ascent, DC}|T], Dict) ->
    parse_opts(T, maps:put(descent_coeff, DC, Dict));
parse_opts([{bypass, DC}|T], Dict) ->
    parse_opts(T, maps:put(bypass, DC, Dict));
parse_opts([{verbose, Lvl}|T], Dict) ->
    parse_opts(T, maps:put(verbose, Lvl, Dict));
parse_opts([debug|T], Dict) ->
    parse_opts(T, maps:put(debug, debug, maps:put(verbose, 10, maps:put(logger_level, debug, Dict))));
parse_opts([{meta, Path}|T], Dict) ->
    parse_opts(T, maps:put(metadata, convert_metapath(Path), Dict));
parse_opts([{cloudsvc, Token}|T], Dict) ->
    parse_opts(T, maps:put(cloudsvc, convert_token(Token), Dict));
parse_opts([recursive|T], Dict) ->
    parse_opts(T, maps:put(recursive, true, Dict));
parse_opts([{count, N}|T], Dict) ->
    parse_opts(T, maps:put(n, N, Dict));
parse_opts([hexoutput|T], Dict) ->
    parse_opts(T, maps:put(dataoutput, hex, Dict));
parse_opts([workers_same_seed|T], Dict) ->
    parse_opts(T, maps:put(workers_same_seed, true, Dict));
parse_opts([stroutput|T], Dict) ->
    parse_opts(T, maps:put(dataoutput, str, Dict));
parse_opts([{certfile, CertFile}|T], Dict) ->
    parse_opts(T, maps:put(certfile, CertFile, Dict));
parse_opts([{keyfile, KeyFile}|T], Dict) ->
    parse_opts(T, maps:put(keyfile, KeyFile, Dict));
parse_opts([{mnesia, MnesiaDir}|T], Dict) ->
    parse_opts(T, maps:put(mnesia_dir, MnesiaDir, Dict));
parse_opts([{pidfile, PidFile}|T], Dict) ->
    parse_opts(T, maps:put(pidfile, PidFile, Dict));
parse_opts([{maxrunningtime, MT}|T], Dict) ->
    parse_opts(T, maps:put(maxrunningtime, MT, Dict));
parse_opts([{maxfails, MT}|T], Dict) ->
    parse_opts(T, maps:put(maxfails, MT, Dict));
parse_opts([{blockscale, B}|T], Dict) ->
    parse_opts(T, maps:put(blockscale, B, Dict));
parse_opts([{genfuzz, BP}|T], Dict) ->
    parse_opts(T, maps:put(mode, genfuzz, maps:put(genfuzz, BP, Dict)));
parse_opts([{workers, W}|T], Dict) ->
    parse_opts(T, maps:put(workers, W, Dict));
parse_opts([{faildelay, FailDelay}|T], Dict) ->
    parse_opts(T, maps:put(faildelay, FailDelay, Dict));
parse_opts([{sleep, Sleep}|T], Dict) ->
    parse_opts(T, maps:put(sleep, Sleep, Dict));
parse_opts([{logger, LogOpts}|T], Dict) ->
    parse_opts(T, parse_logger_opts(LogOpts, Dict));
parse_opts([noiolog|T], Dict) ->
    parse_opts(T, maps:put(noiolog, true, Dict));
parse_opts([{external, ModuleNames}|T], Dict) ->
    parse_opts(T, parse_external(string:tokens(ModuleNames, ","), Dict));
parse_opts([{proxyprob, ProxyProbOpts}|T], Dict) ->
    parse_opts(T, parse_proxyprob_opts(ProxyProbOpts, Dict));
parse_opts([{input, InputOpts}|T], Dict) ->
    parse_opts(T, parse_input_opts(InputOpts, Dict));
%% TODO: ugly solution, forces user to pass external module BEFORE list of priorities, fix in future when cmd interface will be rewritten
parse_opts([{mutations, Mutators}|T], Dict) ->
    CustomMutas = erlamsa_utils:make_mutas(maps:get(external_mutations, Dict, nil)),
    parse_opts(T, parse_actions(Mutators, mutations, erlamsa_mutations:default(CustomMutas), Dict));
parse_opts([{patterns, Patterns}|T], Dict) ->
    parse_opts(T, parse_actions(Patterns, patterns, erlamsa_patterns:default(), Dict));
parse_opts([{generators, Generators}|T], Dict) ->
    parse_opts(T, parse_actions(Generators, generators, erlamsa_gen:default(), Dict));
parse_opts([{output, OutputOpts}|T], Dict) ->
     parse_opts(T, maps:put(output, parse_output(OutputOpts), Dict));
parse_opts([{seed, SeedOpts}|T], Dict) ->
    parse_opts(T, parse_seed_opt(SeedOpts, Dict));
parse_opts([detach|T], Dict) ->
    parse_opts(T, Dict);
parse_opts([_|T], Dict) ->
    parse_opts(T, Dict);
parse_opts([], Dict) ->
    set_defaults(Dict).
