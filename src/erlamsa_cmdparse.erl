% Copyright (c) 2011-2014 Aki Helin
% Copyright (c) 2014-2015 Alexander Bolshev aka dark_k3y
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
%%% Cmd args parser.
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
fuzzing, SGML fuzzer and more.

Radamsa was written by Aki Helin at OUSPG.
Erlamsa is written by Alexander Bolshev (@dark_k3y).~n".

inputs() ->
	[
		{"filename1.txt filename2.txt ...", "data will be read from file(s) with specified name(s)"},
		{"-i lport:rhost:rport", "erlamsa will work in fuzzing proxy mode (currently tcp only), listenting on lport and sending fuzzed data to rhost:port"},
		{"-H host:port", "erlamsa will listen on <host:port> for HTTP POST queries with data, sending fuzzing result in reply, fuzzing options are passed via HTTP headers"}
	].

outputs() ->
	[
		{"filename_iter%n.txt", "data will be written to files with template filename_iter%n.txt, where %n will be replaced by current interation number"},
		{"[tcp|udp]://ipaddr:port", "send fuzzed data to remote tcp or udp <port> located at <ipaddr>"},
		{"[tcp|udp]://:port", "listens on tcp or udp <port> and send fuzzed data upon client connect/send message"},
		{"http://addr[:port]/path?params,[GET|POST],header1,...", "send fuzzed date to remote http host located at addr"},
		{"ip://ipaddr:proto", "send fuzzed data to remote host located at <ipaddr> using protocol no. <proto> on top of IP (Linux & OS X)"},
		{"raw://ipaddr:iface", "send fuzzed data to remote host located at <ipaddr> raw protocol, outgoing interface is specified with <iface> (Linux only)"}
		].

%% GF-base modes:

cmdline_optsspec() ->
[
	{about		, $a, 	"about", 		undefined, 				"what is this thing"},
	{blockscale , $b, 	"blockscale",	{float, 1.0},			"<arg>, increase/decrease default min (256 bytes) fuzzed blocksize multiplier"},
	{bypass		, $B,	"bypass",		{integer, 0},			"<arg>, fuzzing proxy: bypass first <arg> packets before start fuzzing (TCP/HTTP only)"},
	{debug		, $d,	"debug",		undefined,				"run in debug/profiler mode, activates verbose"},
	{descent	, $D,	"descent",		{float, 1.0},			"<arg>, fuzzing proxy: use ascent/descent coefficient for fuzzing probability (TCP/HTTP only)"},	
	{external	, $e,   "external", 	string,					"external pre/post/generation/mutation module"},	 
	{generators , $g,	"generators",	{string,
 						erlamsa_gen:tostring(		erlamsa_gen:generators())},		"<arg>, which data generators to use"},
	{genfuzz	, $G,	"genfuzz",		float,					"<arg>, activate generation-based fuzzer, arg is base probablity"},
 	{help		, $h, 	"help", 		undefined, 				"show this thing"},
	{httpsvc    , $H,   "httpservice",  string,				    "<arg>, run as HTTP service on <host:port>, e.g.: 127.0.0.1:17771"},
	{input		, $i, 	"input",		string, 				"<arg>, special input, e.g. proto://lport:[udpclientport:]rhost:rport (fuzzing proxy) or proto://:port, proto://host:port for data endpoint (generation mode/FAAS)"},
 	{list		, $l,	"list",			undefined,				"list i/o options, mutations, patterns and generators"},
	{logger		, $L,	"logger",		string,					"<arg>, which logger to use, e.g. file=filename, csv=filename.csv, mnesia=dir or stdout (-) or stderr (-err)"},
	{meta		, $M, 	"meta",			{string, "nil"},		"<arg>, save metadata about fuzzing process to this file or stdout (-) or stderr (-err)"},
	{mutations  , $m,   "mutations",	{string,
	  				erlamsa_mutations:tostring(	erlamsa_mutations:mutations())}, 	"<arg>, which mutations to use"},
	{count		, $n, 	"count",		{integer, 1},			"<arg>, how many outputs to generate (number or inf)"},
	{noiolog   	, $N,   "no-io-logging",undefined,				"disable logging of incoming and outgoing data"},
	{monitor	, $O,   "monitor",      string,					"<arg>, monitor specification"},
	{output		, $o, 	"output",		{string, "-"}, 			"<arg>, output pattern, e.g. /tmp/fuzz-%n.foo, -, tcp://192.168.0.1:80 or udp://127.0.0.1:53 or ip://172.16.0.1:47 or http://example.com or tcp://:80 or udp://:123 [-]"},
	{patterns	, $p,	"patterns",		{string,
					erlamsa_patterns:tostring(	erlamsa_patterns:patterns())},	"<arg>, which mutation patterns to use"},
	{proxyprob	, $P,	"proxy",		string,					"<arg>, activate fuzzing proxy mode, param is fuzzing probability in form of s->c,c->s e.g.: 0.5,0.5"},
%	 {recursive , $r,	"recursive",	undefined, 				"include files in subdirectories"},	 
	{seed		, $s, 	"seed",			string, 				"<arg>, random seed in erlang format: int,int,int"},
	{sleep		, $S, 	"sleep",		{integer, 0},			"<arg>, sleep time (in ms.) between output iterations"},	 
	{maxrunningtime
	 			, $t, 	"maxrunningtime",
				 						{integer, 30}, 			"<arg>, maximum running time for fuzzing instance (service/proxy modes only)"},		  
	{verbose	, $v,	"verbose",		{integer, 0},			"be more verbose, show some progress during generation"},	 
	{version	, $V, 	"version",		undefined, 				"show program version"},
	{workers	, $w, 	"workers",		integer, 				"<arg>, number of working threads"}
].

usage() ->
	getopt:usage(cmdline_optsspec(), "erlamsa", "[file ...]").

fail(Reason) ->
	io:format("Error: ~s~n~n", [Reason]),
	usage(),
	halt(1).

parse_actions(List, OptionName, Default, Dict) ->
	case string_to_actions(List, atom_to_list(OptionName), Default) of
		{ok, L} ->
			maps:put(OptionName, L, Dict);
		{fail, Reason} ->
			fail(Reason)
	end.

set_defaults(Dict) ->
	Workers = 
		case maps:get(mode, Dict, stdio) of
			genfuzz ->   3;
			proxy   -> 	10;    	
			stdio   ->   1;
			faas 	->	10;	
			_Else   ->	 1
		end,
	maps:put(workers, maps:get(workers, Dict, Workers), Dict).

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
%% TODO: check if mutation name exist
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
		["file", FName] ->
			parse_logger_opt(T, maps:put(logger_file, FName, Dict));
		["csv", FName] ->
			parse_logger_opt(T, maps:put(logger_csv, FName, Dict));
		["mnesia", FName] ->
			parse_logger_opt(T, maps:put(logger_mnesia, FName, Dict));
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
		2 ->
			{SockType, {hd(Tokens), hd(tl(Tokens))}};
		1 ->
			{SockType, {hd(Tokens)}};
		_Else ->
			fail(io_lib:format("invalid socket address specification: '~s'", [Addr]))
	end.

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
parse_url([<<"http">>|_T], URL) ->
	parse_http_addr(URL);
parse_url(_, URL) ->
	fail(io_lib:format("invalid URL specification: '~s'", [URL])).

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

parse_seed(Seed) ->
    list_to_tuple([list_to_integer(X) || X <- string:tokens(Seed, ",")]).
parse_seed_opt(Seed, Dict) ->
	try
		maps:put(seed, parse_seed(Seed), Dict)
	catch
        error:badarg ->
        	fail("Invalid seed format! Usage: int,int,int")
    end.

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
			parse_opts(T, maps:put(svchost, Host,
					maps:put(svcport, list_to_integer(Port),
						maps:put(mode, httpsvc, Dict))));
		_ -> fail("Invalid listen endpoint format, usage host:port")
	end;
parse_opts([version|_T], _Dict) ->
	io:format("Erlamsa ~s~n", [?VERSION]),
	halt(0);
parse_opts([about|_T], _Dict) ->
	io:format(about(), []),
	halt(0);
parse_opts([list|_T], _Dict) ->
	Is = lists:foldl(
			fun({N, D}, Acc) ->
				[io_lib:format("    ~s: ~s~n",[N,D])|Acc]
			end
		,[], inputs()),
	Os = lists:foldl(
			fun({N, D}, Acc) ->
				[io_lib:format("    ~s: ~s~n",[N,D])|Acc]
			end
		,[], outputs()),
	Gs = lists:foldl(
			fun({N, _P, D}, Acc) ->
				[io_lib:format("    ~-6s: ~s~n",[atom_to_list(N),D])|Acc]
			end
		,[],
		lists:sort(fun ({_,N1,_}, {_,N2,_}) -> N1 =< N2 end,
			erlamsa_gen:generators())),
	Ms = lists:foldl(
			fun({_,_,_,N,D}, Acc) ->
				[io_lib:format("    ~-3s: ~s~n",[atom_to_list(N),D])|Acc]
			end
		,[],
		lists:sort(fun ({_,_,_,N1,_}, {_,_,_,N2,_}) -> N1 >= N2 end,
			erlamsa_mutations:mutations())),
	Ps = lists:foldl(
			fun({_,_,N,D}, Acc) ->
				[io_lib:format("    ~-3s: ~s~n",[atom_to_list(N),D])|Acc]
			end
		,[],
		erlamsa_patterns:patterns()),
	io:format("Inputs ~n~s~nOutputs (-o)~n~s~nGenerators (-g)~n~s~nMutations (-m)~n~s~nPatterns (-p)~n~s",
		[Is, Os, Gs, Ms, Ps]),
	halt(0);
parse_opts([{monitor, MonitorSpec}|T], Dict) ->
	%%Syntax is monitor_name:params 
	%%TODO: temporary solution, only r2 monitor is supported now
	parse_opts(T, maps:put(monitor, {cdb,MonitorSpec}, Dict));	
parse_opts([{descent, DC}|T], Dict) ->
	parse_opts(T, maps:put(descent_coeff, DC, Dict));
parse_opts([{bypass, DC}|T], Dict) ->
	parse_opts(T, maps:put(bypass, DC, Dict));
parse_opts([{verbose, Lvl}|T], Dict) ->
	parse_opts(T, maps:put(verbose, Lvl, Dict));
parse_opts([debug|T], Dict) ->
	parse_opts(T, maps:put(debug, debug, maps:put(verbose, 10, Dict)));
parse_opts([{meta, Path}|T], Dict) ->
	parse_opts(T, maps:put(metadata, convert_metapath(Path), Dict));	
parse_opts([recursive|T], Dict) ->
	parse_opts(T, maps:put(recursive, 1, Dict));
parse_opts([{count, N}|T], Dict) ->
	parse_opts(T, maps:put(n, N, Dict));
parse_opts([{maxrunningtime, MT}|T], Dict) ->
	parse_opts(T, maps:put(maxrunningtime, MT, Dict));	
parse_opts([{blockscale, B}|T], Dict) ->
	parse_opts(T, maps:put(blockscale, B, Dict));
parse_opts([{genfuzz, BP}|T], Dict) ->
	parse_opts(T, maps:put(mode, genfuzz, maps:put(genfuzz, BP, Dict)));		
parse_opts([{workers, W}|T], Dict) ->
	parse_opts(T, maps:put(workers, W, Dict));
parse_opts([{sleep, Sleep}|T], Dict) ->
	parse_opts(T, maps:put(sleep, Sleep, Dict));
parse_opts([{logger, LogOpts}|T], Dict) ->
	parse_opts(T, parse_logger_opts(LogOpts, Dict));
parse_opts([noiolog|T], Dict) ->
	parse_opts(T, maps:put(noiolog, true, Dict));
parse_opts([{external, ModuleName}|T], Dict) ->
	parse_opts(T, maps:put(external, ModuleName, Dict));	
parse_opts([{proxyprob, ProxyProbOpts}|T], Dict) ->
	parse_opts(T, parse_proxyprob_opts(ProxyProbOpts, Dict));
parse_opts([{input, InputOpts}|T], Dict) ->
	parse_opts(T, parse_input_opts(InputOpts, Dict));
%% TODO: ugly solution, forces user to pass external module BEFORE list of priorities, fix in future when cmd interface will be rewritten
parse_opts([{mutations, Mutators}|T], Dict) ->
	CustomMutas = erlamsa_utils:make_mutas(maps:get(external, Dict, nil)),
	parse_opts(T, parse_actions(Mutators, mutations, erlamsa_mutations:default(CustomMutas), Dict));
parse_opts([{patterns, Patterns}|T], Dict) ->
	parse_opts(T, parse_actions(Patterns, patterns, erlamsa_patterns:default(), Dict));
parse_opts([{generators, Generators}|T], Dict) ->
	parse_opts(T, parse_actions(Generators, generators, erlamsa_gen:default(), Dict));
parse_opts([{output, OutputOpts}|T], Dict) ->
 	parse_opts(T, maps:put(output, parse_output(OutputOpts), Dict));
parse_opts([{seed, SeedOpts}|T], Dict) ->
	parse_opts(T, parse_seed_opt(SeedOpts, Dict));
parse_opts([_|T], Dict) ->
	parse_opts(T, Dict);
parse_opts([], Dict) ->
	set_defaults(Dict).
