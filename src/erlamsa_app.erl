-module(erlamsa_app).

-behaviour(application).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Application callbacks
-export([start/2, stop/1, start/1, loop/1, call/2, fuzz/1, fuzz/2]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

get_supervisor_opts(Opts) ->								
	[#{id => ?MODULE,
	start => {?MODULE, start, [Opts]},
	restart => permanent,
	shutdown => brutal_kill,
	type => worker,
	modules => [?MODULE]}].

sleep() ->
	timer:sleep(infinity).

prepare_auxproc(Dict) ->
    [
		erlamsa_logger:get_supervisor_opts(Dict),
		erlamsa_fsupervisor:get_supervisor_opts(Dict),
		erlamsa_monitor:get_supervisor_opts(Dict)	
	].

start_behaviour(Dict) -> 
    case maps:get(mode, Dict, stdio) of
        genfuzz -> %%TODO: make genfuzz supevisable
            {[], fun () -> erlamsa_gfcomms:start(Dict), sleep() end};
        proxy ->     	
            {[erlamsa_fuzzproxy:get_supervisor_opts(Dict)], fun sleep/0};
        stdio ->    		
            {[], fun () -> erlamsa_main:fuzzer(Dict), timer:sleep(1) end};
        httpsvc ->
            {[erlamsa_httpsvc:get_supervisor_opts(Dict)], fun sleep/0};
        faas ->
            io:format("Mode not supported yet!"),
            exit(normal);
        _Else -> 
            io:format("Invalid mode."),
            exit(normal)
	end.

start(escript, Dict) ->
	AuxProcesses = prepare_auxproc(Dict),
	{MainProcess, StartFunc} = start_behaviour(Dict),
	erlamsa_sup:start_link(lists:flatten([AuxProcesses | MainProcess])),
	%% Profiler should not be supervised
	erlamsa_profiler:start(maps:get(debug, Dict, nil), [erlamsa_logger:get_pid()]),
	StartFunc();
start(direct, Dict) ->
	AuxProcesses = prepare_auxproc(Dict),
    MainProcess = get_supervisor_opts(Dict),
	erlamsa_sup:start_link(lists:flatten([AuxProcesses | MainProcess])),
	%% Profiler should not be supervised
	erlamsa_profiler:start(maps:get(debug, Dict, nil), [erlamsa_logger:get_pid()]);
start(remote, {Pid, Node}) ->
    {Pid, Node} ! {test, self()};
start(_StartType, _StartArgs) ->
    throw("Unknown start type!~n").

start(Opts) ->
    Pid = spawn(?MODULE, loop, [Opts]),
    global:register_name(erlamsa, Pid),
    {ok, Pid}.

loop(Opts) ->
    receive 
        {fuzz, Client, Data} ->
            NewOpts = maps:put(paths, [direct],
                maps:put(output, return,
                maps:put(input, Data, Opts))),
            Client ! {fuzzing_ok, erlamsa_fsupervisor:get_fuzzing_output(NewOpts)};
        {test, Client} -> ok
    end,
    loop(Opts).

-spec call(pid(), binary()) -> binary().
call(Node, Data) ->
    global:send(Node, {fuzz, self(), Data}),
    receive 
        {fuzzing_ok, Result} -> Result
    end.

-spec fuzz(binary()) -> binary().
fuzz(Data) -> 
    fuzz(Data, maps:new()).

-spec fuzz(binary(), map()) -> binary().
fuzz(Data, Dict) -> 
    Opts = maps:put(paths, [direct],
            maps:put(output, return,
             maps:put(input, Data, Dict))),
    MutatedData = erlamsa_utils:extract_function(erlamsa_main:fuzzer(Opts)),
    MutatedData.   

stop(_State) ->
    ok.
