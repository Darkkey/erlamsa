% Copyright (c) 2011-2014 Aki Helin
% Copyright (c) 2014-2018 Alexander Bolshev aka dark_k3y
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
%%% Main application file: implementation of process.
%%% @end
%%%-------------------------------------------------------------------

-module(erlamsa_app).

-behaviour(application).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("erlamsa.hrl").

%% Module types
-type start_type() :: escript | direct | remote | atom().

%% Application callbacks
-export([main/0, main/1, start/2, stop/1, start/1, loop/1, call/2, fuzz/1, fuzz/2]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec main() -> no_return().
main() ->
    Dict = erlamsa_cmdparse:parse(init:get_plain_arguments()),
	erlamsa_app:start(escript, Dict).

-spec main(list(string())) -> no_return().
main(Dict) ->
	erlamsa_app:start(escript, Dict).

-spec get_supervisor_opts(options()) -> supervisor:child_spec().
get_supervisor_opts(Opts) ->
    [#{id => ?MODULE,
    start => {?MODULE, start, [Opts]},
    restart => permanent,
    shutdown => brutal_kill,
    type => worker,
    modules => [?MODULE]}].

-spec sleep() -> ok.
sleep() ->
    timer:sleep(infinity), ok.

-spec prepare_auxproc(options()) -> list(supervisor:child_spec()).
prepare_auxproc(Dict) ->
    [
        erlamsa_logger:get_supervisor_opts(Dict),
        erlamsa_fsupervisor:get_supervisor_opts(Dict),
        erlamsa_monitor:get_supervisor_opts(Dict)
    ].

-spec start_behaviour(options()) -> {list(supervisor:child_spec()), fun()}.
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

%TODO: refactor a little, to avoid duplicating code
-spec start(start_type(), options()) -> ok.
start(escript, Dict) ->
    ets:new(global_config, [public, named_table, bag]),
    AuxProcesses = prepare_auxproc(Dict),
    {MainProcess, StartFunc} = start_behaviour(Dict),
    erlamsa_sup:start_link(lists:flatten([AuxProcesses | MainProcess])),
    %% Profiler should not be supervised
    erlamsa_profiler:start(maps:get(debug, Dict, nil), [erlamsa_logger:get_pid()]),
    StartFunc();
start(direct, Dict) ->
    ets:new(global_config, [named_table, bag]),
    AuxProcesses = prepare_auxproc(Dict),
    MainProcess = get_supervisor_opts(Dict),
    erlamsa_sup:start_link(lists:flatten([AuxProcesses | MainProcess])),
    %% Profiler should not be supervised
    erlamsa_profiler:start(maps:get(debug, Dict, nil), [erlamsa_logger:get_pid()]);
start(remote, {Pid, Node}) ->
    {Pid, Node} ! {test, self()};
start(_StartType, _StartArgs) ->
    throw("Unknown start type!~n").

-spec start(options()) -> {ok, pid()}.
start(Opts) ->
    Pid = spawn(?MODULE, loop, [Opts]),
    global:register_name(erlamsa, Pid),
    {ok, Pid}.

-spec loop(options()) -> ok.
loop(Opts) ->
    receive
        {fuzz, Client, Data} ->
            NewOpts = erlamsa_utils:get_direct_fuzzing_opts(Data, Opts),
            Client ! {fuzzing_ok, erlamsa_fsupervisor:get_fuzzing_output(NewOpts)};
        {test, _Client} -> ok
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
    Opts = erlamsa_utils:get_direct_fuzzing_opts(Data, Dict),
    MutatedData = erlamsa_utils:extract_function(erlamsa_main:fuzzer(Opts)),
    MutatedData.

-spec stop(any()) -> ok.
stop(_State) ->
    ok.
