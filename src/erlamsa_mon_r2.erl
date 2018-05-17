-module(erlamsa_mon_r2).           
-export([start/1, start/2, stop/1, init/2, call_port/2]).

parse_params(["after_params=" ++ AfterParam|T], Acc) ->
    parse_params(T, maps:put(do_after_params, AfterParam, Acc));
parse_params(["after=" ++ AfterType|T], Acc) ->
    parse_params(T, maps:put(do_after_type, list_to_atom(AfterType), Acc));
%%TODO:add launch in addition to attach
parse_params(["app=" ++ App|T], Acc) ->
    parse_params(T, maps:put(app, App, Acc));
parse_params(["r2path=" ++ R2Path|T], Acc) ->
    parse_params(T, maps:put(r2path, R2Path, Acc));
parse_params(["platform=" ++ Platform|T], Acc) ->
    parse_params(T, maps:put(platform, list_to_atom(Platform), Acc));
parse_params([_H|T], Acc) ->
    %%TODO: do it in more elegant way
    io:format("Invalid monitor parameter: ~p, skipping...", [T]),
    parse_params(T, Acc);
parse_params([], Acc) ->
    Acc.

do_after(exec, Opts) ->
    ExecPath = maps:get(do_after_params, Opts, ""),
    os:cmd(ExecPath); %%TODO: add result to logs
do_after(nil, _Opts) -> ok.

start(Params) -> 
    MonOpts = parse_params(string:split(Params,",",all), maps:new()),
    %%TODO:handle wrong params
    R2Cmd = io_lib:format("~s -q0 -d ~s", [maps:get(r2path, MonOpts, "r2"), maps:get(app, MonOpts, "")]),
    erlamsa_logger:log(info, "r2monitor attempting to run: '~s'", [R2Cmd]),
    R2Pid = start(R2Cmd, []), %%TODO: windows platform handling
    io:format("!!!!!!!\n"),
    Res = call_port(R2Pid, "ij\r\n"), %%autoanalysis
    io:format("/!!!!!!!\n"),
    io:format("Tmpres: ~s", [Res]),
    Res = call_port(R2Pid, "aaa\n"), %%autoanalysis
    io:format("Tmpres: ~s", [Res]),
    Res2 = call_port(R2Pid, "dc\n"), %%TODO: handle not started state
    io:format("Tmpres: ~s", [Res2]),
    CrashMsg = call_port(R2Pid, "dc\n"), %% in loop run dc until everything is ok
    %% here there is crash
    erlamsa_logger:log(info, "r2monitor [-->!!!<--] detected event (CRASH?!): ~sp", [CrashMsg]),
    RegInfoMsg = call_port(R2Pid, "drj\n"),
    erlamsa_logger:log(info, "r2monitor registers info ~s", [RegInfoMsg]),
    Info = call_port(R2Pid, "ij\n"),
    erlamsa_logger:log(info, "r2monitor executable info ~s", [Info]),
    MemMaps = call_port(R2Pid, "dmj\n"),
    erlamsa_logger:log_data(info, "r2monitor memory maps ~s", [], MemMaps),
    %BackTraceJson = erlamsa_simple_port:call_port("dbtj\n"),
    %erlamsa_logger:log(info, "r2monitor json backtrace ~s", [BackTraceJson]),
    BackTrace = call_port(R2Pid, "dbt\n"),
    erlamsa_logger:log(info, "r2monitor backtrace ~s", [BackTrace]),
    stop(R2Pid),
    do_after(maps:get(do_after_type, MonOpts, nil), MonOpts),
    start(MonOpts). %% TODO:ugly, rewrite

start(ExtPrg, ExtraParams) ->
    spawn(?MODULE, init, [ExtPrg, ExtraParams]).
stop(Pid) ->
    Pid ! stop.

call_port(Pid, Msg) ->
    Pid ! {call, self(), Msg},
    receive
        {Pid, Result} ->
            Result
    end.

receive_port(Port) ->
    receive
		{Port, {data, Data}} ->
            Data
	end.

%%TODO:FIXME:Very ugly, thx to r2 weirdiness, this may be slow and unefficient.
read_r2_data(Port) ->
    read_r2_data(Port, none, []).
read_r2_data(Port, 0, [[0]] ) ->
    Data = receive_port(Port),
    read_r2_data(Port, lists:nth(length(Data), Data), [Data]);
read_r2_data(_Port, 0, Acc) ->
    lists:droplast(lists:flatten(lists:reverse(Acc)));
read_r2_data(Port, _Any, Acc) ->
    Data = receive_port(Port),
    read_r2_data(Port, lists:nth(length(Data), Data), [Data | Acc]).

r2_ready(Port, start) ->
    [H|T] = receive_port(Port),
    r2_ready(Port, H);
r2_ready(Port, 0) ->
    ok.

loop(Port) ->
    receive
	{call, Caller, Msg} ->
	    Port ! {self(), {command, Msg}},
      	    Caller ! {erlamsa_mon_r2, read_r2_data(Port)},
	    loop(Port);
	stop ->
	    Port ! {self(), close},
	    receive
            {Port, closed} ->
                exit(normal)
	    end;
	{'EXIT', Port, _Reason} ->
	    exit(port_terminated)
    end.   

init(ExtPrg, ExtraParams) ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [use_stdio, stderr_to_stdout, hide, stream | ExtraParams]),
    ok = r2_ready(Port, start),
    io:format("r2 ready!"),
    loop(Port). 



