-module(erlamsa_mon_cdb).           
-export([launch/1, start/1, start/2, stop/1, init/3, call_port/2]).

parse_params(["after_params=" ++ AfterParam|T], Acc) ->
    parse_params(T, maps:put(do_after_params, AfterParam, Acc));
parse_params(["after=" ++ AfterType|T], Acc) ->
    parse_params(T, maps:put(do_after_type, list_to_atom(AfterType), Acc));
%%TODO:add launch in addition to attach
parse_params(["app=" ++ App|T], Acc) ->
    parse_params(T, maps:put(app, App, Acc));
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

launch(Params) ->
    spawn(?MODULE, start, [Params]).

start(Params) -> 
    MonOpts = parse_params(string:split(Params,",",all), maps:new()),
    %%TODO:handle wrong params
    erlamsa_logger:log(info, "entering cdb_monitor, options parsing complete", []),
    Cmd = io_lib:format("cdb -p ~s", [maps:get(app, MonOpts, "")]),
    erlamsa_logger:log(info, "cdb_monitor attempting to run: '~s'", [Cmd]),
    {Pid, StartResult} = start(Cmd, []),
    erlamsa_logger:log(info, "cdb_monitor execution returned, seems legit: '~s'", [StartResult]),
    CrashMsg = call_port(Pid, "g\r\n"), 
    erlamsa_logger:log(info, "cdb_monitor [-->!!!<--] detected event (CRASH?!): ~s", [CrashMsg]),
    Backtrace = call_port(Pid, "k\r\n"),
    erlamsa_logger:log(info, "cdb_monitor backtrace: ~s", [Backtrace]),
    Registers = call_port(Pid, "r\r\n"),
    erlamsa_logger:log(info, "cdb_monitor registers: ~s", [Registers]),
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_local_time(erlang:now()),
    DumpFile = io_lib:format("~s_~4..0w_~2..0w_~2..0w_~2..0w_~2..0w_~2..0w.minidump", [maps:get(app, MonOpts, ""), Year, Month, Day, Hour, Minute, Second]),
    Minidump = call_port(Pid, io_lib:format(".dump /m ~s \r\n", [DumpFile])),
    erlamsa_logger:log(info, "cdb_monitor minidump saved to ~s with result: ~s", [DumpFile, Minidump]),
    call_port_no_wait(Pid, "q\r\n"),
    stop(Pid),
    erlamsa_logger:log(info, "cdb_monitor cdb finished.", []),
    erlamsa_logger:log(info, "cdb_monitor executing after actions", []),
    do_after(maps:get(do_after_type, MonOpts, nil), MonOpts),
    start(Params). %% TODO:ugly, rewrite

start(ExtPrg, ExtraParams) ->
    Pid = spawn(?MODULE, init, [ExtPrg, ExtraParams, self()]),
    receive
        {Pid, Result} ->
            {Pid, Result}
    end.

stop(Pid) ->
    Pid ! stop.

call_port(Pid, Msg) ->
    Pid ! {call, self(), Msg},
    receive
        {Pid, Result} ->
            Result
    end.

call_port_no_wait(Pid, Msg) ->
    Pid ! {call, self(), Msg}.

receive_port(Port) ->
    receive
		{Port, {data, Data}} ->
            Data
	end.

%%TODO:FIXME:Very ugly, thx to r2 weirdiness, this may be slow and unefficient.
read_cdb_data(Port) ->
    read_cdb_data(Port, none, []).
read_cdb_data(_Port, "> ", Acc) ->
    %io:format("Finished: ~p~n~n", [Acc]),
    lists:flatten(lists:reverse(Acc));
read_cdb_data(Port, _Any, Acc) ->
    Data = receive_port(Port),
    %io:format("Data: ~p~n~n", [Dat]),
    read_cdb_data(Port, lists:nthtail(length(Data) - 2, Data), [Data | Acc]).


loop(Port) ->
    receive
	{call, Caller, Msg} ->
	    Port ! {self(), {command, Msg}},
	    %io:format("Reading data back...~n~n"),
      	    Caller ! {self(), read_cdb_data(Port)},
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

init(ExtPrg, ExtraParams, RetPid) ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [use_stdio, stderr_to_stdout, stream, hide | ExtraParams]),
    Data = read_cdb_data(Port),
    %io:format("ready!"),
    RetPid ! {self(), Data},
    loop(Port). 



