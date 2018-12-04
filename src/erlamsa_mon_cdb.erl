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
%%% Monitor for Windows/CDB console debugger.
%%% @end
%%%-------------------------------------------------------------------
-module(erlamsa_mon_cdb).

-include("erlamsa.hrl").

-export([init/1, start/1, init_port/3]).

%%TODO:add launch in addition to attach
parse_params(["pid=" ++ App|T], Acc) ->
    parse_params(T, maps:put(run, io_lib:format("-p ~s", [App]), Acc));
parse_params(["attach=" ++ App|T], Acc) ->
    parse_params(T, maps:put(run, io_lib:format("-pn ~s", [App]), Acc));
parse_params(["app=" ++ App|T], Acc) ->
    parse_params(T, maps:put(run, io_lib:format("~s", [App]), Acc));
parse_params([_H|T], Acc) ->
    %%TODO: do it in more elegant way
    io:format("Invalid monitor parameter: ~p, skipping...", [T]),
    parse_params(T, Acc);
parse_params([], Acc) ->
    Acc.

start(Params) ->
    Pid = spawn(?MODULE, init, [Params]),
    {ok, Pid}.

init(Params) ->
    {GenericMonOpts, LeftParams} = erlamsa_monitor:parse_after(string:tokens(Params, ",")),
    MonOpts = parse_params(LeftParams, GenericMonOpts),
    cdb_start(MonOpts, ?START_MONITOR_ATTEMPTS).

cdb_start(_MonOpts, 0) ->
    erlamsa_logger:log(error, "cdb_monitor: too many failures (~p), giving up",
                        [?START_MONITOR_ATTEMPTS]);
cdb_start(MonOpts, N) ->
    erlamsa_logger:log(info, "entering cdb_monitor, options parsing complete", []),
    Cmd = io_lib:format("cdb ~s", [maps:get(run, MonOpts, "no_app_provided")]),
    erlamsa_logger:log(info, "cdb_monitor attempting to run: '~s'", [Cmd]),
    {Pid, StartResult} = start_port(Cmd, []), %% TODO:ugly, rewrite
    cdb_cmdline(MonOpts, Pid, StartResult, N).

cdb_cmdline(MonOpts, _Pid, {State, Acc}, N) when State =:= closed; State =:= process_exit ->
    erlamsa_logger:log(warning, "cdb_monitor error (~p): '~s'", [State, Acc]),
    cdb_start(MonOpts, N-1);
cdb_cmdline(MonOpts, Pid, StartResult, _N) ->
    erlamsa_logger:log(info, "cdb_monitor execution returned, seems legit: '~s'", [StartResult]),
    CrashMsg = call_port(Pid, "g\r\n"),
    erlamsa_logger:log(finding, "cdb_monitor [-->!!!<--] detected event (CRASH?!): ~s", [CrashMsg]),
    Backtrace = call_port(Pid, "k\r\n"),
    erlamsa_logger:log(finding, "cdb_monitor backtrace: ~s", [Backtrace]),
    Registers = call_port(Pid, "r\r\n"),
    erlamsa_logger:log(finding, "cdb_monitor registers: ~s", [Registers]),
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_local_time(erlang:now()),
    DumpFile = io_lib:format("~s_~4..0w_~2..0w_~2..0w_~2..0w_~2..0w_~2..0w.minidump",
                             [maps:get(app, MonOpts, ""), Year, Month, Day, Hour, Minute, Second]),
    Minidump = call_port(Pid, io_lib:format(".dump /m ~s \r\n", [DumpFile])),
    erlamsa_logger:log(finding, "cdb_monitor minidump saved to ~s with result: ~s",
                             [DumpFile, Minidump]),
    call_port_no_wait(Pid, "q\r\n"),
    stop_port(Pid),
    erlamsa_logger:log(info, "cdb_monitor cdb finished.", []),
    erlamsa_logger:log(info, "cdb_monitor executing after actions", []),
    erlamsa_monitor:do_after(MonOpts),
    cdb_start(MonOpts, ?START_MONITOR_ATTEMPTS).


start_port(ExtPrg, ExtraParams) ->
    Pid = spawn(?MODULE, init_port, [ExtPrg, ExtraParams, self()]),
    receive
        {Pid, Result} ->
            {Pid, Result}
    end.

stop_port(Pid) ->
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
            {data, Data};
    {Port, closed} ->
        {closed};
    {'EXIT', Port, Reason} ->
        {process_exit, Reason}
    end.

read_cdb_data(Port) ->
    read_cdb_data(Port, none, []).
read_cdb_data(_Port, "> ", Acc) ->
    lists:flatten(lists:reverse(Acc));
read_cdb_data(Port, _Any, Acc) ->
    case receive_port(Port) of
    {data, Data} ->
        read_cdb_data(Port, lists:nthtail(length(Data) - 2, Data), [Data | Acc]);
    {closed} ->
        {closed, lists:flatten(lists:reverse(Acc))};
    {process_exit, Reason} ->
        {process_exit, lists:flatten(lists:reverse(Acc))}
    end.

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
    {'EXIT', Port, Reason} ->
        Port ! {self(), process_exit},
        exit(port_terminated)
    end.

init_port(ExtPrg, ExtraParams, RetPid) ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [use_stdio, stderr_to_stdout, stream, hide | ExtraParams]),
    Data = read_cdb_data(Port),
    RetPid ! {self(), Data},
    loop(Port).



