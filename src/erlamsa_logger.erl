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
%%% Logger service process.
%%% @end
%%%-------------------------------------------------------------------

-module(erlamsa_logger).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

-include("erlamsa.hrl").

% API
-export([get_timestamp/0, get_supervisor_opts/1, start/1, logger/1, get_pid/0, log/3, log_data/4, loglvl_toint/1]).

%% log levels:
%% critical, error, warning, finding, info, meta, decision, debug
loglvl_toint(debug) -> ?DEBUG;
loglvl_toint(decision) -> ?DECISION;
loglvl_toint(meta) -> ?META;
loglvl_toint(info) -> ?INFO;
loglvl_toint(finding) -> ?FINDING;
loglvl_toint(warning) -> ?WARNING;
loglvl_toint(error) -> ?ERROR;
loglvl_toint(critical) -> ?CRITICAL;
loglvl_toint(Int) when is_integer(Int) -> Int.

get_supervisor_opts(Opts) ->
    #{id => ?MODULE,
    start => {?MODULE, start, [build_logger(Opts)]},
    restart => permanent,
    shutdown => brutal_kill,
    type => worker,
    modules => [?MODULE]}.

log(Type, Fmt, Lst) ->
    try
        global:send(logger, {log, self(), Type, Fmt, Lst, <<>>})
    catch
        exit: {badarg, {_Name, _Msg}} -> badarg
    end.

log_data(Type, Fmt, Lst, Data) ->
    try
        global:send(logger, {log, self(), Type, Fmt, Lst, Data})
    catch
        exit: {badarg, {_Name, _Msg}} -> badarg
    end.

%% crash if fail
get_pid() ->
    global:send(logger, {get_pid, self()}),
    receive
        {pid, Pid} -> Pid
    end.

get_timestamp() ->
    {_, _, Ms} = os:timestamp(),
    {Y, M, D} = date(),
    {H, Min, S} = time(),
    io_lib:format('~4..0b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b.~3..0b',
                  [Y, M, D, H, Min, S, round(Ms/1000)]).

remove_newlines_from_data(Data) ->
    [X || X <- io_lib:format("~p", [Data]), X =/= 10].


%%
%% Basic I/O and file logger
%%
append_to_io(IO, TimeStamp, Pid, Type, LogMsg, Data) when is_atom(IO) ->
    io:format(IO, "~s <~p> [~p]: ~s~s~n", [TimeStamp, Pid, Type, LogMsg, Data]);
append_to_io(FileName, TimeStamp, Pid, Type, LogMsg, Data) when is_list(FileName) ->
    file:write_file(FileName, io_lib:format("~s <~p> [~p]: ~s~s~n",
                    [TimeStamp, Pid, Type, LogMsg, Data]), [append]).

%% by default, data logging is now enabled --> none is default value
build_logger_io(IO) ->
    fun
        (TimeStamp, Pid, Type, LogMsg, none, DataLen) when DataLen > 0 ->
            append_to_io(IO, TimeStamp, Pid, Type, LogMsg,
                         io_lib:format(" :[data_len = ~p, data ommited]", [DataLen]));
        (TimeStamp, Pid, Type, LogMsg, Data, DataLen) when DataLen > 0, DataLen < ?MAX_LOG_DATA ->
            append_to_io(IO, TimeStamp, Pid, Type, LogMsg,
                         io_lib:format(" [data_len = ~p]: ~p", [DataLen, Data]));
        (TimeStamp, Pid, Type, LogMsg, _Data, DataLen) when DataLen > 0 ->
            append_to_io(IO, TimeStamp, Pid, Type, LogMsg,
                         io_lib:format(" [skipping large output, data len = ~p]", [DataLen]));
        (TimeStamp, Pid, Type, LogMsg, _Data, _DataLen) ->
            append_to_io(IO, TimeStamp, Pid, Type, LogMsg, [])
    end.

%%
%% Syslog logger
%%

%% log levels to syslog conversion
loglvl_syslog(debug) -> 7;
loglvl_syslog(decision) -> 6;
loglvl_syslog(meta) -> 6;
loglvl_syslog(info) -> 6;
loglvl_syslog(finding) -> 5;
loglvl_syslog(warning) -> 4;
loglvl_syslog(error) -> 3;
loglvl_syslog(critical) -> 2.

send_to_syslog({IP, Port}, TimeStamp, Pid, Type, LogMsg, Data) ->
    Msg = list_to_binary(io_lib:format("~s erlamsa ~p [~p] : ~s ~s~n", [TimeStamp, Pid, Type, LogMsg, Data])),
    Level = loglvl_syslog(Type),
    Packet = <<16:5,Level:3,Msg/binary>>,
    {ok, Socket} = gen_udp:open(0,[inet]),
    en_udp:send(Socket, IP, Port, Packet),
    gen_udp:close(Socket),
    ok.

build_logger_syslog(none) ->
    [];
build_logger_syslog(Uri) ->
    fun
        (TimeStamp, Pid, Type, LogMsg, Data, DataLen) when DataLen > 0 ->
            send_to_syslog(Uri, TimeStamp, Pid, Type, LogMsg,
                          io_lib:format(" [data_len = ~p]", [byte_size(Data)]));
        (TimeStamp, Pid, Type, LogMsg, _Data, _DataLen) ->
            send_to_syslog(Uri, TimeStamp, Pid, Type, LogMsg, "")
    end.

%%
%% CSV Logger
%%
append_to_csv(FileName, TimeStamp, Pid, Type, LogMsg, Data) when is_list(FileName) ->
    file:write_file(FileName,
                    io_lib:format("~s,~p,~p,\"~s\",~s~n", [TimeStamp, Pid, Type, LogMsg, Data]), [append]).

%% FIXME: TODO: output data len if >= ?MAX_LOG_DATA
%% by default, data logging is now enabled --> none is default value
build_logger_csv(none) ->
    [];
build_logger_csv(FName) ->
    fun
        (TimeStamp, Pid, Type, LogMsg, Data, DataLen) when DataLen > 0, DataLen < ?MAX_LOG_DATA ->
            append_to_csv(FName, TimeStamp, Pid, Type, LogMsg,
                          io_lib:format("~p,~s", [DataLen,
                                        erlamsa_utils:bin_to_hexstr(Data)]));
        (TimeStamp, Pid, Type, LogMsg, _Data, DataLen) when DataLen > 0 ->
            append_to_csv(FName, TimeStamp, Pid, Type, LogMsg,
                          io_lib:format("~p", [DataLen]));                                
        (TimeStamp, Pid, Type, LogMsg, _Data, _DataLen) ->
            append_to_csv(FName, TimeStamp, Pid, Type, LogMsg, ",")
    end.

%%
%% Mnesia logger
%%

build_logger_mnesia(none) ->
    [];
build_logger_mnesia(_MnesiaEnabled) ->
    mnesia:create_table(log_entry, [{attributes, record_info(fields, log_entry)}]),
    fun
        (TimeStamp, Pid, Type, LogMsg, Data, _DataLen) ->
            mnesia:transaction(fun () ->
                mnesia:write(#log_entry{date = TimeStamp, pid = Pid, type = Type,  message = LogMsg, data = Data})
            end)
    end.

%%
%% Build loggers based on options
%%
build_logger(Opts) ->
    Loggers =
        lists:flatten([
            build_logger_console(maps:get(logger_stdout, Opts, none)),
            build_logger_console(maps:get(logger_stderr, Opts, none)),
            build_logger_file(maps:get(logger_file, Opts, none)),
            build_logger_csv(maps:get(logger_csv, Opts, none)),
            build_logger_syslog(maps:get(logger_syslog, Opts, none)),
            build_logger_mnesia(maps:get(logger_mnesia, Opts, none))
        ]),
    ProcessData = 
        case maps:get(noiolog, Opts, none) of 
            none ->
                %%TODO: FIXME: [[]] <-- not good, handle upper, ugly fix
                case maps:get(dataoutput, Opts, none) of 
                    hex -> fun ([]) -> {<<>>, 0}; (Data) -> {erlamsa_utils:bin_to_hexstr(Data), byte_size(Data)}  end;
                    str -> fun ([]) -> {<<>>, 0}; (Data) -> {lists:flatten(io_lib:format("~s", [Data])), byte_size(Data)}  end;
                    _Else -> fun ([]) -> {<<>>, 0}; (Data) -> {Data, byte_size(Data)} end
                end;
            _Else ->
                fun (Data) -> {none, byte_size(Data)} end
        end,
    OutputToLog =
        fun (Pid, Type, Fmt, Lst, Data) ->
            TimeStamp = get_timestamp(),
            LogMsg = io_lib:format(Fmt, Lst),
            {ProcessedData, DataLen} = ProcessData(Data),
            [F(TimeStamp, Pid, Type, LogMsg, ProcessedData, DataLen) || F <- Loggers],
            ok
        end,
    V = loglvl_toint(maps:get(logger_level, Opts, ?INFO)),
    fun 
        (Pid, Type, TypeInt, Fmt, Lst, Data) when TypeInt =< V -> OutputToLog(Pid, Type, Fmt, Lst, Data);
        (_Pid, _Type, _TypeInt, _Fmt, _Lst, _Data) -> ok
    end.

build_logger_file(none) ->
    [];
build_logger_file([]) ->
    build_logger_io("./erlamsa.log");
build_logger_file(FileName) ->
    build_logger_io(FileName).

build_logger_console(none) ->
    [];
build_logger_console(stdout) ->
    build_logger_io(standard_io);
build_logger_console(stderr) ->
    build_logger_io(standard_error).

start(Log) ->
    Pid = spawn(erlamsa_logger, logger, [Log]),
    global:register_name(logger, Pid),
    {ok, Pid}.

logger(Log) ->
    receive
        {log, Pid, Type, Fmt, Lst, Data} ->
            Log(Pid, Type, loglvl_toint(Type), Fmt, Lst, Data);
        {get_pid, Pid} ->
            Pid ! {pid, self()}
    end,
    logger(Log).