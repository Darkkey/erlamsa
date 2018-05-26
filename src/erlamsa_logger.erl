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
%%% Logger.
%%% @end
%%%-------------------------------------------------------------------

-module(erlamsa_logger).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

-include("erlamsa.hrl").

% API
-export([get_timestamp/0, get_supervisor_opts/1, start/1, logger/1, get_pid/0, log/3, log_data/4]).

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
    io_lib:format('~4..0b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b.~3..0b', [Y, M, D, H, Min, S, round(Ms/1000)]).

append_to_io(IO, TimeStamp, Pid, LogMsg, Data) when is_atom(IO) ->
	io:format(IO, "~s <~p>: ~s~s~n", [TimeStamp, Pid, LogMsg, Data]); 
append_to_io(FileName, TimeStamp, Pid, LogMsg, Data) when is_list(FileName) -> 
	file:write_file(FileName, io_lib:format("~s <~p>: ~s~s~n", [TimeStamp, Pid, LogMsg, Data]), [append]).

%% by default, data logging is now enabled --> none is default value
build_logger_io(IO, none) -> 
	fun 
		(TimeStamp, Pid, LogMsg, Data) when is_binary(Data), byte_size(Data) > 0 -> 
			append_to_io(IO, TimeStamp, Pid, LogMsg, io_lib:format(" [data(len = ~p) = ~p]", [byte_size(Data), Data])); 
		(TimeStamp, Pid, LogMsg, _Data) -> 
			append_to_io(IO, TimeStamp, Pid, LogMsg, [])
	end;
build_logger_io(IO, _DoData) -> 
	fun 
		(TimeStamp, Pid, LogMsg, Data) when is_binary(Data), byte_size(Data) > 0 -> 
			append_to_io(IO, TimeStamp, Pid, LogMsg, io_lib:format(" [data_len = ~p]", [byte_size(Data)]));
		(TimeStamp, Pid, LogMsg, _Data) -> 
			append_to_io(IO, TimeStamp, Pid, LogMsg, [])
	end.

build_logger(Opts) -> 
	StdOutLogger = build_logger_console(maps:get(logger_stdout, Opts, none), maps:get(noiolog, Opts, none)),
	StdErrLogger = build_logger_console(maps:get(logger_stderr, Opts, none), maps:get(noiolog, Opts, none)),
	FileLogger = build_logger_file(maps:get(logger_file, Opts, none), maps:get(noiolog, Opts, none)),
	OutputToLog =
		fun (Pid, Fmt, Lst, Data) ->
			TimeStamp = get_timestamp(),
			LogMsg = io_lib:format(Fmt, Lst),
			StdOutLogger(TimeStamp, Pid, LogMsg, Data),
			StdErrLogger(TimeStamp, Pid, LogMsg, Data),
			FileLogger(TimeStamp, Pid, LogMsg, Data),
			ok
		end,
	case maps:get(verbose, Opts, 0) of
		V when V > 0 ->
			fun (Pid, _Type, Fmt, Lst, Data) -> OutputToLog(Pid, Fmt, Lst, Data) end;
		_Else ->
			fun 
				(_Pid, debug, _Fmt, _Lst, _Data) -> ok; 
				(Pid, _Type, Fmt, Lst, Data) -> OutputToLog(Pid, Fmt, Lst, Data) 
			end
	end.

build_logger_file(none, _) -> 
	fun (_, _, _, _) -> ok end;
build_logger_file([], DoData) -> 
	build_logger_io("./erlamsa.log", DoData);
build_logger_file(FileName, DoData) -> 
	build_logger_io(FileName, DoData).

build_logger_console(none, _) -> 
	fun (_, _, _, _) -> ok end;
build_logger_console(stdout, DoData) -> 
	build_logger_io(standard_io, DoData);
build_logger_console(stderr, DoData) -> 
	build_logger_io(standard_error, DoData).

start(Log) ->
    Pid = spawn(erlamsa_logger, logger, [Log]),
    global:register_name(logger, Pid),
	{ok, Pid}.

logger(Log) ->    
    receive
        {log, Pid, Type, Fmt, Lst, Data} -> 
            Log(Pid, Type, Fmt, Lst, Data); 
		{get_pid, Pid} -> 
            Pid ! {pid, self()}
    end,
	logger(Log).