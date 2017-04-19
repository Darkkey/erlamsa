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
-export([get_timestamp/0, build_logger/1]).

get_timestamp() ->
	{_, _, Ms} = os:timestamp(),
	{Y, M, D} = date(),
    {H, Min, S} = time(),
    io_lib:format('~4..0b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b.~3..0b ~p', [Y, M, D, H, Min, S, round(Ms/1000), self()]).

append_to_logfile(FileName, TimeStamp, LogMsg) -> 
	file:write_file(FileName, io_lib:format("~s: ~s~n", [TimeStamp, LogMsg]), [append]).

build_logger(Opts) -> 
	StdOutLogger = build_logger_stdout(maps:get(logger_stdout, Opts, none)),
	StdErrLogger = build_logger_stderr(maps:get(logger_stderr, Opts, none)),
	FileLogger = build_logger_file(maps:get(logger_file, Opts, none)),
	fun (Fmt, Lst) ->
		TimeStamp = get_timestamp(),
		LogMsg = io_lib:format(Fmt, Lst),
		StdOutLogger(TimeStamp, LogMsg),
		StdErrLogger(TimeStamp, LogMsg),
		FileLogger(TimeStamp, LogMsg),
		ok
	end.

build_logger_file(none) -> 
	fun (_, _) -> ok end;
build_logger_file([]) -> 
	fun (TimeStamp, LogMsg) -> append_to_logfile("./erlamsa.log", TimeStamp, LogMsg) end;
build_logger_file(FileName) -> 
	fun (TimeStamp, LogMsg) -> append_to_logfile(FileName, TimeStamp, LogMsg) end.

build_logger_stdout(none) -> 
	fun (_, _) -> ok end;
build_logger_stdout(stdout) -> 
	fun (TimeStamp, LogMsg) -> io:format("~s: ~s~n", [TimeStamp, LogMsg]) end.

build_logger_stderr(none) -> 
	fun (_, _) -> ok end;
build_logger_stderr(stderr) -> 
	fun (TimeStamp, LogMsg) -> io:format(standard_error, "~s: ~s~n", [TimeStamp, LogMsg]) end.


