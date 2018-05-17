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
%%% Profiler: logging memory consumption.
%%% @end
%%%-------------------------------------------------------------------

-module(erlamsa_profiler).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

-include("erlamsa.hrl").

% API
-export([start/2, profiler/1]).

start(nil, _Args) ->
    ok;
start(debug, Args) ->
    spawn(observer, start, []),
    spawn(erlamsa_profiler, profiler, [Args]).
    
profiler(Args) ->    
    ProcessCount = length(erlang:processes()),
    Memory = erlang:memory(total),
    LoggerMemory = process_info(hd(Args), [memory]),
    erlamsa_logger:log(info, "Profiler report: procs = ~p, memory = ~p, logger_memory = ~p", [ProcessCount, Memory, LoggerMemory]),
    timer:sleep(5000),
    profiler(Args).