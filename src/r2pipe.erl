% Copyright (c) 2015-2019 Alexander Bolshev aka dark_k3y
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
%%% Updated R2Pipe binding for radare2
%%% @end
%%%-------------------------------------------------------------------
-module(r2pipe).
-export([start/2, stop/1, call/2, init/3]).

start(ExtPrg, ExtraParams) ->
    Pid = spawn(?MODULE, init, [self(), ExtPrg, ExtraParams]),
    receive
        {Pid, Result} ->
            {Pid, Result};
        {Pid, exit, Result} ->
            {exit, Result}
    end.

stop(Pid) ->
    Pid ! stop.

call(Pid, Msg) ->
    Pid ! {call, self(), Msg},
    receive
        {Pid, Result} ->
            Result
    end.

receive_port(Port) ->
    receive
		{Port, {data, Data}} ->
            Data;
        {'EXIT', Port, _Reason} ->
            port_exit 
	end.

%% Very ugly, thx to r2 weirdiness, this may be slow and unefficient.
read_r2_data(Port) ->
    read_r2_data(Port, none, []).
read_r2_data(Port, 0, [[0]] ) ->
    Data = receive_port(Port),
    %io:format("1) ~p~n", [Data]),
    read_r2_data(Port, lists:nth(length(Data), Data), [Data]);
read_r2_data(_Port, 0, Acc) ->
    %io:format("2) ~p~n", [nil]),
    lists:droplast(lists:flatten(lists:reverse(Acc)));
read_r2_data(Port, _Any, Acc) ->
    Data = receive_port(Port),
    %io:format("3) ~p ~p~n", [Data, lists:nth(length(Data), Data)]),
    read_r2_data(Port, lists:nth(length(Data), Data), [Data | Acc]).


% waiting while r2 returns \0 byte -- signal that it has been started
r2_ready(_Port, [0], Acc) ->
    {ok, lists:reverse(Acc)};
r2_ready(Port, _PrevData, Acc) ->
    case receive_port(Port) of
        port_exit ->
            {exit, lists:reverse(Acc)};
        Data -> 
            %io:format("Start: ~p  ~p~n", [Data, lists:nth(length(Data), Data)]),
            r2_ready(Port, Data, [Data | Acc])
    end.

loop(Port) ->
    receive
	{call, Caller, Msg} ->
	    Port ! {self(), {command, Msg}},
        RcvdData = read_r2_data(Port),
        %io:format("******* RcvData: ~p~n", [RcvdData]),
   	    Caller ! {self(), RcvdData},
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

init(StarterPid, ExtPrg, ExtraParams) ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [use_stdio, stderr_to_stdout, hide, stream | ExtraParams]),
    case r2_ready(Port, [], []) of
        {ok, Data} ->
            StarterPid ! {self(), Data},
            loop(Port);
        {exit, ExitData} ->
            StarterPid ! {self(), exit, ExitData}
    end.


