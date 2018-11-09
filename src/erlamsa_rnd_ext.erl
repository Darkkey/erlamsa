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
%%% External random source
%%% @end
%%%-------------------------------------------------------------------

-module(erlamsa_rnd_ext).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

-include("erlamsa.hrl").

% API
-export([get_supervisor_opts/1, start/1, rnd_ext/1, get_pid/0, get_seed/0]).

get_supervisor_opts(Opts) ->
    #{id => ?MODULE,
    start => {?MODULE, start, [build_rnd_ext(Opts)]},
    restart => permanent,
    shutdown => brutal_kill,
    type => worker,
    modules => [?MODULE]}.

%% crash if fail
get_seed() ->
    global:send(rnd_ext, {get_seed, self()}),
    receive
        {seed, Seed} -> Seed
        
    end.

%% crash if fail
get_pid() ->
    global:send(rnd_ext, {get_pid, self()}),
    receive
       {pid, Pid} -> Pid
    end.

%%
%% Build loggers based on options
%%
build_rnd_ext(Opts) ->
    Source = maps:get(ext_rnd_source, Opts, nil),
    %% TODO: check device existence
    Source.

start(Source) ->
    Pid = spawn(erlamsa_rnd_ext, rnd_ext, [Source]),
    global:register_name(rnd_ext, Pid),
    {ok, Pid}.

rnd_ext(Source) ->
    {ok, IoDevice} = file:open(Source, [read, binary, raw]),
    loop(IoDevice).

loop(IoDevice) ->
    receive
        {get_seed, Pid} ->
            case file:read(IoDevice, 6) of
                {ok, <<S1:16/unsigned-integer, S2:16/unsigned-integer, S3:16/unsigned-integer>>} ->
                    Pid ! {seed, {S1, S2, S3}};
                _Else -> %%TODO: re-read?
                    Pid ! {seed, notenoughdata}
            end;
        {get_pid, Pid} ->
            Pid ! {pid, self()}
    end,
    loop(IoDevice).