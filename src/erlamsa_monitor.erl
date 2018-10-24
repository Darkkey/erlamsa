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
%%% Generic monitors module.
%%% @end
%%%-------------------------------------------------------------------
-module(erlamsa_monitor).
-export([get_supervisor_opts/1, monitors/0]).

get_supervisor_opts(Opts) ->
    get_monitors(maps:get(monitor, Opts, nil)).

monitors() ->
    [{r2, erlamsa_mon_r2},
     {cdb, erlamsa_mon_cdb},
     {lc, erlamsa_mon_logcat},
     {cm, erlamsa_mon_connect}].

get_monitors(nil) ->
    [];
get_monitors({InMonName, Params}) ->
    lists:foldl(
        fun ({MonName, ModName}, Acc) when MonName == InMonName->
                [#{id => ModName,
                start => {ModName, start, [Params]},
                restart => temporary,
                shutdown => brutal_kill,
                type => worker,
                modules => [ModName]} | Acc];
            (_, Acc) -> Acc
        end,
        [],
        monitors()
    ).




