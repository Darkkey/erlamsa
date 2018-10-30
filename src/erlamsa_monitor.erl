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
-export([get_supervisor_opts/1, monitors/0, default/0, parse_after/1, do_after/1]).

get_supervisor_opts(Opts) ->
    get_monitors(maps:get(monitor, Opts, [])).

default() -> [{plus,cm,"timeout=5000"}].

monitors() ->
    [
     {cdb, erlamsa_mon_cdb, "CDB debugging monitor (windows targets only)"},
     {cm, erlamsa_mon_connect, "TCP connection monitor (run by default)"},
     {lc, erlamsa_mon_logcat, "Logcat/ADB crash monitor"},
     {r2, erlamsa_mon_r2, "Radare2 debugging monitor"}
    ].

get_monitors([]) ->
    [];
get_monitors(MonLst) ->
    MonitorsMap = maps:from_list(lists:map(fun ({K, M, _Desc}) -> {K, M} end, monitors())),
    FullMonLst = lists:map(fun ({A, N, P}) -> {A, N, P, maps:get(N, MonitorsMap, nil)} end,
                            MonLst),
    SelectedMons = get_monitors_loop(lists:reverse(FullMonLst), maps:new()),
    lists:foldl(
        fun ({MonParams, ModName}, Acc) ->
                [#{id => ModName,
                start => {ModName, start, [MonParams]},
                restart => temporary,
                shutdown => brutal_kill,
                type => worker,
                modules => [ModName]} | Acc];
            (nil, Acc) -> Acc
        end,
        [],
        SelectedMons
    ).

get_monitors_loop([{plus, N, P, M} | Mons], Dict) ->
    get_monitors_loop(Mons, maps:put(N, {P, M}, Dict));
get_monitors_loop([{minus, N, _P, _M} | Mons], Dict) ->
    get_monitors_loop(Mons, maps:put(N, nil, Dict));
get_monitors_loop([], Dict) ->
    lists:map(fun ({_K, V}) -> V end, maps:to_list(Dict)).

parse_after(Params) ->
    parse_after(Params, maps:new(), []).

parse_after(["after_params=" ++ AfterParam|T], Acc, ParamsAcc) ->
    parse_after(T, maps:put(do_after_params, AfterParam, Acc), ParamsAcc);
parse_after(["after=" ++ AfterType|T], Acc, ParamsAcc) ->
    parse_after(T, maps:put(do_after_type, list_to_atom(AfterType), Acc), ParamsAcc);
parse_after([H|T], Acc, ParamsAcc) ->
    parse_after(T, Acc, [H|ParamsAcc]);
parse_after([], Acc, ParamsAcc) ->
    {Acc, ParamsAcc}.

do_after(MonOpts) ->
    do_after(maps:get(do_after_type, MonOpts, nil), MonOpts).

do_after(exec, Opts) ->
    ExecPath = maps:get(do_after_params, Opts, ""),
    os:cmd(ExecPath); %%TODO: add result to logs
do_after(nil, _Opts) -> ok.


