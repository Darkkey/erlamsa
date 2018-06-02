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
%%% Alignment-based sequence fusing
%%% @end
%%%-------------------------------------------------------------------
-module(erlamsa_fuse).
-author("dark_k3y").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

%% API
-export([fuse/2]).

%% WARN: This module could be wrongly implemented
%% TODO: ReCheck, ReFactor and Comment
%% TODO: fix specs

-define(SEARCH_FUEL, 100000).
-define(SEARCH_STOP_IP, 8).

-spec jump(any(), any(), any()) -> any().
jump(F, F, To) ->
    To;
jump([H|T], F, To) ->
    [H|jump(T, F, To)].

-spec suffixes(list()) ->list().
suffixes([]) -> [];
suffixes(Lst = [_H|T]) ->
    [Lst | suffixes(T)].

-spec fix_empty_list([[]] | nonempty_list()) -> list().
fix_empty_list([[]]) -> [];
fix_empty_list(L) -> L.

-spec char_suffixes(list()) -> gb_trees:tree().
char_suffixes(S) ->
    lists:foldl(
        fun ([H|T], Subs) ->
                %% TODO: ugly, need refactor
                El = fix_empty_list([T|erlamsa_utils:get(H, [], Subs)]),
                gb_trees:enter(H, El, Subs);
            ([], Subs) ->
                Subs
        end, gb_trees:empty(), S).

-spec any_position_pair(list()) -> {any(), any()}.
any_position_pair(Nodes) ->
    [Froms|Tos] = erlamsa_rnd:rand_elem(Nodes),
    From = erlamsa_rnd:rand_elem(Froms),
    To = erlamsa_rnd:rand_elem(Tos),
    {From, To}.

%%          .------------------------> source list suffixes after a shared prefix
%%          |               .--------> target -||-
%%          |               v
%%          v
%% node = ((suffix ...) . (suffix ...))
-spec split(nonempty_list(), list()) -> list(list()).
split([H|T], Acc) ->
    Sas = char_suffixes(H),
    Sbs = char_suffixes(T),
    lists:foldl(
        fun
            ({_Char, []}, Tl) ->
                %% WARN: Ugly, but | is not like cons in owllisp
                [[[[]], []]| Tl];
            ({Char, Sufs}, Tl) ->
                Bs = erlamsa_utils:get(Char, notfound, Sbs),
                case Bs of
                    notfound -> Tl;
                    _Else -> [[Sufs|Bs]|Tl]
                end
        end,
        Acc,  gb_trees:to_list(Sas)).

-spec find_jump_points(list(), list()) -> {any(), any()}.
find_jump_points(A, B) ->
    Al = suffixes(A),
    Bl = suffixes(B),
    Nodes = [[Al|Bl]],
    find_jump_points_loop(Nodes, ?SEARCH_FUEL).


%% TODO: WARN: check whether I'm correct
%% in radamsa:
%% (let loop ((rs rs) (nodes nodes) (fuel search-fuel))
%% (if (< search-fuel 0) ;; <-- but this is a CONSTANT. Bug?
-spec find_jump_points_loop(list(list()), integer()) -> {any(), any()}.
find_jump_points_loop(Nodes, Fuel) when Fuel < 0 ->
    any_position_pair(Nodes);
%% TODO: very UGLY code, need rewrite
find_jump_points_loop(Nodes, Fuel) ->
    X = erlamsa_rnd:rand(?SEARCH_STOP_IP),
    case X of
        0 -> any_position_pair(Nodes);
        _Else ->
            NoDesp = lists:foldl(fun split/2, [], Nodes),
            case NoDesp of
                [] -> any_position_pair(Nodes);
                _Else2 -> find_jump_points_loop(NoDesp, Fuel - length(NoDesp))
            end
        end.

-spec fuse(list(), list()) -> list().
fuse([], Bl) -> Bl;
fuse(Al, []) -> Al;
fuse(Al, Bl) ->
    {A, B} = find_jump_points(Al, Bl),
    jump(Al, A, B).