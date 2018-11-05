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
%%% Simple Generic Linear Mutations
%%% @end
%%%-------------------------------------------------------------------
-module(erlamsa_generic).
-author("dark_k3y").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

%% API
-export([list_del/2, list_del_seq/2, list_dup/2,
        list_repeat/2, list_clone/2, list_swap/2,
        list_perm/2,
        st_list_mod/3, st_list_ins/2, st_list_replace/2]).

-define(STORED_ELEMS, 10).

%%
%% Simple Generic Linear Mutations
%%

%% operatioon(List, Len) -> List
%% Len here for more clear pattern matching and optimizing
%% (see http://www.erlang.org/doc/efficiency_guide/commoncaveats.html, 3.3)

-spec list_del(list(), non_neg_integer()) -> list().
%% delete a random element
list_del([], 0) -> [];
list_del(L, Len) ->
    P = erlamsa_rnd:erand(Len),
    erlamsa_utils:applynth(P, L, fun (_, R) -> R end).

-spec list_del_seq(list(), non_neg_integer()) -> list().
%% delete a sequence of things
list_del_seq([], 0) -> [];
list_del_seq(L, Len) ->
    Start = erlamsa_rnd:erand(Len),
    N = erlamsa_rnd:erand(Len - Start + 1), % in erlang lists are starting with 1
    %% TODO: check if sublist is effective
    erlamsa_utils:applynth(Start, L, fun (_, R) -> lists:sublist(R, N, Len) end).

-spec list_dup(list(), non_neg_integer()) -> list().
%% duplicate a random element
list_dup([], 0) -> [];
list_dup(L, Len) ->
    P = erlamsa_rnd:erand(Len),
    erlamsa_utils:applynth(P, L, fun (E, R) -> [E, E|R] end).

-spec list_repeat(list(), non_neg_integer()) -> list().
%% repeat an element
list_repeat([], 0) -> [];
list_repeat(L, Len) ->
    P = erlamsa_rnd:erand(Len),
    N = max(2, erlamsa_rnd:rand_log(10)),
    %% FIXME: don't like ++ here ;(
    erlamsa_utils:applynth(P, L, fun (E, R) -> [E || _X <- lists:seq(1, N)] ++ R end).

-spec list_clone(list(), non_neg_integer()) -> list().
%% clone a value to another position
list_clone([], 0) -> [];
list_clone(L, Len) ->
    From = erlamsa_rnd:erand(Len),
    To = erlamsa_rnd:erand(Len),
    Elem = lists:nth(From, L),
    erlamsa_utils:applynth(To, L, fun(_E, R) -> [Elem | R] end).

-spec list_swap(list(), non_neg_integer()) -> list().
%% swap two adjecent values
list_swap([], 0) -> [];
list_swap(L, Len) when Len < 2 -> L;
list_swap(L, Len) ->
    P = erlamsa_rnd:erand(Len - 1),
    erlamsa_utils:applynth(P, L, fun (E, [H|T]) -> [H, E|T] end).

-spec list_perm(list(), non_neg_integer()) -> list().
%% permute values
%% NOTE: the behaviour of this function differs from radamsa (list-perm ...)
%% FIXME: should be optimized
list_perm([], 0) -> [];
list_perm(L, Len) when Len < 3 -> L;
list_perm(L, Len) ->
    From = erlamsa_rnd:erand(Len - 1), % in erlang lists are starting with 1
    A = erlamsa_rnd:rand_range(2, Len - From),
    B = erlamsa_rnd:rand_log(10),
    N = max(2, min(A, B)),
    erlamsa_utils:applynth(From, L,
        fun (E, R) -> % WARN: may be ineffective
            {ToPerm, Rest} = lists:split(N - 1, R),
            erlamsa_rnd:random_permutation([E|ToPerm]) ++ Rest
        end).

%% mutations which keep some old elements for mutations in (n-elems elem-1 elem2 ...)
-spec update_prob() -> non_neg_integer().
update_prob() -> ?STORED_ELEMS bsl 1.

-spec step_state(list(), list(), non_neg_integer()) -> list().
step_state([H|T], L, Len) when H < ?STORED_ELEMS ->
%% add elements until there are enough stored elements
    P = erlamsa_rnd:erand(Len),
    step_state(
        [H + 1, lists:nth(P, L) | T]
        , L, Len);
step_state(St, L, Len) ->
    Up = erlamsa_rnd:erand(update_prob()),
    case Up < ?STORED_ELEMS of
        true ->
            Ep = erlamsa_rnd:erand(Len),
            New = lists:nth(Ep, L),
            erlamsa_utils:applynth(Up + 1, St, fun ([_|T], R) -> [[New | T] | R] end); % +1 for len
        false ->
            St
    end.

-spec pick_state(nonempty_list()) -> any().
pick_state([H|T]) ->
    P = erlamsa_rnd:erand(H),
    lists:nth(P, T).

-spec st_list_mod(list(), list(), fun()) -> {list(), list()}.
st_list_mod(St, L, Fun) ->
    N = length(L),
    Stp = step_state(St, L, N),
    X = pick_state(Stp),
    P = erlamsa_rnd:erand(N),
    Lp = erlamsa_utils:applynth(P, L, Fun(X)),
    {Stp, Lp}.

%% ins
-spec st_list_ins(list(), list()) -> {list(), list()}.
st_list_ins(St, L) ->
    st_list_mod(St, L, fun(X) -> fun (T, R) -> [X, T | R] end end).

%% replace
-spec st_list_replace(list(), list()) -> {list(), list()}.
st_list_replace(St, L) ->
    st_list_mod(St, L, fun(X) -> fun (_, R) -> [X | R] end end).










