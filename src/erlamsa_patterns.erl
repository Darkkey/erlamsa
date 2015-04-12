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
%%% Patterns for calling mutators.
%%% @end
%%%-------------------------------------------------------------------
-module(erlamsa_patterns).
-author("dark_k3y").

-include("erlamsa.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

%% API
-export([make_pattern/1, default/0, patterns/0, tostring/1]).

-define(MAX_BURST_MUTATIONS, 16).

-type mutator_cont_fun() :: fun((any(), mutator(), meta_list()) -> list()).

%% Ll -- list of smth
%% TODO: WARNING: Ll could be a function in Radamsa terms
%% TODO: WARNING: check this code!
%% TODO: temporary contract, fix it.
-spec mutate_once(any(), mutator(), meta_list(), mutator_cont_fun()) -> list().
mutate_once(Ll, Mutator, Meta, Cont) ->
    Ip = erlamsa_rnd:rand(?INITIAL_IP),
    {This, LlN} = erlamsa_utils:uncons(Ll, false),
    if
        This /= false ->
            mutate_once_loop(Mutator, Meta, Cont, Ip, This, LlN);
        true ->
            Cont([], Mutator, Meta)
    end.

%% TODO: temporary contract, fix it.
-spec mutate_once_loop(mutator(), meta_list(), mutator_cont_fun(), non_neg_integer(), any(), any()) -> list().
mutate_once_loop(Mutator, Meta, Cont, Ip, This, Ll) when is_function(Ll) ->
    mutate_once_loop(Mutator, Meta, Cont, Ip, This, Ll());
mutate_once_loop(Mutator, Meta, Cont, Ip, This, Ll) ->
    N = erlamsa_rnd:rand(Ip),
    if
        N =:= 0 orelse Ll =:= [] ->  %% or TODO: Ll == nil???
            {M, L, Mt} = Mutator([This | Ll], Meta),
            Cont(L, M, Mt);
        true ->
            [This, fun () -> mutate_once_loop(Mutator, Meta, Cont, Ip, hd(Ll), tl(Ll)) end]
    end.


%% Patterns:
%% TODO: check what is really used, should we use ++ or can just return tuple {list, {...}} or smth

%% pat :: ll muta meta -> ll' ++ (list (tuple mutator meta))
%% WARNING: in radamsa it should return list ++ tuple{mutator, meta}, here we're returning [list ++ {mutator, meta}]
%% TODO: UGLY, need to refactor
%% TODO: temporary contract, fix it.
-spec pat_once_dec(any(), mutator(), meta_list()) -> list().
pat_once_dec(Ll, Mutator, Meta) ->
    mutate_once(Ll, Mutator, [{pattern, once_dec} | Meta], fun (L, M, Mt) -> L ++ [{M, Mt}] end).

%% 1 or more mutations
%% TODO: UGLY, need to refactor
%% TODO: temporary contract, fix it.
-spec pat_many_dec_cont(any(), mutator(), meta_list()) -> list().
pat_many_dec_cont (Ll, Mutator, Meta) ->
    Muta = erlamsa_rnd:rand_occurs(?REMUTATE_PROBABILITY),
    % erlamsa_utils:debug("Muta occurs: ", Muta),
    case Muta of
        true -> pat_many_dec(Ll, Mutator, Meta);
        _ -> Ll ++ [{Mutator, Meta}]
    end.

%% TODO: temporary contract, fix it.
-spec pat_many_dec(any(), mutator(), meta_list()) -> list().
pat_many_dec(Ll, Mutator, Meta) ->
    mutate_once(Ll, Mutator,  [{pattern, many_dec} | Meta], fun pat_many_dec_cont/3).


%% TODO: temporary contract, fix it.
-spec pat_burst_cont(any(), mutator(), meta_list()) -> list().
%% TODO: UGLY, need to refactor
pat_burst_cont (Ll, Mutator, Meta) -> 
    pat_burst_cont (Ll, Mutator, Meta, 1). 

pat_burst_cont (Ll, Mutator, Meta, N) ->
    P = erlamsa_rnd:rand_occurs(?REMUTATE_PROBABILITY),
    MutateMore = P or (N < 2),
    case MutateMore of
        true ->
            {M, L, Mt} = Mutator(Ll, Meta),
            pat_burst_cont(L, M, Mt, N+1); %% TODO: check!!!!
        false ->
            Ll ++ [{Mutator, Meta}]
    end.

%% TODO: temporary contract, fix it.
-spec pat_burst(any(), mutator(), meta_list()) -> list().
pat_burst(Ll, Mutator, Meta) ->
    mutate_once(Ll, Mutator, [{pattern, burst}|Meta], fun pat_burst_cont/3).

%% /Patterns

-spec patterns() -> [pattern()].
patterns() -> [{1, fun pat_once_dec/3, od, "Mutate once pattern"},
               {1, fun pat_many_dec/3, nd, "Mutate possibly many times"},
               {1, fun pat_burst/3, bu, "Make several mutations closeby once"}
                ].

-spec default() -> [{atom(), non_neg_integer()}].
default() -> lists:map(fun ({Pri, _, Name, _}) -> {Name, Pri} end, patterns()).

-spec tostring(list()) -> string().
tostring(Lst) -> 
    lists:foldl(fun ({_Pri, _Fun, Name, _Desc}, Acc) -> 
        atom_to_list(Name) ++ "," ++ Acc
    end, [], Lst).

%% TODO: rewrite?
%-spec string_patterns([pattern()]) -> fun((any(), mutator(), meta_list()) -> list()).
%string_patterns(PatDefaultList) -> mux_patterns(lists:map(fun ({Pri, F, _, _}) -> {Pri, F} end, PatDefaultList)).
-spec make_pattern([{atom(), non_neg_integer()}]) -> fun().
make_pattern(Lst) ->
    SelectedPats = maps:from_list(Lst),
    Pats = lists:foldl(
        fun ({_Pri, F, Name, _Desc}, Acc) ->
            Val = maps:get(Name, SelectedPats, notfound),
            case Val of 
                notfound -> Acc;
                _Else -> [{Val, F} | Acc]
            end
        end,
        [],
        patterns()),
    mux_patterns(Pats).

%% [{Pri, Pat}, ...] -> fun(rs, ll, muta, meta) .. pattern_output .. end
-spec mux_patterns([pattern()]) -> fun((any(), mutator(), meta_list()) -> list()).
mux_patterns(Patterns) ->
    {SortedPatterns, N} = erlamsa_utils:sort_by_priority(Patterns),
    fun(Ll, Muta, Meta) ->
        RIdxPs = erlamsa_rnd:rand(N),
        PatF = erlamsa_utils:choose_pri(SortedPatterns, RIdxPs),
        PatF(Ll, Muta, Meta)
    end.