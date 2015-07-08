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
%%% Various helper random functions
%%% @end
%%%-------------------------------------------------------------------
-module(erlamsa_rnd).
-author("dark_k3y").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

%% API
-export([rand/1, erand/1, rand_float/0, rand_bit/0, rand_occurs/1, rand_occurs_fixed/2,
		rand_nbit/1, rand_log/1, rand_elem/1, random_block/1, 
		random_numbers/2, random_permutation/1, rand_range/2,
		reservoir_sample/2, rand_delta/0, rand_delta_up/0]).

%% Constants
-define(P_WEAKLY_USUALLY_NOM, 11).
-define(P_WEAKLY_USUALLY_DENOM, 20).

%% common GCD (uneffective!)
-spec gcd(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).

%% generate random in range [0, N)
-spec rand(non_neg_integer()) -> non_neg_integer().
rand(0) -> 0;
rand(N) -> random:uniform(N) - 1.

%% generate random in range [1, N]
-spec erand(non_neg_integer()) -> non_neg_integer().
erand(0) -> 0;
erand(N) -> random:uniform(N).

% generates random in range(L,R)
-spec rand_range(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
rand_range(L, R) when R>L ->
    rand(R-L) + L;
rand_range(L, L) ->
    L;
rand_range(_, _) ->
    0.

%% generate random float
-spec rand_float() -> number().
rand_float() -> random:uniform().

% random 1 bit
-spec rand_bit() -> 1 | 0.
rand_bit() -> round(random:uniform()).

%% check if random occurs with some probability
-spec rand_occurs(any()) -> true | false.
rand_occurs({Nom, Denom}) when is_integer(Nom), is_integer(Denom) ->
    rand_occurs_fixed(Nom, Denom);
rand_occurs(Prob) when is_float(Prob) ->
    PreNom = trunc(Prob * 100),
    G = gcd(PreNom, 100),
    Nom = PreNom div G,
    Denom = 100 div G,
    rand_occurs_fixed(Nom, Denom);
rand_occurs(Prob) when is_integer(Prob) -> false;
rand_occurs(Prob) when Prob == 0 -> false;
rand_occurs(_) -> false.

%% check if random occurs with Nom/Denom probability
-spec rand_occurs_fixed(non_neg_integer(), pos_integer()) -> true | false.
rand_occurs_fixed(Nom, Denom) when is_integer(Nom), is_integer(Denom) ->
    N = rand(Denom),
    if
        Nom == 1 ->
            N /= 0;
        true ->
            N < Nom
    end.

% random exactly n-bit number
-spec rand_nbit(non_neg_integer()) -> non_neg_integer().
rand_nbit(0) -> 0;
rand_nbit(N) ->
    Hi = 1 bsl (N-1),
    Hi bor rand(Hi).

%% a number with log_2(n) instead of n evenly distributed in range
-spec rand_log(non_neg_integer()) -> non_neg_integer().
rand_log(0) -> 0;
rand_log(N) ->
    rand_nbit(rand(N)).

%% random element of a list
%% TODO: need optimize
-spec rand_elem(list()) -> any().
rand_elem([]) -> [];
rand_elem(L) ->
    N = length(L),
    lists:nth(random:uniform(N), L).


%% Generate random block of bytes.
%% TODO: check byte-stream magic here, Radamsa realization is MUCH differ
-spec random_block(non_neg_integer()) -> binary().
random_block(N) -> random_block(N, []).

-spec random_block(non_neg_integer(), list(byte())) -> binary().
random_block(0, Out) -> list_to_binary(Out);
random_block(N, Out) -> random_block(N - 1, [rand(256) | Out]).

%% generates a list of Cnt numbers with upper Bount
-spec random_numbers(non_neg_integer(), non_neg_integer()) -> [non_neg_integer()].
random_numbers(Bound, Cnt) -> random_numbers(Bound, Cnt, []).

-spec random_numbers(non_neg_integer(), non_neg_integer(), [non_neg_integer()]) -> [non_neg_integer()].
random_numbers(_, 0, Acc) -> Acc;
random_numbers(Bound, Cnt, Acc) -> random_numbers(Bound, Cnt - 1, [rand(Bound) | Acc]).

%% WARNING: the Owl-Lisp original realization is much complex!
%% TODO: FIXME if it's incorrect
%% randomly permutate the given list
%% DIRTY HACK(!!!): works bad on 2 bytes, so force permute, FIXME
-spec random_permutation(list()) -> any().
random_permutation(Lst = [A, B]) ->
    case rand(2) =:= 1 of
        true -> [B, A];
        false -> Lst
    end;
random_permutation(L) ->
    [Y || {_, Y} <- lists:sort([{random:uniform(),X} || X <- L])].


%% see http://en.wikipedia.org/wiki/Reservoir_sampling
-spec reservoir_sample(list(), non_neg_integer()) -> list().
reservoir_sample(Ll, K) when K >= length(Ll) -> Ll;
reservoir_sample(Ll, K) ->     
    reservoir_sample(list_to_tuple(Ll), length(Ll), K, K+1, list_to_tuple(lists:sublist(Ll, K))).

-spec reservoir_sample(tuple(), non_neg_integer(), non_neg_integer(), non_neg_integer(), tuple()) -> list().
reservoir_sample(_S, N, _K, I, R) when I > N -> tuple_to_list(R);
reservoir_sample(S, N, K, I, R) ->
    J = erand(I),
    case J =< K of
        true -> reservoir_sample(S, N, K, I+1, 
            setelement(J, R, element(I, S)));
        false -> reservoir_sample(S, N, K, I+1, R)
    end.

%%%
%%% Random delta generators for random priority steppers
%%%

%% random delta for brownian steppers
%% strange random delta <-- copied from radamsa,
%% TODO: check for uniformity for Bit && rewrite
-spec rand_delta() -> +1 | -1.
rand_delta() ->
    Bit = rand_bit(),
    case Bit of
        0 ->
            +1;
        _Else ->
            -1
    end.

%% random delta with a slightly positive bias
-spec rand_delta_up() -> +1 | -1.
rand_delta_up() ->
    Occ = rand_occurs_fixed(?P_WEAKLY_USUALLY_NOM, ?P_WEAKLY_USUALLY_DENOM),
    case Occ of
        true ->
            +1;
        false ->
            -1
    end.