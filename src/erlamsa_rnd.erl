%%%-------------------------------------------------------------------
%%% @author dark_k3y
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlamsa_rnd).
-author("dark_k3y").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

%% API
-export([rand/1, erand/1, rand_float/0, rand_bit/0, rand_occurs/1, 
		rand_nbit/1, rand_log/1, rand_elem/1, random_block/1, 
		random_numbers/2, random_permutation/1, rand_range/2,
		reservoir_sample/2, rand_delta/0, rand_delta_up/0]).

%% Constants
-define(P_WEAKLY_USUALLY_NOM, 11).
-define(P_WEAKLY_USUALLY_DENOM, 20).

%% common GCD (uneffective!)
gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).

%% generate random in range [0, N)
rand(0) -> 0;
rand(N) -> random:uniform(N) - 1.

%% generate random in range [1, N]
erand(0) -> 0;
erand(N) -> random:uniform(N).

% generates random in range(L,R)
rand_range(L, R) when R>L ->
    rand(R-L) + L;
rand_range(L, L) ->
    L;
rand_range(_, _) ->
    0.

%% generate random float
rand_float() -> rand:uniform().

% random 1 bit
rand_bit() -> round(random:uniform()).

%% check if random occurs with some probability
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
rand_occurs_fixed(Nom, Denom) ->
    N = rand(Denom),
    if
        Nom == 1 ->
            N /= 0;
        true ->
            N < Nom
    end.

% random exactly n-bit number
rand_nbit(0) -> 0;
rand_nbit(N) ->
    Hi = 1 bsl (N-1),
    Hi bor rand(Hi).

%% a number with log_2(n) instead of n evenly distributed in range
rand_log(0) -> 0;
rand_log(N) ->
    rand_nbit(rand(N)).

%% random element of a list
%% TODO: need optimize
rand_elem([]) -> [];
rand_elem(L) ->
    N = length(L),
    lists:nth(random:uniform(N), L).


%% Generate random block of bytes.
%% TODO: check byte-stream magic here, Radamsa realization is MUCH differ
random_block(N) -> random_block(N, []).

random_block(0, Out) -> list_to_binary(Out);
random_block(N, Out) -> random_block(N - 1, [rand(256) | Out]).

%% generates a list of Cnt numbers with upper Bount
random_numbers(Bound, Cnt) -> random_numbers(Bound, Cnt, []).

random_numbers(_, 0, Acc) -> Acc;
random_numbers(Bound, Cnt, Acc) -> random_numbers(Bound, Cnt - 1, [rand(Bound) | Acc]).

%% WARNING: the Owl-Lisp original realization is much complex!
%% TODO: FIXME if it's incorrect
%% randomly permutate the given list
%% DIRTY HACK(!!!): works bad on 2 bytes, so force permute, FIXME
random_permutation(Lst = [A, B]) ->
    case rand(2) =:= 1 of
        true -> [B, A];
        false -> Lst
    end;
random_permutation(L) ->
    [Y || {_, Y} <- lists:sort([{random:uniform(),X} || X <- L])].


%% see http://en.wikipedia.org/wiki/Reservoir_sampling
reservoir_sample(Ll, K) when K >= length(Ll) -> Ll;
reservoir_sample(Ll, K) ->     
    reservoir_sample(list_to_tuple(Ll), length(Ll), K, K+1, list_to_tuple(lists:sublist(Ll, K))).

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
rand_delta() ->
    Bit = rand_bit(),
    case Bit of
        0 ->
            +1;
        _Else ->
            -1
    end.

%% random delta with a slight positive bias
rand_delta_up() ->
    Occ = owllisp:rand_occurs({?P_WEAKLY_USUALLY_NOM, ?P_WEAKLY_USUALLY_DENOM}),
    case Occ of
        true ->
            +1;
        false ->
            -1
    end.