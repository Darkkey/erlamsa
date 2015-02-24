%%%-------------------------------------------------------------------
%%% @author dark_k3y
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(owllisp).
-author("dark_k3y").

%% API
-compile([export_all]).
-export([]).

%% l -> hd l' | error
uncons([L|T], _) -> {L, T};
uncons([], D) -> {D, []};
uncons(B, _) when is_binary(B) -> {B, []};
uncons(L, D) when is_function(L) -> uncons(L(), D).

%% extract function value
extract_function([X]) -> extract_function(X); %% <-- may be ambigious
extract_function(X) when is_function(X) -> extract_function(X());
extract_function(X) -> X.

%% forcing ll def
forcell([]) -> [];
forcell([H|T]) -> [H | forcell(T)];
forcell(X) when is_function(X) -> X().

%% get an element by key from gb_tree, return DefaulrValue if none exists
get(Key, DefaultValue, Tree) ->
    case gb_trees:lookup(Key, Tree) of
        {value, Value} -> Value;
        none -> DefaultValue
    end.

%% last member find
last(L) when is_list(L) -> last(lists:last(L));
%% last(F) when is_function(F) -> shared:debug(F()), last(F);
last(X) -> X.

%% apply function to list element at position N
%% Fun MUST return list!
%% WARN: TODO: Ugly, need rewrite
led(L, Pos, Fun) ->
    % io:write({L, Pos, Fun, lists:sublist(L, Pos - 1), Fun(lists:nth(Pos,L)), lists:nthtail(Pos,L)}),
    lists:sublist(L, Pos - 1) ++ Fun(lists:nth(Pos,L)) ++ lists:nthtail(Pos,L).

%% check if pair
is_pair([_ | T]) when T =/= [] -> true;
is_pair(_) -> false.

%% common GCD (uneffective!)
gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).

%% generate random in range [0, N)
rand(0) -> 0;
rand(N) -> random:uniform(N) - 1.

%% generate random in range [1, N]
erand(0) -> 0;
erand(N) -> random:uniform(N).

%% generate random float
rand_float() -> rand:uniform().

% random 1 bit
rand_bit() -> round(random:uniform()).

%% check if random occurs with some probability
rand_occurs(Rs, {Nom, Denom}) when is_integer(Nom), is_integer(Denom) ->
    rand_occurs_fixed(Rs, Nom, Denom);
rand_occurs(Rs, Prob) when is_float(Prob) ->
    PreNom = trunc(Prob * 100),
    G = gcd(PreNom, 100),
    Nom = PreNom div G,
    Denom = 100 div G,
    rand_occurs_fixed(Rs, Nom, Denom);
rand_occurs(Rs, Prob) when is_integer(Prob) -> {Rs, false};
rand_occurs(Rs, Prob) when Prob == 0 -> {Rs, false};
rand_occurs(Rs, _) -> {Rs, false}.

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

%% check if random occurs with Nom/Denom probability
rand_occurs_fixed(Rs, Nom, Denom) ->
    N = rand(Denom),
    if
        Nom == 1 ->
            {Rs, N /= 0};
        true ->
            {Rs, N < Nom}
    end.

%% generates a list of Cnt numbers with upper Bount
random_numbers(Bound, Cnt) -> random_numbers(Bound, Cnt, []).

random_numbers(_, 0, Acc) -> Acc;
random_numbers(Bound, Cnt, Acc) -> random_numbers(Bound, Cnt - 1, [rand(Bound) | Acc]).

%% WARNING: the Owl-Lisp original realization is much complex!
%% TODO: FIXME if it's incorrect
%% randomly permutate the given list
%% DIRTY HACK(!!!): works bad on 2 bytes, so force permute, FIXME
random_permutation([A, B]) ->
    [B, A];
random_permutation(L) ->
    [Y || {_, Y} <- lists:sort([{random:uniform(),X} || X <- L])].

% generates random in range(L,R)
rand_range(L, R) when R>L ->
    rand(R-L) + L;
rand_range(L, L) ->
    L;
rand_range(_, _) ->
    0.

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

