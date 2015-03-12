%%%-------------------------------------------------------------------
%%% @author dark_k3y
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(patterns).
-author("dark_k3y").

-include("erlamsa.hrl").

-define(MAX_BURST_MUTATIONS, 16).

-compile([export_all]).

%% API
-export([]).

%% Ll -- list of smth
%% TODO: WARNING: Ll could be a function in Radamsa terms
%% TODO: WARNING: check this code!
mutate_once(Ll, Mutator, Meta, Cont) ->
    Ip = erlamsa_rnd:rand(?INITIAL_IP),
    {This, LlN} =  erlamsa_utils:uncons(Ll, false),
    if
        This /= false ->
            mutate_once_loop(Mutator, Meta, Cont, Ip, This, LlN);
        true ->
            Cont([], Mutator, Meta)
    end.

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
pat_once_dec(Ll, Mutator, Meta) ->
    mutate_once(Ll, Mutator, [{pattern, once_dec}|Meta], fun (L, M, Mt) -> L ++ {M, Mt} end).

%% 1 or more mutations
%% TODO: UGLY, need to refactor
pattern_many_dec_cont (Ll, Mutator, Meta) ->
    Muta = erlamsa_rnd:rand_occurs(?REMUTATE_PROBABILITY),
    % erlamsa_utils:debug("Muta occurs: ", Muta),
    case Muta of
        true -> pat_many_dec(Ll, Mutator, Meta);
        _ -> Ll ++ {Mutator, Meta}
    end.

pat_many_dec(Ll, Mutator, Meta) ->
    mutate_once(Ll, Mutator,  [{pattern, many_dec}|Meta], fun pattern_many_dec_cont/3).


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

pat_burst(Ll, Mutator, Meta) ->
    mutate_once(Ll, Mutator, [{pattern, burst}|Meta], fun pat_burst_cont/3).

%% /Patterns


patterns() -> [{1, fun pat_once_dec/3, "od", "Mutate patterns"},
               {1, fun pat_many_dec/3, "nd", "Mutate possibly many times"},
               {1, fun pat_burst/3, "bu", "Make several mutations closeby once"}
                ].

%% TODO: rewrite?
string_patterns(PatDefaultList) -> mux_patterns(lists:map(fun ({Pri, F, _, _}) -> {Pri, F} end, PatDefaultList)).

%% [{Pri, Pat}, ...] -> fun(rs, ll, muta, meta) .. pattern_output .. end
mux_patterns(Patterns) ->
    {SortedPatterns, N} = erlamsa_utils:sort_by_priority(Patterns),
    fun(Ll, Muta, Meta) ->
        RIdxPs = erlamsa_rnd:rand(N),
        PatF = erlamsa_utils:choose_pri(SortedPatterns, RIdxPs),
        PatF(Ll, Muta, Meta)
    end.