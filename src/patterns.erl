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
mutate_once(Rs, Ll, Mutator, Meta, Cont) ->
    %shared:debug("mutate-once", Ll),
    Ip = owllisp:rand(?INITIAL_IP),
    {This, LlN} =  owllisp:uncons(Ll, false),
    %shared:debug("mutate-once - 1", Ll),
    if
        This /= false ->
            mutate_once_loop(Rs, Mutator, Meta, Cont, Ip, This, LlN);
        true ->
            Cont([], Rs, Mutator, Meta)
    end.

mutate_once_loop(Rs, Mutator, Meta, Cont, Ip, This, Ll) when is_function(Ll) ->
    mutate_once_loop(Rs, Mutator, Meta, Cont, Ip, This, Ll());
mutate_once_loop(Rs, Mutator, Meta, Cont, Ip, This, Ll) ->
    %shared:debug("mutate-once-loop - This", This),
    %shared:debug("mutate-once-loop - Ll", Ll),
    %shared:debug("mutate-once-loop - Ip", Ip),
    N = owllisp:rand(Ip),
    %shared:debug("mutate-once-loop - N", N),
    if
        N =:= 0 orelse Ll =:= [] ->  %% or TODO: Ll == nil???
            %shared:debug_str("mutate-once-loop - if1"),
            shared:debug("\n\nmutate-once-loop - calling Mutator", [This | Ll]),
            {M, R, L, Mt} = Mutator(Rs, [This | Ll], Meta),
            shared:debug("mutate-once-loop - /calling Mutator", {M, R, L, Mt}),
            %shared:debug_str("mutate-once-loop - before Cont"),
            Cont(L, R, M, Mt);
        true ->
            %shared:debug_str("mutate-once-loop - ifelse"),
            [This, fun () -> mutate_once_loop(Rs, Mutator, Meta, Cont, Ip, hd(Ll), tl(Ll)) end]
    end.


%% Patterns:
%% TODO: check what is really used, should we use ++ or can just return tuple {list, {...}} or smth

%% pat :: rs ll muta meta -> ll' ++ (list (tuple rs mutator meta))
%% WARNING: in radamsa it should return list ++ tuple{rs, mutator, meta}, here we're returning [list ++ {rs, mutator, meta}]
%% TODO: UGLY, need to refactor
pat_once_dec(Rs, Ll, Mutator, Meta) ->
    mutate_once(Rs, Ll, Mutator, [{pattern, once_dec}|Meta], fun (L, R, M, Mt) -> L ++ {R, M, Mt} end).

%% 1 or more mutations
%% TODO: UGLY, need to refactor
pattern_many_dec_cont (Ll, Rs, Mutator, Meta) ->
    {RsNew, Muta} = owllisp:rand_occurs(Rs, ?REMUTATE_PROBABILITY),
    % shared:debug("Muta occurs: ", Muta),
    case Muta of
        true -> pat_many_dec(RsNew, Ll, Mutator, Meta);
        _ -> Ll ++ {RsNew, Mutator, Meta}
    end.

pat_many_dec(Rs, Ll, Mutator, Meta) ->
    mutate_once(Rs, Ll, Mutator,  [{pattern, many_dec}|Meta], fun pattern_many_dec_cont/4).


%% TODO: UGLY, need to refactor
pat_burst_cont (Ll, Rs, Mutator, Meta) -> % shared:debug("!!!!!!!!!!", RsN), shared:debug("!!!!!!!!!!", LlN), failnow(),
    pat_burst_cont (Ll, Rs, Mutator, Meta, 1). %, shared:debug(T), T.

pat_burst_cont (Ll, Rs, Mutator, Meta, N) ->
    %shared:debug("in pat_burst_fun:", LlF),
    {RsFNew, P} = owllisp:rand_occurs(Rs, ?REMUTATE_PROBABILITY),
    MutateMore = P or (N < 2),
    %shared:debug(MutateMore),
    case MutateMore of
        true ->
            shared:debug_str("\n\npat_burst_cont: more: calling Mutator"),
            {M, R, L, Mt} = Mutator(RsFNew, Ll, Meta),
            shared:debug("pat_burst_cont: more: /calling Mutator", {M, R, L, Mt}),
            pat_burst_cont(L, Rs, M, Mt, N+1); %% TODO: check!!!!
        false ->
            %shared:debug_str("enough..."),
            Ll ++ [{RsFNew, Mutator, Meta}]
    end.

pat_burst(Rs, Ll, Mutator, Meta) ->
    mutate_once(Rs, Ll, Mutator, [{pattern, burst}|Meta], fun pat_burst_cont/4).

%% /Patterns


patterns() -> [{1, fun pat_once_dec/4, "od", "Mutate patterns"}%%,
               %%{1, fun pat_many_dec/4, "nd", "Mutate possibly many times"},
               %%{1, fun pat_burst/4, "bu", "Make several mutations closeby once"}
                ].

%% TODO: rewrite?
string_patterns(PatDefaultList) -> mux_patterns(lists:map(fun ({Pri, F, _, _}) -> {Pri, F} end, PatDefaultList)).

%% [{Pri, Pat}, ...] -> fun(rs, ll, muta, meta) .. pattern_output .. end
mux_patterns(Patterns) ->
    {SortedPatterns, N} = shared:sort_by_priority(Patterns),
    fun(Rs, Ll, Muta, Meta) ->
        RIdxPs = owllisp:rand(N),
        PatF = shared:choose_pri(SortedPatterns, RIdxPs),
        PatF(Rs, Ll, Muta, Meta)
    end.