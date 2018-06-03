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

-type mutator_cont_fun() :: fun((any(), mutator(), meta_list()) -> list()).

%% during mutation a very large block could appear, here we splitting it into
%% blocks that could be processed by erlang bitstring engine
split_into_maxblocks(This, Acc) when byte_size(This) > ?ABSMAX_BINARY_BLOCK ->
    S = ?ABSMAXHALF_BINARY_BLOCK,
    AS = (S + erlamsa_rnd:rand(S) - 1)*8,
    <<A:AS, B/binary>> = This,
    split_into_maxblocks(B, [<<A:AS>> | Acc]);
split_into_maxblocks(This, Acc) ->
    [This | Acc].

split(U = {false, _LlN}) ->
    U;
split({This, LlN}) when is_binary(This), byte_size(This) > ?ABSMAX_BINARY_BLOCK ->
    Lst = split_into_maxblocks(This, []),
    [H |T] = erlamsa_utils:cons_revlst(Lst, LlN),
    {H, T};
split(U) ->
    U.

%% unpack until we got [ binary(), ..., binary(), { muta, meta } ]
%% TODO: move to utils?
-spec prepare4sizer(list(any())) -> {binary(), any()}.
prepare4sizer(L) -> 
    Res = prepare4sizer(L, []),
    %io:format("~p~n", [Res]),
    Res.

-spec prepare4sizer(list(any()), list()) -> {binary(), any()}.
prepare4sizer([T], Acc) when is_list(T) -> 
    prepare4sizer(T, Acc);
prepare4sizer([H|T], Acc) when is_binary(H) -> 
    prepare4sizer(T, [H|Acc]);
prepare4sizer([H|T], Acc) when is_function(H) -> 
    prepare4sizer([H()|T], Acc);
prepare4sizer(H, Acc) -> 
    {erlang:iolist_to_binary(lists:reverse(Acc)), H}.

%%TODO: fix spec
-spec mutate_once_sizer(binary(), any(), any(), integer(), mutator(), 
                        meta_list(), mutator_cont_fun()) -> list().
mutate_once_sizer(Binary, [], Rest, Ip, Mutator, Meta, NextPat) ->
    %% do nothing, go for next pattern
    SizerMeta = [{sizer, failed} | Meta],
    {This, LlN} = split({Binary, Rest}),
    if
        This /= false ->
            mutate_once_loop(Mutator, SizerMeta, NextPat, Ip, This, LlN);
        true ->
            NextPat([], Mutator, SizerMeta)
    end;
mutate_once_sizer(Binary, Elem = {ok, Size, Len, A, _B}, Rest, Ip, Mutator, Meta, NextPat) ->
    Am8 = A * 8, Len8 = Len * 8,
    <<H:Am8, Len:Size, Blob:Len8, TailBin/binary>> = Binary,
    {This, LlN} = split({<<Blob:Len8>>, Rest}),
    SizerMeta = [{sizer, Elem} | Meta],
    {NewBlob, RestLst} = prepare4sizer(mutate_once_loop(Mutator, SizerMeta, NextPat, Ip, This, LlN)),
    NewLen = size(NewBlob),
    %io:format(standard_error, "~nNewLen = ~p/~p/~p/~p~n", [NewLen, NewBlob, TailBin, RestLst]),
    NewBin = <<H:Am8, NewLen:Size, NewBlob/binary>>,
    [NewBin, TailBin | RestLst].

%% mutate_once for sizer pattern
-spec mutate_once_sizer(any(), mutator(), meta_list(), mutator_cont_fun()) -> list().
mutate_once_sizer(Ll, Mutator, Meta, NextPat) -> 
    Ip = erlamsa_rnd:rand(?INITIAL_IP),
    {Bin, Rest} = erlamsa_utils:uncons(Ll, false),
    Elem = erlamsa_rnd:rand_elem(erlamsa_len_predict:get_possible_simple_lens(Bin)),
    %io:format("Elem = ~p ~p ~p~n", [Elem, size(Bin), NextPat]),
    mutate_once_sizer(Bin, Elem, Rest, Ip, Mutator, Meta, NextPat).

%% mutate_once for skipper pattern
-spec mutate_once_skipper(any(), mutator(), meta_list(), mutator_cont_fun()) -> list().
mutate_once_skipper(Ll, Mutator, Meta, NextPat) -> 
    Ip = erlamsa_rnd:rand(?INITIAL_IP),
    {Bin, Rest} = erlamsa_utils:uncons(Ll, false),
    Len = erlamsa_rnd:rand(trunc(size(Bin)/2))*8,
    <<HeadBin:Len, TailBin/binary>> = Bin,
    {This, LlN} = split({TailBin, Rest}),
    SkipperMeta = [{skipped, Len/8} | Meta],
    Res =   if
                This /= false ->
                    mutate_once_loop(Mutator, SkipperMeta, NextPat, Ip, This, LlN);
                true ->
                    [{Mutator, SkipperMeta}]
            end,
    [<<HeadBin:Len>>| Res].

%% Ll -- list of smth
%% TODO: WARNING: Ll could be a function in Radamsa terms
%% TODO: WARNING: check this code!
%% TODO: temporary contract, fix it.
-spec mutate_once(any(), mutator(), meta_list(), mutator_cont_fun()) -> list().
mutate_once([<<>>], Mutator, Meta, _Cont) ->
    %% FIXME: We're not calling Cont here, it's ok UNTIL we have pat that accepts empty binary
    {Mutator, [{mutate_once, empty_stopped}|Meta]};
mutate_once(Ll, Mutator, Meta, Cont) ->
    Ip = erlamsa_rnd:rand(?INITIAL_IP),
    {This, LlN} = split(erlamsa_utils:uncons(Ll, false)),
    if
        This /= false ->
            mutate_once_loop(Mutator, Meta, Cont, Ip, This, LlN);
        true ->
            Cont([], Mutator, Meta)
    end.

%% TODO: temporary contract, fix it.
-spec mutate_once_loop(mutator(), meta_list(), mutator_cont_fun(), non_neg_integer(), any(), any())
    -> list().
mutate_once_loop(Mutator, Meta, Cont, Ip, This, Ll) when is_function(Ll) ->
    mutate_once_loop(Mutator, Meta, Cont, Ip, This, Ll());
mutate_once_loop(Mutator, Meta, Cont, Ip, This, Ll) ->
    N = erlamsa_rnd:rand(Ip),
    if
        N =:= 0 orelse Ll =:= [] ->  %% or TODO: Ll == nil???
            %io:format("~p~p~p~n", [Mutator, Meta, [This | Ll]]),
            {M, L, Mt} = Mutator([This | Ll], Meta),
            %io:format("ll empty ~p~n",[Meta]),
            Cont(L, M, Mt);
        true ->
            %io:format("ll not empty ~p~n",[Meta]),
            [This, fun () -> mutate_once_loop(Mutator, Meta, Cont, Ip, hd(Ll), tl(Ll)) end]
    end.


%% Patterns:
%% TODO: check what is really used, should we use ++ or can just return tuple {list, {...}} or smth

%% pat :: ll muta meta -> ll' ++ (list (tuple mutator meta))
%% WARNING: in radamsa it should return list ++ tuple{mutator, meta}, here
%% we're returning [list ++ {mutator, meta}]
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
    mutate_once(Ll, Mutator, [{pattern, burst} | Meta], fun pat_burst_cont/3).


-spec make_complex_pat(fun(), atom()) -> fun().
make_complex_pat(MutatorFun, Type) ->
    fun (Ll, Mutator, Meta) ->
        {_, ContPatF, _, _} = erlamsa_rnd:rand_elem(patterns()),
        MutatorFun(Ll, Mutator, [{pattern, Type} | Meta], ContPatF)
    end.

-spec make_pat_skip() -> fun().
make_pat_skip() ->
    make_complex_pat(fun mutate_once_skipper/4, skipper).

-spec make_pat_sizer() -> fun().
make_pat_sizer() ->
    make_complex_pat(fun mutate_once_sizer/4, sizer).


%% TODO: temporary contract, fix it.
-spec pat_nomuta(any(), mutator(), meta_list()) -> list().
pat_nomuta(Ll, Mutator, Meta) ->
    {This, LlN} = split(erlamsa_utils:uncons(Ll, false)),
    [This | LlN] ++ [{Mutator, [{pattern, no_muta} | Meta]}].

%% /Patterns

-spec patterns() -> [pattern()].
patterns() -> [{1, fun pat_once_dec/3, od, "Mutate once pattern"},
               {2, fun pat_many_dec/3, nd, "Mutate possibly many times"},
               {1, fun pat_burst/3, bu, "Make several mutations closeby once"},
               {1, make_pat_skip(), sk, "Skip random sized block and mutate rest"},
               {1, make_pat_sizer(), sz, "Try to find sizer and mutate enclosed data"},
               {0, fun pat_nomuta/3, nu, "Pattern that calls no mutations"}
              ].

-spec default() -> [{atom(), non_neg_integer()}].
default() -> [{Name, Pri} || {Pri, _, Name, _} <- patterns()].

-spec tostring(list()) -> string().
tostring(Lst) ->
    lists:foldl(fun ({_Pri, _Fun, Name, _Desc}, Acc) ->
        atom_to_list(Name) ++ "," ++ Acc
    end, [], Lst).

%% TODO: rewrite?
%-spec string_patterns([pattern()]) -> fun((any(), mutator(), meta_list()) -> list()).
%string_patterns(PatDefaultList) -> mux_patterns(lists:map(fun ({Pri, F, _, _})
%% -> {Pri, F} end, PatDefaultList)).
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

-spec choose_pattern_fun({[pattern()], integer()}) -> fun().
choose_pattern_fun({SortedPatterns, N}) -> 
    RIdxPs = erlamsa_rnd:rand(N),
    erlamsa_utils:choose_pri(SortedPatterns, RIdxPs).

%% [{Pri, Pat}, ...] -> fun(rs, ll, muta, meta) .. pattern_output .. end
-spec mux_patterns([pattern()]) -> fun((any(), mutator(), meta_list()) -> list()).
mux_patterns(Patterns) ->
    SortedPats = erlamsa_utils:sort_by_priority(Patterns),
    fun(Ll, Muta, Meta) ->
        PatF = choose_pattern_fun(SortedPats),
        PatF(Ll, Muta, Meta)
    end.