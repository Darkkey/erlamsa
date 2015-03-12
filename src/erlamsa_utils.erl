%%%-------------------------------------------------------------------
%%% @author dark_k3y
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(erlamsa_utils).
-author("dark_k3y").

-include("erlamsa.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

%% API
-export([uncons/2, extract_function/1, forcell/1, last/1,
        merge/2, hd_bin/1, tl_bin/1, choose_pri/2, is_pair/1,
        flush_bvecs/2, applynth/3, sort_by_priority/1, stderr_probe/2]). 

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

sort_by_priority(L) ->
    SL = lists:sort(fun ({A,_},{B,_}) -> A > B end, L),
    N = lists:foldl(fun ({A, _}, Acc) -> Acc + A end, 0, SL),
    {SL, N}.

%% last member find
last(L) when is_list(L) -> last(lists:last(L));
%% last(F) when is_function(F) -> shared:debug(F()), last(F);
last(X) -> X.

%% check if pair
is_pair([_ | T]) when T =/= [] -> true;
is_pair(_) -> false.

hd_bin(<<>>) -> <<>>;
hd_bin(<<A:8, _/binary>>) -> A.
tl_bin(<<>>) -> <<>>;
tl_bin(<<_:8, B/binary>>) -> B.

%% TODO: Check if correct <---!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
merge(false, B) -> B;
merge(A, B) -> <<A/binary, B/binary>>.

%% [{Pri, Fun}, ...], N -> Fun
choose_pri([{This, Fun }| _], N) when N < This ->
     Fun;
choose_pri([{This, _ }| Tail], N) ->
    choose_pri(Tail, N - This).

%% TODO: write to stderr, not stdin
stderr_probe(nil, Value) -> Value;
stderr_probe(Thing, Value) -> io:write(Thing), Value.

%% (byte ...) bvec-ll -> (bvec0, bvec1 ... bvec-ll) ; bvecs are limited by AVG_BLOCK_SIZE
flush_bvecs(Byte_list, Tail) -> flush_bvecs(byte_size(Byte_list), Byte_list, Tail).

flush_bvecs(Len, Lst, Tail) when Len < ?AVG_BLOCK_SIZE -> [Lst | Tail];
flush_bvecs(Len, Lst, Tail) ->
    <<H:?AVG_BLOCK_SIZE_BITS, T/binary>> = Lst,
    [<<H:?AVG_BLOCK_SIZE_BITS>> | flush_bvecs(Len - ?AVG_BLOCK_SIZE, T, Tail)].

%% More effective operations for changing at a position of a list
%% for explanaction while we're not using lists:... see 
%% http://stackoverflow.com/questions/4776033/how-to-change-an-element-in-a-list-in-erlang
%% , comment by rvirding. Thank you, rvirding!
%% WARN: it's not tail-recursive, so could consume some memory, 
%% but it's really faster on LONG lists

%% applynth(Index, List, Fun) -> List.
%% Fun (Elem, Rest) -> RestList
applynth(1, [E|Rest], Fun) -> Fun(E, Rest);
applynth(I, [E|Rest], Fun) -> [E|applynth(I-1, Rest, Fun)].


