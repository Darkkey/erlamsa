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
%%% Util functions of different designation
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
-export([cons_revlst/2, uncons/2, extract_function/1, verb/2, forcell/1, last/1, get/3,
        merge/2, hd_bin/1, tl_bin/1, choose_pri/2, is_pair/1,
        flush_bvecs/2, applynth/3, sort_by_priority/1,
        check_empty/1, stderr_probe/2, halve/1, error/1,
        resolve_addr/1, make_post/1, make_fuzzer/1, make_mutas/1,
        load_deps/1, get_direct_fuzzing_opts/2]).

load_deps(RuntimeDir) ->
    true and ?LOAD_PROCKET(RuntimeDir).

get_direct_fuzzing_opts(Data, Opts) ->
    maps:put(paths, [direct],
        maps:put(output, return,
            maps:put(input, Data, Opts))).

cons_revlst([H|T], L) ->
    cons_revlst(T, [H|L]);
cons_revlst([], L) ->
    L.

%% l -> hd l' | error
-spec uncons(list() | binary() | fun(), any()) -> any().
uncons([L|T], _) -> {L, T};
uncons([], D) -> {D, []};
uncons(B, _) when is_binary(B) -> {B, []};
uncons(L, D) when is_function(L) -> uncons(L(), D).

%% extract function value
-spec extract_function(any()) -> any().
extract_function([X]) -> extract_function(X); %% <-- may be ambigious
extract_function(X) when is_function(X) -> extract_function(X());
extract_function(X) -> X.

-spec verb(file:io_device() | standard_error | standard_io, non_neg_integer()) -> fun().
verb(_Fd, V) when V < 1 -> fun(X) -> X end;
verb(stdout, _I) -> fun(X) -> io:format("~s", [X]) end;
verb(stderr, _I) -> fun(X) -> io:format(standard_error, "~s", [X]) end;
verb(Fd, _I) -> fun(X) -> io:format(Fd, "~s", [X]) end.

%% forcing ll def
-spec forcell(list() | function()) -> any().
forcell([]) -> [];
forcell([H|T]) -> [H | forcell(T)];
forcell(X) when is_function(X) -> X().

-spec sort_by_priority(prioritized_list()) -> {prioritized_list(), number()}.
sort_by_priority(L) ->
    SL = lists:sort(fun ({A, _}, {B, _}) -> A > B end, L),
    N = lists:foldl(fun ({A, _}, Acc) -> Acc + A end, 0, SL),
    {SL, N}.

%% last member find
-spec last(any()) -> any().
last(L) when is_list(L) -> last(lists:last(L));
last(X) -> X.

%% check if pair
-spec is_pair(any()) -> true | false.
is_pair([_ | T]) when T =/= [] -> true;
is_pair(_) -> false.

-spec hd_bin(binary()) -> byte() |binary().
hd_bin(<<>>) -> <<>>;
hd_bin(<<A:8, _/binary>>) -> A.

-spec tl_bin(binary()) -> binary().
tl_bin(<<>>) -> <<>>;
tl_bin(<<_:8, B/binary>>) -> B.


-spec halve(list()) -> {list(), list()}.
%% split list into two halves
%% lst -> a b, a ++ b == lst, length a = length b +/- 1
%% ?faster than lists:split(length(..)/2, ..), which takes O(n^2)
halve(Lst) -> list_halves_walk(Lst, Lst, []).

-spec list_halves_walk(list(), list(), list()) -> {list(), list()}.
%% TODO: ugly, need refactor
list_halves_walk(T, [], Acc) ->
    {lists:reverse(Acc), T};
list_halves_walk(T, [_Head|Tail], Acc) when Tail =:= [] ->
    {lists:reverse(Acc), T};
list_halves_walk([A|B], [_Head|Tail], Acc) ->
    list_halves_walk(B, tl(Tail), [A | Acc]).

%% TODO: Check if correct <---!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-spec merge(false | binary(), binary()) -> binary().
merge(false, B) -> B;
merge(A, B) -> <<A/binary, B/binary>>.

%% [{Pri, El}, ...], N -> El
-spec choose_pri(prioritized_list(), number()) -> any().
choose_pri([{_, El}| _], 0) ->  %% Handle case with 0-only elements
    El;
choose_pri([{This, El }| _], N) when N < This ->
     El;
choose_pri([{This, _ }| Tail], N) ->
    choose_pri(Tail, N - This).

%% TODO: write to stderr, not stdin
-spec stderr_probe(any(), any()) -> any().
stderr_probe(nil, Value) -> Value;
stderr_probe(Thing, Value) -> io:write(Thing), Value.

%% (byte ...) bvec-ll -> (bvec0, bvec1 ... bvec-ll) ; bvecs are limited by AVG_BLOCK_SIZE
-spec flush_bvecs(binary(), [binary()]) -> [binary()].
flush_bvecs(Bin, Tail) -> flush_bvecs(byte_size(Bin), Bin, Tail).

-spec flush_bvecs(non_neg_integer(), binary(), [binary()]) -> [binary()].
flush_bvecs(Len, Bin, Tail) when Len < ?AVG_BLOCK_SIZE -> [Bin | Tail];
flush_bvecs(Len, Bin, Tail) ->
    <<H:?AVG_BLOCK_SIZE_BITS, T/binary>> = Bin,
    [<<H:?AVG_BLOCK_SIZE_BITS>> | flush_bvecs(Len - ?AVG_BLOCK_SIZE, T, Tail)].

-spec check_empty([binary()]) -> [] | [binary()].
check_empty([<<>>]) -> [];
check_empty(X) when is_list(X) -> X.

%% More effective operations for changing at a position of a list
%% for explanaction while we're not using lists:... see
%% http://stackoverflow.com/questions/4776033/how-to-change-an-element-in-a-list-in-erlang
%% , comment by rvirding. Thank you, rvirding!
%% WARN: it's not tail-recursive, so could consume some memory,
%% but it's really faster on LONG lists

%% applynth(Index, List, Fun) -> List.
%% Fun (Elem, Rest) -> RestList
-spec applynth(pos_integer(), list(), fun()) -> list().
applynth(1, [E|Rest], Fun) -> Fun(E, Rest);
applynth(I, [E|Rest], Fun) -> [E|applynth(I-1, Rest, Fun)].

%% TODO: Should be replaced by applynth
%% apply function to list element at position N
%% Fun MUST return list!
%% WARN: TODO: Ugly, need rewrite
% led(L, Pos, Fun) ->
%     % io:write({L, Pos, Fun, lists:sublist(L, Pos - 1), Fun(lists:nth(Pos,L)),
%     %   lists:nthtail(Pos,L)}),
%     lists:sublist(L, Pos - 1) ++ Fun(lists:nth(Pos,L)) ++ lists:nthtail(Pos,L).

%% get an element by key from gb_tree, return DefaultValue if none exists
-spec get(any(), any(), gb_trees:tree()) -> any().
get(Key, DefaultValue, Tree) ->
    case gb_trees:lookup(Key, Tree) of
        {value, Value} -> Value;
        none -> DefaultValue
    end.

-spec error(any()) -> any().
error(Err = {_Reason, Desc}) ->
    io:format(standard_error, "Error: ~s~n~n", [Desc]),
    erlamsa_logger:log(error, "Error: ~s", [Desc]),
    throw(Err);
error(Err) ->
    io:format(standard_error, "Error: ~s~n~n", [Err]),
    erlamsa_logger:log(error, "Error: ~s", [Err]),
    throw(Err).

resolve_addr(Host) ->
    {ok, Address} = inet:getaddr(Host, inet),
    Address.

make_post(nil) ->
    fun (Data) -> Data end;
make_post(ModuleName) ->
    fun (Data) -> erlang:apply(list_to_atom(ModuleName), post, [Data]) end.

make_fuzzer(nil) ->
    fun (_Proto, Data, _Opts) -> Data end;
make_fuzzer(ModuleName) ->
    fun (Proto, Data, Opts) ->
        erlang:apply(list_to_atom(ModuleName), fuzzer, [Proto, Data, Opts])
    end.

make_mutas(nil) ->
    [];
make_mutas(ModuleName) ->
    erlang:apply(list_to_atom(ModuleName), mutations, []).

