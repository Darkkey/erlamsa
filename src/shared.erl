%%%-------------------------------------------------------------------
%%% @author dark_k3y
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(shared).
-author("dark_k3y").

-include("erlamsa.hrl").

-compile([export_all]).

%% API
-export([merge/2]). 

sort_by_priority(L) ->
    SL = lists:sort(fun ({A,_},{B,_}) -> A > B end, L),
    N = lists:foldl(fun ({A, _}, Acc) -> Acc + A end, 0, SL),
    {SL, N}.

tst() -> 12.

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

%% debug output
debug(P) -> ok. %%debug("dbg: ", P).
debug(H, P) -> ok. %%io:put_chars(H), io:write(P), io:put_chars("\n").
debug_str(P) -> ok. %%io:put_chars("dbg: "), io:put_chars(P), io:put_chars("\n").

interesting_numbers() ->
    lists:foldl(
        fun (I, Acc) ->
            X = 1 bsl I,
            [X-1, X, X+1 | Acc]
            end,
        [],
        [1, 7, 8, 15, 16, 31, 32, 63, 64, 127, 128]).

%% strange mutate-num
mutate_num(Num) -> mutate_num(Num, owllisp:rand(12)).

mutate_num(Num, 0) -> Num + 1; %% in randamsa n + 1 which is exactly 1; Bug in Radamsa?
mutate_num(Num, 1) -> Num - 1; %% in randamsa n - 1 which is exactly 0; Bug in Radamsa?
mutate_num(_, 2) -> 0;
mutate_num(_, 3) -> 1;
mutate_num(_, N) when N > 2 andalso N < 7 -> owllisp:rand_elem(interesting_numbers());
mutate_num(Num, 7) -> Num + owllisp:rand_elem(interesting_numbers());
mutate_num(Num, 8) -> Num - owllisp:rand_elem(interesting_numbers());
mutate_num(Num, 9) -> Num - owllisp:rand(Num*2); %% in radamsa n*2 which is exactly 18 (9*2); Bug in Radamsa?
mutate_num(Num, _) ->
    N = owllisp:rand_range(1, 129),
    L = owllisp:rand_log(N),
    S = owllisp:rand(3),
    case S of
        0 -> Num - L;
        _Else -> Num + L
    end.


