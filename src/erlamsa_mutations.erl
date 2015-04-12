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
%%% Mutations definitions.
%%% @end
%%%-------------------------------------------------------------------
-module(erlamsa_mutations).
-author("dark_k3y").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

-include("erlamsa.hrl").

%% API
-export([make_mutator/1, mutations/0, default/0, tostring/1]).
 

-define(MIN_SCORE, 2).
-define(MAX_SCORE, 10).
-define(RAND_DELTA, 18446744073709551616).

-type byte_edit_fun() :: fun((byte()) -> binary()).
-type text_mutators() :: insert_badness | replace_badness | insert_aaas | insert_null | insert_delimeter.

%% quick peek if the data looks possibly binary
%% quick stupid version: ignore UTF-8, look for high bits
-spec binarish(binary()) -> boolean().
binarish(Lst) -> binarish(Lst, 0).

-spec binarish(binary(), non_neg_integer()) -> boolean().
binarish(_, P) when P =:= 8 -> false;
binarish(<<>>, _) -> false;
binarish(<<H:8,_/binary>>, _) when H =:= 0 -> true;
binarish(<<H:8,_/binary>>, _) when H band 128 =/= 0 -> true;
binarish(<<_:8,T/binary>>, P) -> binarish(T, P+1).

-spec edit_byte_vector(binary(), non_neg_integer(), byte_edit_fun()) -> binary().
%% clone a byte vector and edit at given position
edit_byte_vector(BVec = <<>>, _, _) -> BVec;
edit_byte_vector(BVec, Edit_pos, Func) ->
    Head_cnt = Edit_pos*8,
    <<H:Head_cnt, B:8, T/binary>> = BVec,
    C = Func(B),    
    <<H:Head_cnt, C/binary, T/binary>>.

%%%
%%% Number Mutator
%%%

-spec interesting_numbers() -> list(integer()).
interesting_numbers() ->
    lists:foldl(
        fun (I, Acc) ->
            X = 1 bsl I,
            [X-1, X, X+1 | Acc]
            end,
        [],
        [1, 7, 8, 15, 16, 31, 32, 63, 64, 127, 128]).

-spec mutate_num(integer()) -> integer().
mutate_num(Num) -> mutate_num(Num, erlamsa_rnd:rand(12)).

-spec mutate_num(integer(), 0..12) -> integer().
mutate_num(Num, 0) -> Num + 1; %% in randamsa n + 1 which is exactly 1; Bug in Radamsa?
mutate_num(Num, 1) -> Num - 1; %% in randamsa n - 1 which is exactly 0; Bug in Radamsa?
mutate_num(_, 2) -> 0;
mutate_num(_, 3) -> 1;
mutate_num(_, N) when N > 2 andalso N < 7 -> erlamsa_rnd:rand_elem(interesting_numbers());
mutate_num(Num, 7) -> Num + erlamsa_rnd:rand_elem(interesting_numbers());
mutate_num(Num, 8) -> Num - erlamsa_rnd:rand_elem(interesting_numbers());
mutate_num(Num, 9) -> Num - erlamsa_rnd:rand(Num*2); %% in radamsa n*2 which is exactly 18 (9*2); Bug in Radamsa?
mutate_num(Num, _) ->
    N = erlamsa_rnd:rand_range(1, 129),
    L = erlamsa_rnd:rand_log(N),
    S = erlamsa_rnd:rand(3),
    case S of
        0 -> Num - L;
        _Else -> Num + L
    end.

-spec get_num(binary()) -> {integer() | false, binary()}.
get_num(Bin) -> get_num(Bin, 0, 0).

-spec get_num(binary(), integer(), non_neg_integer()) -> {integer() | false, binary()}.
get_num(<<>>, _, 0) -> {false, <<>>};
get_num(<<>>, N, _) -> {N, <<>>};
get_num(<<D:8,T/binary>>, N, Digits) when (D >= 48) and (D =< 57) ->
    get_num(T, D - 48 + N*10, Digits + 1);
get_num(Lst, _, 0) -> {false, Lst};
get_num(Lst, N, _) -> {N, Lst}.

-spec copy_range(binary(), binary(), binary()) -> binary().
copy_range(Pos, Pos, Tail) -> Tail;
copy_range(<<H:8,T/binary>>, End, Tail) -> NT = copy_range(T, End, Tail), <<H:8, NT/binary>>.

-spec mutate_a_num(binary(), integer()) -> {integer(), binary()}.
mutate_a_num(<<>>, NFound) ->
    Which = erlamsa_rnd:rand(NFound),
    {Which, <<>>};
mutate_a_num(Lst = <<H:8, T/binary>>, NFound) ->
    {ValP, LstP} = get_num(Lst),
    if
        ValP =/= false ->
            {Which, Tail} = mutate_a_num(LstP, NFound + 1),
            case Which of
                0 ->
                    NewNum = mutate_num(ValP),
                    BinNewNum = list_to_bitstring(integer_to_list(NewNum)),
                    {-1, erlamsa_utils:merge(BinNewNum, Tail)};
                _Else ->
                    {Which - 1, copy_range(Lst, LstP, Tail)}
            end;
        true ->
            {Which, Tail} = mutate_a_num(T, NFound),
            {Which, <<H:8, Tail/binary>>}
    end.

-spec sed_num(list_of_bins(), meta_list()) -> mutation_res().
sed_num([H|T], Meta) ->
    {N, Lst} = mutate_a_num(H, 0),
    IsBin = binarish(Lst),
    FlushedLst = erlamsa_utils:flush_bvecs(Lst, T),
    if
        N =:= 0 ->
            R = erlamsa_rnd:rand(10), %% low priority negative, because could be textual with less frequent numbers
            case R of
                0 -> {fun sed_num/2, FlushedLst, [{muta_num, 1}|Meta], -1};
                _Else -> {fun sed_num/2, FlushedLst, [{muta_num, 1}|Meta], 0}
            end;
        IsBin =:= true ->
            {fun sed_num/2, FlushedLst, [{muta_num, 1}|Meta], -1};
        true ->
            {fun sed_num/2, FlushedLst, [{muta_num, 1}|Meta], +2}
    end.


%%%
%%% Single Byte-level Mutations
%%%
-spec construct_sed_byte_muta(byte_edit_fun(), atom()) -> mutation_fun().
construct_sed_byte_muta(F, Name) ->
    fun Self([H|T], Meta) ->
        P = erlamsa_rnd:rand(byte_size(H)),
        D = erlamsa_rnd:rand_delta(),
        {Self, [edit_byte_vector(H, P, F) | T], [{Name, D}|Meta], D}
    end.

-spec construct_sed_byte_drop() -> mutation_fun().
construct_sed_byte_drop() ->  %% drop byte
    construct_sed_byte_muta(fun (B) -> <<B:0>> end, byte_drop).

-spec construct_sed_byte_inc() -> mutation_fun().
construct_sed_byte_inc() ->  %% inc byte mod 256
    construct_sed_byte_muta(fun (B) -> C = (B + 1) band 255, <<C:8>> end, byte_inc).

-spec construct_sed_byte_dec() -> mutation_fun().
construct_sed_byte_dec() ->  %% dec byte mod 256
    construct_sed_byte_muta(fun (B) -> C = (B - 1) band 255, <<C:8>> end, byte_dec).

-spec construct_sed_byte_repeat() -> mutation_fun().
construct_sed_byte_repeat() ->  %% repeat a byte
    construct_sed_byte_muta(fun (B) -> <<B:8, B:8>> end, byte_repeat).

-spec construct_sed_byte_flip() -> mutation_fun().
construct_sed_byte_flip() ->  %% flip a bit in a byte
    construct_sed_byte_muta(
        fun (B) -> 
            Flip = erlamsa_rnd:rand(8),
            Mask = 1 bsl Flip,
            C = B bxor Mask, 
            <<C:8>> 
        end, byte_flip).

-spec construct_sed_byte_insert() -> mutation_fun().
construct_sed_byte_insert() ->  %% insert a byte
    construct_sed_byte_muta(
        fun (B) -> 
            NewByte = erlamsa_rnd:rand(256),
            <<NewByte:8, B:8>> 
        end, byte_insert).

-spec construct_sed_byte_random() -> mutation_fun().
construct_sed_byte_random() ->  %% swap a byte with a random one
    construct_sed_byte_muta(
        fun (_B) -> 
            NewByte = erlamsa_rnd:rand(256),
            <<NewByte:8>> 
        end, byte_swap_random).

%%%
%%% Multiple Byte-level Mutations
%%%
%%% Warning: this implememntations are not radamsa-like, only making "similar" things, but in the other way and with slight differences

%-spec construct_sed_bytes_muta(byte_edit_fun(), atom()) -> mutation_fun().
-spec construct_sed_bytes_muta(fun(), atom()) -> mutation_fun().
%% WARNING && TODO: this impementation is DIRTY and not as effective as Radamsa
construct_sed_bytes_muta(F, Name) ->
    fun 
    Self([<<>>|BTail], Meta) -> 
        {Self, BTail, [{Name, -1}|Meta], -1};
    Self([BVec|BTail], Meta) ->
        BSize = byte_size(BVec),
        S = erlamsa_rnd:rand(BSize), 
        L = erlamsa_rnd:rand_range(1, BSize - S + 1),  %% FIXME: here any (min 2), in radamsa 20: fixme: magic constant, should use something like repeat-len
                                                   %% ^^ check may be max(20, ...) with "MAGIX" could be MORE effective
                                                   %% TODO: make this interval more random...?
        H_bits = S*8,
        P_bits = L*8, 
        <<H:H_bits, P:P_bits, T/binary>> = BVec,      
        C = F(<<H:H_bits>>, <<P:P_bits>>, T, BTail),
        D = erlamsa_rnd:rand_delta(),
        {Self, C, [{Name, D}|Meta], D}
    end.

-spec construct_sed_bytes_perm() -> mutation_fun().
%% WARNING: in radamsa max permutation block could not exceed length 20, here could be any length
construct_sed_bytes_perm() -> %% permute a few bytes
    construct_sed_bytes_muta(
        fun (H, Bs, T, BTail) -> 
            C = list_to_binary(
            erlamsa_rnd:random_permutation(
                binary_to_list(Bs))),
            [<<H/binary, C/binary, T/binary>> | BTail]
        end, seq_perm).

-spec construct_sed_bytes_repeat() -> mutation_fun().
construct_sed_bytes_repeat() -> %% repeat a seq
    construct_sed_bytes_muta(
        fun (H, Bs, T, BTail) ->            
            N = max(2, erlamsa_rnd:rand_log(10)), %% max 2^10 = 1024 stuts
            %% !FIXME: !WARNING: Below is VERY INEFFECTIVE CODE, just working sketch, may be need to optimize?            
            C = list_to_binary([Bs || _ <- lists:seq(1,N)]),
            Res = [<<H/binary, C/binary ,T/binary>> | BTail],
            Res
        end, seq_repeat).

-spec construct_sed_bytes_drop() -> mutation_fun().
construct_sed_bytes_drop() -> %% drop a seq
    construct_sed_bytes_muta(
        fun (H, _Bs, T, BTail) -> 
            [<<H/binary, T/binary>> | BTail] end, seq_drop).


%%
%% Lines
%%

-spec lines(binary()) -> [string()].
%% bvec -> ([... , 10] .. [... , 10]), cut after newlines
lines(Bvec) -> lines(binary_to_list(Bvec), [], []).

lines([], [], Out) -> lists:reverse(Out);
lines([], Buff, Out) -> lists:reverse([lists:reverse(Buff)] ++ Out);
lines([10|T], Buff, Out) -> lines(T, [], [lists:reverse([10 | Buff]) | Out]);
lines([H|T], Buff, Out) -> lines(T, [H | Buff], Out).

-spec unlines([string()]) -> binary().
%% Lst -> bvec
unlines(Lst) ->
    lists:foldl(fun (X, Acc) -> C = list_to_binary(X), <<Acc/binary, C/binary>> end, <<>>, Lst).

-spec try_lines(binary()) -> [string()] | false.
%% #u8[byte ...] -> ((byte ... 10) ...) | #false, if this doesn't look like line-based text data
%% TODO: ugly code, need a bit refactor
try_lines(Bvec) ->
    Ls = lines(Bvec),
    IsBin = binarish(Bvec),
    if
        Ls =:= [] -> false;
        IsBin =:= true -> false;
        true -> Ls
    end.

-spec construct_line_muta(fun(), atom()) -> mutation_fun().
construct_line_muta(Op, Name) ->
    fun Self(Ll = [H|T], Meta) ->
        Ls = try_lines(H),        
        if
            Ls =:= false ->
                {Self, Ll, Meta, -1};
            true ->
                MLs = Op(Ls, length(Ls)), % calc length only once
                NH = unlines(MLs),
                {Self, [NH | T], [{Name, 1}|Meta], 1} 
        end
    end. 

-spec construct_st_line_muta(fun(), atom(), list()) -> mutation_fun().
%% state is (n <line> ...)
construct_st_line_muta(Op, Name, InitialState) ->
    fun (Ll = [H|T], Meta) ->
        Ls = try_lines(H),
        if
            Ls =:= false ->
                {construct_st_line_muta(Op, Name, InitialState), 
                    Ll, Meta, -1};
            true ->
                {Stp, NewLs} = Op(InitialState, Ls), 
                {construct_st_line_muta(Op, Name, Stp), 
                    [unlines(NewLs) | T], [{Name, 1} | Meta], 1} 
        end
    end.

%%
%% Shared sequences
%%

-spec sed_fuse_this(list_of_bins(), meta_list()) -> mutation_res().
%% (a b ...) -> (a+a b ...)
sed_fuse_this([H|T], Meta) -> % jump between two shared suffixes in the block
    Lst = binary_to_list(H),    
    B = list_to_binary(erlamsa_fuse:fuse(Lst, Lst)),
    D = erlamsa_rnd:rand_delta(),
    {fun sed_fuse_this/2, [B | T], [{fuse_this, D}|Meta], D}.

-spec sed_fuse_next(list_of_bins(), meta_list()) -> mutation_res().
sed_fuse_next([H|T], Meta) ->
    {Al1, Al2} = erlamsa_utils:halve(binary_to_list(H)),
    {B, Ll} = erlamsa_utils:uncons(T, H), % next or current
    Bl = binary_to_list(B),
    Abl = erlamsa_fuse:fuse(Al1, Bl),
    Abal = erlamsa_fuse:fuse(Abl, Al2),
    D = erlamsa_rnd:rand_delta(),
    {fun sed_fuse_next/2,  
        erlamsa_utils:flush_bvecs(list_to_binary(Abal), Ll), %  <- on avg 1x, max 2x block sizes
        [{fuse_next, D}|Meta], D}.

-spec remember(binary()) -> mutation_fun().
remember(Block) ->
    fun ([H|T], Meta) -> 
        %% TODO: Check -- in radamsa here using owllisp halve instead of split
        {Al1, Al2} = erlamsa_utils:halve(binary_to_list(H)),
        {Ol1, Ol2} = erlamsa_utils:halve(binary_to_list(Block)),
        A = erlamsa_fuse:fuse(Al1, Ol1), % a -> o
        B = erlamsa_fuse:fuse(Ol2, Al2), % o -> a
        Swap = erlamsa_rnd:rand(3),
        D = erlamsa_rnd:rand_delta(),
        NewBlock = case Swap of
                     0 -> H;
                     _Else -> Block
                 end,
        {remember(NewBlock),  
            erlamsa_utils:flush_bvecs(list_to_binary(A), % <- on avg 1x, max 2x block sizes
                 erlamsa_utils:flush_bvecs(list_to_binary(B), T)), 
            [{fuse_old, D}|Meta], D}
    end.

-spec sed_fuse_old(list_of_bins(), meta_list()) -> mutation_res().
 sed_fuse_old(Ll = [H|_], Meta) ->
     R = remember(H),
     R(Ll, Meta).


%%
%% ASCII string mutations (use UTF-8 later)
%%

%%% Text mutations

%% check that the nodes do look stringy enough to mutate with these 
%% heuristics, meaning there are some nodes and/or just one, but it's
%% stringy
%% in radamsa this will stop if found byte node; here, we continue
-spec stringy(list(byte())) -> false | true.
stringy([]) -> false;
stringy([{byte, _} | T]) -> false or stringy(T);
stringy(_) -> true. %% in radamsa -- length(Cs)

-spec silly_strings() -> list(string()).
silly_strings() -> % added \r \n \t \b 
    ["%n", "%n", "%s", "%d", "%p", "%#x", [0], "aaaa%d%n", [10], [13], [9], [8]]. 

-spec delimeters() -> list(string()).
delimeters() ->
    ["'", "\"", "'", "\"", "'", "\"", "&", ":", "|", ";",
     "\\", [10], [13], [9], " ", "`", [0], "]", "[", ">", "<"].
    
-spec random_badness() -> list().
random_badness() ->
    random_badness(erlamsa_rnd:rand(20) + 1, []).

-spec random_badness(non_neg_integer(), list()) -> list().
random_badness(0, Out) -> Out;
random_badness(N, Out) ->
    X = erlamsa_rnd:rand_elem(silly_strings()),
    random_badness(N - 1, X ++ Out).

-spec overwrite(list(), list()) -> list().
overwrite([], Old) -> Old;
overwrite([H|T], [_|OldT]) ->
    [H | overwrite(T, OldT)];
overwrite([H|T], Old) ->
    [H | overwrite(T, Old)].

-spec rand_as_count() -> non_neg_integer().
rand_as_count() ->
    Type = erlamsa_rnd:rand(11),
    case Type of
        0 -> 127;
        1 -> 128;
        2 -> 255;
        3 -> 256;
        4 -> 16383;
        5 -> 16384;
        6 -> 32767;
        7 -> 32768;
        8 -> 65535;
        9 -> 65536;
        _Else -> erlamsa_rnd:rand(1024)        
    end.

-spec push_as(non_neg_integer(), list()) -> list().
push_as(0, Tail)
    -> Tail;
push_as(N, Tail) ->
    push_as(N - 1, [97 | Tail]).

-spec mutate_text_data(string(), [text_mutators()]) -> string().
mutate_text_data(Lst, TxtMutators) ->
    mutate_text(erlamsa_rnd:rand_elem(TxtMutators), Lst).

-spec mutate_text(text_mutators(), string()) -> string().
%% insert badness
mutate_text(insert_badness, []) -> random_badness(); %% empty list -- just insert random
mutate_text(insert_badness, Lst) ->
    P = erlamsa_rnd:erand(length(Lst)), % in erlang lists starts from 1
    Bad = random_badness(),
    erlamsa_utils:applynth(P, Lst, fun(E, R) -> Bad ++ [E|R] end); %% TODO: check before or after E, in radamsa Bad ++ [X],
%% replace badness
mutate_text(replace_badness, []) -> random_badness(); %% empty list -- just replace with random
mutate_text(replace_badness, Lst) ->
    P = erlamsa_rnd:erand(length(Lst)), % in erlang lists starts from 1
    Bad = random_badness(),
    lists:sublist(Lst, P - 1) ++ overwrite(lists:nthtail(P, Lst), Bad);
%% insert as
mutate_text(insert_aaas, []) -> push_as(rand_as_count(), []); %% empty list -- just insert random
mutate_text(insert_aaas, Lst) ->
    N = rand_as_count(),
    P = erlamsa_rnd:erand(length(Lst)), % in erlang lists starts from 1    
    lists:sublist(Lst, P - 1) ++ push_as(N, lists:nthtail(P, Lst));
%% insert null
mutate_text(insert_null, Lst) ->
    Lst ++ [0];
%% insert delimeter
mutate_text(insert_delimeter, []) -> [erlamsa_rnd:rand_elem(delimeters())]; %% empty list -- just insert random
mutate_text(insert_delimeter, Lst) ->
    P = erlamsa_rnd:erand(length(Lst)), % in erlang lists starts from 1
    Bad = erlamsa_rnd:rand_elem(delimeters()),
    erlamsa_utils:applynth(P, Lst, fun(E, R) -> Bad ++ [E|R] end). %% TODO: check before or after E
    

%% Generic ASCII Bad mutation
%% In Radamsa, this function will work only if Cs started as string
%% Else, if Cs contains only byte nodes, it will run infinetely
%% Here, we do maximum L/4 runs in this case
%% TODO: WARN: Ineffective, need rewrite/optimize
-spec string_generic_mutate(chunk_list(), [text_mutators()], non_neg_integer(), non_neg_integer()) -> chunk_list().
string_generic_mutate(Cs, _, L, R) when R > L/4 -> Cs;
string_generic_mutate(Cs, TxtMutators, L, R) ->
    P = erlamsa_rnd:erand(L), % in erlang, list is beginning from index 1
    El = lists:nth(P, Cs),
    case El of 
        {text, Bs} -> 
            Data = mutate_text_data(Bs, TxtMutators),
            erlamsa_utils:applynth(P, Cs, fun(_E, Rest) -> [{text, Data} | Rest] end);  % [Node]
        {byte, _Bs} ->
            string_generic_mutate(Cs, TxtMutators, L, R + 1);
        {delimited, Left, Bs, Right} ->
            erlamsa_utils:applynth(P, Cs, fun (_E, Rest) -> [{delimited, Left, mutate_text_data(Bs, TxtMutators), Right}] ++ Rest end)
    end. 

-spec construct_ascii_mutator(fun(), atom()) -> mutation_fun().
construct_ascii_mutator(Fun, Name) ->
    fun Ascii_mutator (Ll = [H|T], Meta) -> 
        Data = binary_to_list(H),
        Cs = erlamsa_strlex:lex(Data),

        case stringy(Cs) of % in radamsa stringy_length
            true ->   % do something bad...
                Ms = Fun(Cs),
                D = erlamsa_rnd:rand_delta(),        
                BinData = list_to_binary(erlamsa_strlex:unlex(Ms)),
                {Ascii_mutator, 
                    [BinData | T], 
                    [{Name, D}|Meta], D};
            false ->  % not a string at all (even 1 node), skipping
                {Ascii_mutator, Ll, Meta, -1} 
        end
    end.

-spec construct_ascii_bad_mutator() -> mutation_fun().
construct_ascii_bad_mutator() ->    
    construct_ascii_mutator(
        fun (Cs) -> string_generic_mutate(Cs, 
                    [insert_badness, replace_badness, insert_aaas, insert_null], 
                    length(Cs), 0)
        end,
        ascii_bad).

-spec drop_delimeter(non_neg_integer(), chunk()) -> chunk().
%% drop one(both) or zero delimeters
drop_delimeter(0, {delimited, Left, Bs, _Right}) -> % drop right
    {text, [Left|Bs]};
drop_delimeter(1, {delimited, _Left, Bs, Right}) -> % drop left
    {text, Bs ++ [Right]} ;
drop_delimeter(2, {delimited, _Left, Bs, _Right}) -> % drop both
    {text, Bs};
drop_delimeter(_, El) -> % drop none
    El.

%% Play with delimeters
-spec string_delimeter_mutate(chunk_list(), non_neg_integer(), non_neg_integer()) -> chunk_list().
string_delimeter_mutate(Cs, L, R) when R > L/4 -> Cs;
string_delimeter_mutate(Cs, L, R) ->
    P = erlamsa_rnd:erand(L), % in erlang, list is beginning from index 1
    El = lists:nth(P, Cs),
    case El of 
        {text, Bs} -> %% insert or drop special delimeter(s)
            Data = mutate_text_data(Bs, [insert_delimeter]),
            erlamsa_utils:applynth(P, Cs, fun(_E, Rest) -> [{text, Data}|Rest] end); % [Node]            
        {byte, _Bs} -> %% do nothing
            string_delimeter_mutate(Cs, L, R + 1);
        {delimited, _Left, _Bs, _Right} ->
            Drop = drop_delimeter(erlamsa_rnd:rand(4), El),
            erlamsa_utils:applynth(P, Cs, fun(_E, Rest) -> [Drop|Rest] end)
    end.

-spec construct_ascii_delimeter_mutator() -> mutation_fun().
construct_ascii_delimeter_mutator() ->    
    construct_ascii_mutator(
        fun (Cs) -> string_delimeter_mutate(Cs, length(Cs), 0)
        end,
        ascii_delimeter).


%%
%% Guessed Parse-tree Mutations
%%

-spec usual_delims(non_neg_integer()) -> non_neg_integer() | false.
%% ?? could be settable from command line
usual_delims(40) -> 41;   % ()
usual_delims(91) -> 93;   % []
usual_delims(60) -> 62;   % <>
usual_delims(123) -> 125; % {}
usual_delims(34) -> 34;   % ""
usual_delims(39) -> 39;   % ''
usual_delims(_) -> false.

-spec grow(list(), byte(), list()) -> {list(), list() | false}.
%% -> lst #false = ran out of data trying to parse up to close, but lst is the same with partial parsing
%% -> lst tail-lst = did successfully parse up to close. ready node is lst, tail is following data
grow([], _Close, Rout) ->  %% out of data, didn't find close. return partial parse.
    {lists:reverse(Rout), false};    
grow([H|T], Close, Rout) when H =:= Close -> %% match complete, return with rest of list
    {lists:reverse([Close | Rout]), T};
grow([H|T], Close, Rout) ->
    Next_close = usual_delims(H),
    case Next_close of
        false -> %% add one byte to this node
            grow(T, Close, [H | Rout]);
        _Else -> 
            {This, Lst} = grow(T, Next_close, []),
            if  %% we didn't run out of data and this is a single tree node
                Lst =:= false ->                    
                    {lists:reverse(Rout) ++ [H | This], false};
                    %% we ran out of data. this is a list of partial parses (having the data of
                    %% lst after hd in some form) which we want to preserve as tail
                true ->
                    grow(Lst, Close, [[H | This] | Rout])
            end
    end.

% -spec count_nodes(list()) -> non_neg_integer().
% %% count how many list nodes are in a tree structure
% count_nodes(Lst) -> count_nodes(Lst, 0).

% -spec count_nodes(list(), non_neg_integer()) -> non_neg_integer().
% count_nodes([], N) -> N;
% count_nodes([H|T], N) when is_list(H) -> 
%     count_nodes(T, count_nodes(H, N+1));
% count_nodes([_|T], N) ->
%     count_nodes(T, N).

-spec sublists(list()) -> list().
%% lst -> a list of lists (not counting tails) in lst, when walked recursively, not counting lst itself
sublists(Lst) -> sublists(Lst, []).

-spec sublists(list(), list()) -> list().
sublists([], Found) -> Found;
sublists([H|T], Found) when is_list(H) ->
    sublists(T, sublists(H, [H|Found]));
sublists([_H|T], Found) ->
    sublists(T, Found).

-spec pick_sublist(list()) -> list().
pick_sublist(Lst) ->
    Subs = sublists(Lst),
    case Subs of
        [] -> false;
        _Else ->
            erlamsa_rnd:rand_elem(Subs) %% TODO: FIXME: CHECK IF it's correct!
    end.

%% TODO: type for fun().
-spec edit_sublist(list(), list(), fun()) -> list().
%% replace the node (sub . tail) with (op (sub . tail))
edit_sublist(Lst = [H|_T], Sub, Op) when H =:= Sub ->
    Op(Lst);
edit_sublist([H|T], Sub, Op) -> 
    [edit_sublist(H, Sub, Op) | edit_sublist(T, Sub, Op)];
edit_sublist(Lst, _Sub, _Op) -> 
    Lst.

%% TODO: type for fun().
-spec edit_sublists(list(), gb_trees:tree()) -> list().
%% lst (ff of node -> (node -> node)) -> lst' ; <- could also be done with a recursive-mapn
edit_sublists([Hd|T], OpFF) when is_list(Hd) ->     
    MaybeOp = erlamsa_utils:get(Hd, false, OpFF),
    case MaybeOp of
        false -> 
            [edit_sublists(Hd, OpFF) | edit_sublists(T, OpFF)];
        _ ->
            [MaybeOp(Hd) | edit_sublists(T, OpFF)]            
    end;
edit_sublists([H|T], OpFF) -> 
    [H | edit_sublists(T, OpFF)];
edit_sublists(Lst, _) -> Lst.

-spec partial_parse(list()) -> list().
partial_parse(Lst) -> partial_parse(Lst, []).

-spec partial_parse(list(), list()) -> list().
partial_parse([], Rout) ->
    lists:reverse(Rout);
partial_parse([H|T], Rout) ->
    CloseP = usual_delims(H),
    case CloseP of
        false ->             
            partial_parse(T, [H | Rout]);
        _ ->
            {This, Lst} = grow(T, CloseP, []),
            case Lst of 
                false -> 
                    lists:reverse(Rout) ++ [H|This];
                _ -> 
                    partial_parse(Lst, [[H|This] | Rout])
            end
    end.

-spec flatten(list(), list()) -> list().
flatten([], Tl) -> Tl;
flatten([H|T], Tl) ->
    flatten(H, flatten(T, Tl));
flatten(Node, Tl) ->
    [Node | Tl].

%% TODO: type for fun().
-spec sed_tree_op(fun(), atom()) -> mutation_fun().
sed_tree_op(Op, Name) ->
    fun F (Ll = [H|T], Meta) ->
        case binarish(H) of
            true -> {F, Ll, Meta, -1};
            false -> 
                NewMeta = [{Name, 1} | Meta],
                Lst = partial_parse(binary_to_list(H)),
                Sub = pick_sublist(Lst), %% choose partially parsed node to mutate ;; fixme: not checked for F
                NewLst = edit_sublist(Lst, Sub, Op),
                {F, [list_to_binary(flatten(NewLst, [])) | T], NewMeta, 1}
        end    
    end.

-spec sed_tree_dup() -> mutation_fun().
sed_tree_dup() ->
    sed_tree_op(fun (Node = [H|_T]) -> [H|Node] end, tree_dup).

-spec sed_tree_del() -> mutation_fun().
sed_tree_del() ->
    sed_tree_op(fun ([_H|T]) -> T end, tree_del).

-spec sed_tree_swap_one(list(), list()) -> list().
%% overwrite one node with one of the others
sed_tree_swap_one(Lst, Subs) ->
    ToSwap = erlamsa_rnd:reservoir_sample(Subs, 2),
    [A | [B | _]] = erlamsa_rnd:random_permutation(ToSwap),
    edit_sublist(Lst, A, fun ([_|Tl]) -> [B | Tl] end).

-spec sed_tree_swap_two(list(), list()) -> list().
%% pairwise swap of two nodes
%% TODO: here there is a bug (also in original radamsa) that causes to swap child with the parent node, could be feature xD
sed_tree_swap_two(Lst, Subs) ->
    ToSwap = erlamsa_rnd:reservoir_sample(Subs, 2),
    [A | [B | _]] = ToSwap,
    Mapping = gb_trees:enter(B, fun(_X) -> A end, gb_trees:enter(A, fun (_X) -> B end, gb_trees:empty())),    
    edit_sublists(Lst, Mapping).

%% TODO: type for fun().
-spec construct_sed_tree_swap(fun(), atom()) -> mutation_fun().
construct_sed_tree_swap(Op, Name) ->
    fun F (Ll = [H|T], Meta) ->
        case binarish(H) of
            true -> {F, Ll, Meta, -1};
            false -> 
                Lst = partial_parse(binary_to_list(H)),
                Subs = sublists(Lst),
                N = length(Subs),
                case N < 2 of
                    true -> {F, Ll, Meta, -1};
                    false ->
                        NewLst = Op(Lst, Subs),
                        {F, [list_to_binary(flatten(NewLst, [])) | T], [{Name, 1} | Meta], 1}
                end
        end
    end.

%% tree stutter
-spec repeat_path(list(), list(), non_neg_integer()) -> list().
repeat_path(Parent, _Child, N) when N < 2 ->
    Parent; %% <- causes at least 1 extra path to be made, saves one useless replace cycle
repeat_path(Parent, Child, N) ->
    edit_sublist(Parent, Child, 
        fun ([_H|T]) -> [repeat_path(Parent, Child, N-1) | T] end).

-spec choose_child(list()) -> false | list().
choose_child(Node) ->
    Subs = sublists(Node),
    case Subs of 
        [] -> false;
        _Else -> erlamsa_rnd:rand_elem(Subs)
    end.

-spec choose_stutr_nodes(list()) -> {false | list(), false | list()}.
choose_stutr_nodes([]) -> {false, false}; %% no child nodes
choose_stutr_nodes([H|T]) ->
    Childp = choose_child(H),
    case Childp of
        false -> choose_stutr_nodes(T);
        _Else -> {H, Childp}
    end.

-spec sed_tree_stutter(list_of_bins(), meta_list()) -> mutation_res().
sed_tree_stutter(Ll = [H|T], Meta) ->
    case binarish(H) of
        true -> {fun sed_tree_stutter/2, Ll, Meta, -1};
        false -> 
            Lst = partial_parse(binary_to_list(H)), %% (byte|node ...)
            Subs = sublists(Lst),
            RandSubs = erlamsa_rnd:random_permutation(Subs),
            {Parent, Child} = choose_stutr_nodes(RandSubs),
            N_reps = erlamsa_rnd:rand_log(10),
            case Parent of 
                false -> {fun sed_tree_stutter/2, Ll, Meta, -1};
                _Else ->
                    NewLst = edit_sublist(Lst, Child, 
                            fun ([_H|Tl]) -> io:format("sed_tree_stutter 6.5~n", []), R = [repeat_path(Parent, Child, N_reps)|Tl], io:format("/sed_tree_stutter 6.5~n", []), R end),
                    {fun sed_tree_stutter/2, 
                        [list_to_binary(flatten(NewLst, [])) | T], 
                        [{tree_stutter, 1} | Meta], 1}
            end
    end.


%%
%% UTF-8
%%

%% grab low 6 bits of a number
-spec ext(integer()) -> integer().
ext(N) ->
    (N band 2#111111) bor 2#10000000.
    
-spec encode_point(integer()) -> [integer()].
%% encode an ascii to utf-8
encode_point(Point) when Point < 16#80 ->                    % ascii, fits 7 bits 
    [Point];
encode_point(Point) when Point < 2#100000000000 ->           % 5 + 6 bits
    [16#c0 bor (16#1f band (Point bsr 6)),
    ext(Point)];
encode_point(Point) when Point < 2#10000000000000000 ->      % 4 + 2*6 bits
    [16#e0 bor (16#0f band (Point bsr 12)),
    ext(Point bsr 6),
    ext(Point)];
encode_point(Point) when Point < 2#1000000000000000000000 -> % 3 + 3*6 bits
    [16#f0 bor (2#111 band (Point bsr 18)),
    ext(Point bsr 12),
    ext(Point bsr 6),
    ext(Point)].

-spec funny_unicode() -> list().
%% TODO: VERY INEFFECTIVE, should be constant...
funny_unicode() -> 
    Manual =   [[239, 191, 191],        % 65535
                [240, 144, 128, 128],   % 65536
                [16#ef, 16#bb, 16#bf],  % the canonical utf8 bom
                [16#fe, 16#ff],         % utf16 be bom
                [16#ff, 16#fe],         % utf16 le bom
                [0, 0, 16#ff, 16#ff],   % ascii null be
                [16#ff, 16#ff, 0, 0],   % ascii null le
                [43, 47, 118, 56],      % and some others from wikipedia
                [43,47,118,57],[43,47,118,43],[43,47,118,47],
                [247,100,76],[221,115,102,115],[14,254,255],[251,238,40],
                [251,238,40,255],[132,49,149,51]],
    Codes =     [[16#0009,16#000d],16#00a0,16#1680,16#180e,
                [16#2000,16#200a],16#2028,16#2029,16#202f,16#205f,
                16#3000,[16#200e,16#200f],[16#202a,16#202e],
                [16#200c,16#200d],16#0345,16#00b7,[16#02d0,16#02d1],
                16#ff70,[16#02b0,16#02b8],16#fdd0,16#034f,
                [16#115f,16#1160],[16#2065,16#2069],16#3164,16#ffa0,
                16#e0001,[16#e0020,16#e007f],
                [16#0e40,16#0e44],16#1f4a9],
    Numbers = lists:foldl(
                fun 
                    ([X,Y], Acc) -> lists:seq(X,Y) ++ Acc; 
                    (X, Acc) -> [X|Acc] 
                end, [], Codes), 
    Manual ++ lists:map(fun (X) -> encode_point(X) end, Numbers).

-spec sed_utf8_widen(list_of_bins(), meta_list()) -> mutation_res().
sed_utf8_widen([H|T], Meta) ->    
    P = erlamsa_rnd:rand(size(H)),
    D = erlamsa_rnd:rand_delta(),
    {fun sed_utf8_widen/2, 
     [edit_byte_vector(H, P, 
        fun (B) when B =:= B band 2#111111 -> N = B bor 2#10000000, <<2#11000000:8, N:8>>;
            (B) -> <<B:8>>
        end)
      | T], [{sed_utf8_widen, D}|Meta], D}.

-spec sed_utf8_insert(list_of_bins(), meta_list()) -> mutation_res().
sed_utf8_insert([H|T], Meta) ->    
    P = erlamsa_rnd:rand(size(H)),
    D = erlamsa_rnd:rand_delta(),
    Bin = list_to_binary(erlamsa_rnd:rand_elem(funny_unicode())),
    {fun sed_utf8_insert/2, 
     [edit_byte_vector(H, P, 
        fun (B) -> <<B:8, Bin/binary>> end)
      | T], [{sed_utf8_insert, D}|Meta], D}.

%%
%%  Main Mutation Functions
%%

-spec adjust_priority(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
%% limit to [min-score .. max-score]
adjust_priority(Pri, 0) -> Pri;
adjust_priority(Pri, Delta) ->
    max(?MIN_SCORE, min(?MAX_SCORE, Pri + Delta)).

-spec weighted_permutations([mutation()]) -> [mutation()]. 
%% [{Score, Priority, _, _}, ...] -> [{Rand(S*P), {...}}, ...] -> sort -> [{...}, ...]
weighted_permutations([]) -> [];
weighted_permutations(Pris) ->
    PPris = lists:map(fun (X = {S, P, _, _}) -> {erlamsa_rnd:rand(S*P), X} end, Pris),
    SPPris = lists:sort(fun ({A,_},{B,_}) -> A>=B end, PPris),
    lists:map(fun ({_, X}) -> X end, SPPris).

%% Mutators have a score they can change themselves (1-100) and a priority given by
%% the user at command line. Activation probability is (score*priority)/SUM(total-scores).

%%TODO: decribe fun() type
-spec mux_fuzzers([mutation()]) -> fun().
%% (#(score priority mutafn name) ...) -> merged-mutafn :: rs ll meta -> merged-mutafn' rs' ll' meta'
mux_fuzzers(Fs) ->
    fun L([], Meta) -> {mux_fuzzers(Fs), <<>>, Meta};
        L(Ll, Meta) when is_list(Ll) ->
            mux_fuzzers_loop(Ll, weighted_permutations(Fs), [], Meta) ;
        L(Ll, Meta) -> L(Ll(), Meta) %% TODO: strange behaviour
    end.

-spec mux_fuzzers_loop(lazy_list_of_bins(), [mutation()], [mutation()], meta_list()) -> {fun(), lazy_list_of_bins(), meta_list()}.
mux_fuzzers_loop(Ll, [], Out, Meta) -> {mux_fuzzers(Out), Ll, Meta};
mux_fuzzers_loop(Ll, [Node|Tail], Out, Meta) ->
    {Mscore, MPri, Fn, Mname} = Node,
    Res = Fn(Ll, Meta),
    {MFn, Mll, MMeta, Delta} = Res, %% in radamsa (mfn rs mll mmeta delta) = Fn(...), that strange, TODO: check it    
    NOut = [{adjust_priority(Mscore, Delta), MPri, MFn, Mname} | Out], %% in radamsa mfn instead of fn
    IsMllPair = erlamsa_utils:is_pair(Mll),
    if
        IsMllPair andalso (hd(Mll) == hd(Ll)) -> mux_fuzzers_loop(Ll, Tail, NOut, MMeta);
        true -> {mux_fuzzers(NOut ++ Tail), Mll, [{used, Mname} | MMeta]}
    end.

-spec mutations() -> [mutation()].
%% default mutations list
mutations() ->          [{10, 5, fun sed_num/2, num, "try to modify a textual number"},
                        {10, 1, fun sed_utf8_widen/2, uw, "try to make a code point too wide"},
                        {10, 2, fun sed_utf8_insert/2, ui, "insert funny unicode"},
                        {10, 1, construct_ascii_bad_mutator(), ab, "enhance silly issues in ASCII string data handling"},
                        {10, 1, construct_ascii_delimeter_mutator(), ad, "play with delimeters in ASCII string data"},
                        {10, 1, sed_tree_dup(), tr2, "duplicate a node"},
                        {10, 1, sed_tree_del(), td, "delete a node"},
                        {10, 1, construct_sed_tree_swap(fun sed_tree_swap_one/2, tree_swap_one), ts1, "swap one node with another one"},
                        {10, 1, construct_st_line_muta(fun erlamsa_generic:st_list_ins/2, list_ins, [0]), lis, "insert a line from elsewhere"},
                        {10, 1, construct_st_line_muta(fun erlamsa_generic:st_list_replace/2, list_replace, [0]), lrs, "replace a line with one from elsewhere"},
                        {10, 1, fun sed_tree_stutter/2, tr, "repeat a path of the parse tree"},
                        {10, 1, construct_sed_tree_swap(fun sed_tree_swap_two/2, tree_swap_two), ts2, "swap two nodes pairwise"},
                        {10, 1, construct_sed_byte_drop(), bd, "drop a byte"},
                        {10, 1, construct_sed_byte_inc(), bei, "increment a byte by one"},
                        {10, 1, construct_sed_byte_dec(), bed, "decrement a byte by one"},
                        {10, 1, construct_sed_byte_flip(), bf, "flip one bit"},
                        {10, 1, construct_sed_byte_insert(), bi, "insert a random byte"},
                        {10, 1, construct_sed_byte_random(), ber, "insert a random byte"},
                        {10, 1, construct_sed_byte_repeat(), br, "repeat a byte"},
                        {10, 1, construct_sed_bytes_perm(), sp, "permute a sequence of bytes"},
                        {10, 1, construct_sed_bytes_repeat(), sr, "repeat a sequence of bytes"},
                        {10, 1, construct_sed_bytes_drop(), sd, "delete a sequence of bytes"},
                        {10, 1, construct_line_muta(fun erlamsa_generic:list_del/2, line_del), ld, "delete a line"},
                        {10, 1, construct_line_muta(fun erlamsa_generic:list_del_seq/2, line_del_seq), lds, "delete many lines"},
                        {10, 1, construct_line_muta(fun erlamsa_generic:list_dup/2, line_dup), lr2, "duplicate a line"},
                        {10, 1, construct_line_muta(fun erlamsa_generic:list_clone/2, line_clone), lri, "copy a line closeby"},
                        {10, 1, construct_line_muta(fun erlamsa_generic:list_repeat/2, line_repeat), lr, "repeat a line"},
                        {10, 1, construct_line_muta(fun erlamsa_generic:list_swap/2, line_swap), ls, "swap two lines"},
                        {10, 1, construct_line_muta(fun erlamsa_generic:list_perm/2, line_perm), lp, "swap order of lines"},
                        {10, 2, fun sed_fuse_this/2, ft, "jump to a similar position in block"},
                        {10, 1, fun sed_fuse_next/2, fn, "likely clone data between similar positions"},
                        {10, 2, fun sed_fuse_old/2, fo, "fuse previously seen data elsewhere"}].

-spec default() -> [{atom(), non_neg_integer()}].
default() -> lists:map(fun ({_, Pri, _, Name, _Desc}) -> {Name, Pri} end, mutations()).

-spec tostring(list()) -> string().
tostring(Lst) -> 
    lists:foldl(fun ({_, Pri, _, Name, _Desc}, Acc) -> 
        case Pri of
            1 -> atom_to_list(Name) ++ "," ++ Acc;
            _Else -> atom_to_list(Name) ++ "=" ++ integer_to_list(Pri) ++ "," ++ Acc
        end
    end, [], Lst).

-spec make_mutator([{atom(), non_neg_integer()}]) -> fun().
make_mutator(Lst) ->
    SelectedMutas = maps:from_list(Lst),
    Mutas = lists:foldl(
        fun ({Score, _Pri, F, Name, _Desc}, Acc) ->
            Val = maps:get(Name, SelectedMutas, notfound),
            case Val of 
                notfound -> Acc;
                _Else -> [{Score, Val, F, Name} | Acc]
            end
        end,
        [],
        mutations()),   
    mutators_mutator(Mutas).

-spec mutators_mutator([mutation()]) -> fun().
%% randomize mutator scores
mutators_mutator(Mutas) ->
    mutators_mutator(Mutas, []).

-spec mutators_mutator([mutation()], [mutation()]) -> fun().
mutators_mutator([], Out) ->
    mux_fuzzers(Out);
mutators_mutator([{_, Pri, F, Name}|T], Out) ->
    N = erlamsa_rnd:rand(?MAX_SCORE),
    mutators_mutator(T, [{max(2, N), Pri, F, Name} | Out]).


