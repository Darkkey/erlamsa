%%%-------------------------------------------------------------------
%%% @author dark_k3y
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(mutations).
-author("dark_k3y").

-define(MIN_SCORE, 2).
-define(MAX_SCORE, 10).
-define(RAND_DELTA, 18446744073709551616).
-define(P_WEAKLY_USUALLY_NOM, 11).
-define(P_WEAKLY_USUALLY_DENOM, 20).
%% minimum texty bytes in sequence to be considered interesting
%% note - likely to happen in longish data (a few kb) anyway
-define(MIN_TEXTY, 6).
-compile([export_all]).

%% API
-export([]).

%%%
%%% Random delta generators for random priority steppers
%%%

%% random delta for brownian steppers
%% strange random delta <-- copied from radamsa,
%% TODO: check for uniformity for Bit && rewrite
rand_delta(_Rs) ->
    Bit = owllisp:rand_bit(),
    case Bit of
        0 ->
            +1;
        _Else ->
            -1
    end.

%% random delta with a slight positive bias
rand_delta_up(_Rs) ->
    Occ = owllisp:rand_occurs(?P_WEAKLY_USUALLY_NOM, ?P_WEAKLY_USUALLY_DENOM),
    case Occ of
        true ->
            +1;
        false ->
            -1
    end.

%% quick peek if the data looks possibly binary
%% quick stupid version: ignore UTF-8, look for high bits
binarish(Lst) -> binarish(Lst, 0).

binarish(_, P) when P =:= 8 -> false;
binarish(<<>>, _) -> false;
binarish(<<H:8,_/binary>>, _) when H =:= 0 -> true;
binarish(<<H:8,_/binary>>, _) when H band 128 =/= 0 -> true;
binarish(<<_:8,T/binary>>, P) -> binarish(T, P+1).

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

get_num(Lst) -> get_num(Lst, 0, 0).

get_num(<<>>, _, 0) -> {false, <<>>};
get_num(<<>>, N, _) -> {N, <<>>};
get_num(<<D:8,T/binary>>, N, Digits) when (D >= 48) and (D =< 57) ->
    get_num(T, D - 48 + N*10, Digits + 1);
get_num(Lst, _, 0) -> {false, Lst};
get_num(Lst, N, _) -> {N, Lst}.

copy_range(Pos, Pos, Tail) -> Tail;
copy_range(<<H:8,T/binary>>, End, Tail) -> NT = copy_range(T, End, Tail), <<H:8, NT/binary>>.

mutate_a_num(Rs, <<>>, NFound) ->
    Which = owllisp:rand(NFound),
    {Rs, Which, <<>>};
mutate_a_num(Rs, Lst = <<H:8, T/binary>>, NFound) ->
    {ValP, LstP} = get_num(Lst),
    if
        ValP =/= false ->
            {_RsN, Which, Tail} = mutate_a_num(Rs, LstP, NFound + 1),
            case Which of
                0 ->
                    NewNum = shared:mutate_num(ValP),
                    BinNewNum = list_to_bitstring(integer_to_list(NewNum)),
                    {Rs, -1, shared:merge(BinNewNum, Tail)};
                _Else ->
                    {Rs, Which - 1, copy_range(Lst, LstP, Tail)}
            end;
        true ->
            {_RsN, Which, Tail} = mutate_a_num(Rs, T, NFound),
            {Rs, Which, <<H:8, Tail/binary>>}
    end.


sed_num(Rs, [H|T], Meta) ->
    {Rs, N, Lst} = mutate_a_num(Rs, H, 0),
    IsBin = binarish(Lst),
    FlushedLst = shared:flush_bvecs(Lst, T),
    if
        N =:= 0 ->
            R = owllisp:rand(10), %% low priority negative, because could be textual with less frequent numbers
            case R of
                0 -> {fun sed_num/3, Rs, FlushedLst, [{muta_num, 1}|Meta], -1};
                _Else -> {fun sed_num/3, Rs, FlushedLst, [{muta_num, 1}|Meta], 0}
            end;
        IsBin =:= true ->
            {fun sed_num/3, Rs, FlushedLst, [{muta_num, 1}|Meta], -1};
        true ->
            {fun sed_num/3, Rs, FlushedLst, [{muta_num, 1}|Meta], +2}
    end.


%%%
%%% Single Byte-level Mutations
%%%

construct_sed_byte_muta(F, Name) ->
    fun Self(Rs, [H|T], Meta) ->
        P = owllisp:rand(byte_size(H)),
        D = rand_delta(Rs),
        {Self, Rs, [edit_byte_vector(H, P, F) | T], [{Name, D}|Meta], D}
    end.

construct_sed_byte_drop() ->  %% drop byte
    construct_sed_byte_muta(fun (B) -> <<B:0>> end, byte_drop).

construct_sed_byte_inc() ->  %% inc byte mod 256
    construct_sed_byte_muta(fun (B) -> C = (B + 1) band 255, <<C:8>> end, byte_inc).

construct_sed_byte_dec() ->  %% dec byte mod 256
    construct_sed_byte_muta(fun (B) -> C = (B - 1) band 255, <<C:8>> end, byte_dec).

construct_sed_byte_repeat() ->  %% repeat a byte
    construct_sed_byte_muta(fun (B) -> <<B:8, B:8>> end, byte_repeat).

construct_sed_byte_flip() ->  %% flip a bit in a byte
    construct_sed_byte_muta(
        fun (B) -> 
            Flip = owllisp:rand(8),
            Mask = 1 bsl Flip,
            % shared:debug("Flipping byte...", {Flip, Mask, B, B bxor Mask}),
            C = B bxor Mask, 
            <<C:8>> 
        end, byte_flip).

construct_sed_byte_insert() ->  %% insert a byte
    construct_sed_byte_muta(
        fun (B) -> 
            NewByte = owllisp:rand(256),
            <<NewByte:8, B:8>> 
        end, byte_insert).

construct_sed_byte_random() ->  %% swap a byte with a random one
    construct_sed_byte_muta(
        fun (_B) -> 
            NewByte = owllisp:rand(256),
            <<NewByte:8>> 
        end, byte_swap_random).

%%%
%%% Multiple Byte-level Mutations
%%%
%%% Warning: this implememntations are not radamsa-like, only making "similar" things, but in the other way and with slight differences

%% WARNING && TODO: this impementation is DIRTY and not as effective as Radamsa
construct_sed_bytes_muta(F, Name) ->
    fun 
    Self(Rs, [<<>>|BTail], Meta) -> 
        {Rs, BTail, [{Name, -1}|Meta], -1};
    Self(Rs, [BVec|BTail], Meta) ->
        BSize = byte_size(BVec),
        S = owllisp:rand(BSize), 
        L = owllisp:rand_range(1, BSize - S + 1),  %% FIXME: here any (min 2), in radamsa 20: fixme: magic constant, should use something like repeat-len
                                                   %% ^^ check may be max(20, ...) with "MAGIX" could be MORE effective
                                                   %% TODO: make this interval more random...?
        H_bits = S*8,
        P_bits = L*8, 
        <<H:H_bits, P:P_bits, T/binary>> = BVec,      
        C = F(<<H:H_bits>>, <<P:P_bits>>, T, BTail),
        D = rand_delta(Rs),
        {Self, Rs, C, [{Name, D}|Meta], D}
    end.


%% WARNING: in radamsa max permutation block could not exceed length 20, here could be any length
construct_sed_bytes_perm() -> %% permute a few bytes
    construct_sed_bytes_muta(
        fun (H, Bs, T, BTail) -> 
            C = list_to_binary(
            owllisp:random_permutation(
                binary_to_list(Bs))),
            [<<H/binary, C/binary, T/binary>> | BTail]
        end, seq_perm).

construct_sed_bytes_repeat() -> %% repeat a seq
    construct_sed_bytes_muta(
        fun (H, Bs, T, BTail) ->            
            N = max(2, owllisp:rand_log(10)), %% max 2^10 = 1024 stuts
            %% !FIXME: !WARNING: Below is VERY INEFFECTIVE CODE, just working sketch, may be need to optimize?            
            C = list_to_binary([Bs || _ <- lists:seq(1,N)]),
            Res = [<<H/binary, C/binary ,T/binary>> | BTail],
            Res
        end, seq_repeat).

construct_sed_bytes_drop() -> %% drop a seq
    construct_sed_bytes_muta(
        fun (H, _Bs, T, BTail) -> 
            [<<H/binary, T/binary>> | BTail] end, seq_drop).


%%
%% Lines
%%

%% bvec -> (<<... , 10>> .. <<... , 10>>), cut after newlines
lines(Bvec) -> lines(binary_to_list(Bvec), [], []).

lines([], [], Out) -> lists:reverse(Out);
lines([], Buff, Out) -> lists:reverse([lists:reverse(Buff)] ++ Out);
lines([10|T], Buff, Out) -> lines(T, [], [lists:reverse([10 | Buff]) | Out]);
lines([H|T], Buff, Out) -> lines(T, [H | Buff], Out).

%% Lst -> bvec
unlines(Lst) ->
    lists:foldl(fun (X, Acc) -> C = list_to_binary(X), <<Acc/binary, C/binary>> end, <<>>, Lst).

%% #u8[byte ...] → ((byte ... 10) ...) | #false, if this doesn't look like line-based text data
%% TODO: ugly code, need a bit refactor
try_lines(Bvec) ->
    Ls = lines(Bvec),
    IsBin = binarish(Bvec),
    if
        Ls =:= [] -> false;
        IsBin =:= true -> false;
        true -> Ls
    end.

construct_line_muta(Op, Name) ->
    fun Self(Rs, Ll = [H|T], Meta) ->
        Ls = try_lines(H),        
        if
            Ls =:= false ->
                {Self, Rs, Ll, Meta, -1};
            true ->
                MLs = Op(Ls, length(Ls)), % calc length only once
                NH = unlines(MLs),
                {Self, Rs, [NH | T], [{Name, 1}|Meta], 1} 
        end
    end. 

%% state is (n <line> ...)
construct_st_line_muta(Op, Name, InitialState) ->
    fun (Rs, Ll = [H|T], Meta) ->
        Ls = try_lines(H),
        if
            Ls =:= false ->
                {construct_st_line_muta(Op, Name, InitialState), 
                    Rs, Ll, Meta, -1};
            true ->
                {Stp, NewLs} = Op(InitialState, Ls), 
                {construct_st_line_muta(Op, Name, Stp), 
                    Rs, [unlines(NewLs) | T], [{Name, 1} | Meta], 1} 
        end
    end.

%%
%% Shared sequences
%%

%% (a b ...) -> (a+a b ...)
sed_fuse_this(Rs, [H|T], Meta) -> % jump between two shared suffixes in the block
    Lst = binary_to_list(H),    
    B = list_to_binary(fuse:fuse(Lst, Lst)),
    D = rand_delta(Rs),
    {fun sed_fuse_this/3, Rs, [B | T], [{fuse_this, D}|Meta], D}.


%% split list into two halves
%% lst -> a b, a ++ b == lst, length a = length b +/- 1
%% faster than lists:split(length(..)/2, ..), which takes O(n^2)
split(Lst) -> walk(Lst, Lst, []).

%% TODO: ugly, need refactor
walk(T, [], Acc) -> 
    {lists:reverse(Acc), T};
walk(T, [_Head|Tail], Acc) when Tail =:= [] -> 
    {lists:reverse(Acc), T};
walk([A|B], [_Head|Tail], Acc) ->
    walk(B, tl(Tail), [A | Acc]).

sed_fuse_next(Rs, [H|T], Meta) ->
    {Al1, Al2} = split(binary_to_list(H)),
    {B, Ll} = owllisp:uncons(T, H), % next or current
    Bl = binary_to_list(B),
    Abl = fuse:fuse(Al1, Bl),
    Abal = fuse:fuse(Abl, Al2),
    D = rand_delta(Rs),
    {fun sed_fuse_next/3, Rs, 
        shared:flush_bvecs(list_to_binary(Abal), Ll), %  <- on avg 1x, max 2x block sizes
        [{fuse_next, D}|Meta], D}.


remember(Block) ->
    fun (Rs, [H|T], Meta) -> 
        %% TODO: Check -- in radamsa here using owllisp halve instead of split
        {Al1, Al2} = split(binary_to_list(H)),
        {Ol1, Ol2} = split(binary_to_list(Block)),
        A = fuse:fuse(Al1, Ol1), % a -> o
        B = fuse:fuse(Ol2, Al2), % o -> a
        Swap = owllisp:rand(3),
        D = rand_delta(Rs),
        NewBlock = case Swap of
                     0 -> H;
                     _Else -> Block
                 end,
        {remember(NewBlock), Rs, 
            shared:flush_bvecs(list_to_binary(A), % <- on avg 1x, max 2x block sizes
                 shared:flush_bvecs(list_to_binary(B), T)), 
            [{fuse_old, D}|Meta], D}
    end.

 sed_fuse_old(Rs, Ll = [H|_], Meta) ->
     R = remember(H),
     R(Rs, Ll, Meta).


%%
%% ASCII string mutations (use UTF-8 later)
%%

texty(B) when B < 9 -> false;
texty(B) when B > 126 -> false;
texty(B) when B > 31 -> true;
texty(9) -> true;
texty(10) -> true;
texty(13) -> true;
texty(_) -> false.

texty_enough(Lst) -> texty_enough(Lst, ?MIN_TEXTY).

texty_enough([], _) -> true; % match short textual input, accidentally also short trailing ones
texty_enough(_, 0) -> true;
texty_enough([H|T], N) -> 
    case texty(H) of 
        true -> texty_enough(T, N - 1);
        false -> false
    end.

flush_type_node(Type, Bytes, Chunks) ->    
    [[Type | lists:reverse(Bytes)] | Chunks].

%% (byte ..) → (node ...)
%%  node = #(byte bytes...) | #(text bytes) | #(delimited byte (byte ...) byte)
string_lex(Lst) -> string_lex_step(Lst, [], []).

string_lex_step([], [], Chunks) -> 
    lists:reverse(Chunks);
string_lex_step([], Rawr, Chunks) -> 
    lists:reverse(flush_type_node(byte, Rawr, Chunks));
%% WARN: TODO: ugly, need rewrite
string_lex_step(Lst = [H|T], Rawr, Chunks) -> 
    case texty_enough(Lst) of
        true ->
            if 
                Rawr =:= [] -> 
                    step_text(Lst, [], Chunks);
                true -> 
                    step_text(Lst, [], 
                        flush_type_node(byte, Rawr, Chunks))
            end;
        false -> 
            string_lex_step(T, [H | Rawr], Chunks)
    end.

step_text([], Seenr, Chunks) -> 
    lists:reverse(flush_type_node(text, Seenr, Chunks));
step_text([H|T], Seenr, Chunks) when H == 34; H == 39 ->
    %%fun (End) ->
        step_delimited(T, H, H, [], [H | Seenr], Chunks);
    %%end;
step_text(Lst = [H|T], Seenr, Chunks) ->
    case texty(H) of
        true -> step_text(T, [H | Seenr], Chunks);
        %% min length checked at seen
        false -> string_lex_step(Lst, [], flush_type_node(text, Seenr, Chunks))
    end.

%% reading text, seen a delimiter
%% f o o = " 9 0 0 1
%% prevr---' '--- afterr
%%         '-> start = ", end = "
step_delimited([], _Start, _End, AfterR, PrevR, Chunks) ->
    lists:reverse(flush_type_node(text, AfterR ++ PrevR, Chunks));
%% finish text chunk
step_delimited(_Lst = [H|T], Start, End, AfterR, _PrevR = [_PrevrH | PrevrT], Chunks) when H =:= End ->
    Node = [delimited, {Start, lists:reverse(AfterR), End}],
    case PrevrT =:= [] of
        true -> string_lex_step(T, [], [Node | Chunks]);
        false -> string_lex_step(T, [], [Node, [text | lists:reverse(PrevrT)] | Chunks])
    end;
%% skip byte after quotation, if it seems texty    
step_delimited(_Lst = [H|T], Start, End, AfterR, PrevR, Chunks) when H =:= 92, T =:= [] -> %% \
    step_delimited(T, Start, End, [92 | AfterR], PrevR, Chunks);
step_delimited(_Lst = [H|T], Start, End, AfterR, PrevR, Chunks) when H =:= 92 -> %% \
    case texty(hd(T)) of
        true -> step_delimited(tl(T), Start, End, [hd(T), 92 | AfterR], PrevR, Chunks);
        false -> step_delimited(T, Start, End, [H | AfterR], PrevR, Chunks)
    end;
step_delimited(Lst = [H|T], Start, End, AfterR, PrevR, Chunks) ->
    case texty(H) of
        true -> step_delimited(T, Start, End, [H | AfterR], PrevR, Chunks);
        false -> string_lex_step(Lst, [], flush_type_node(text, AfterR ++ PrevR, Chunks))
    end.

string_unlex(Chunks) ->
    lists:foldr(
        fun 
            ([byte|T], Tail) ->
                T ++ Tail;
            ([delimited, {LD, L, RD}], Tail) ->
                [LD | L] ++ [RD | Tail];
            ([text|T], Tail) -> 
                T ++ Tail
        end
        , [], Chunks).

%%% Text mutations

%% check that the nodes do look stringy enough to mutate with these 
%% heuristics, meaning there are some nodes and/or just one, but it's
%% stringy
%% in radamsa this will stop if found byte node; here, we continue
stringy([]) -> false;
stringy([[byte | _] | T]) -> false or stringy(T);
stringy(_) -> true. %% in radamsa -- length(Cs)

silly_strings() -> % added \r \n \t \b 
    ["%n", "%n", "%s", "%d", "%p", "%#x", [0], "aaaa%d%n", [10], [13], [9], [8]]. 

delimeters() ->
    ["'", "\"", "'", "\"", "'", "\"", "&", ":", "|", ";",
     "\\", [10], [13], [9], " ", "`", [0], "]", "[", ">", "<"].
    
random_badness() ->
    random_badness(owllisp:rand(20) + 1, []).

random_badness(0, Out) -> Out;
random_badness(N, Out) ->
    X = owllisp:rand_elem(silly_strings()),
    random_badness(N - 1, X ++ Out).

overwrite([], Old) -> Old;
overwrite([H|T], [_|OldT]) ->
    [H | overwrite(T, OldT)];
overwrite([H|T], Old) ->
    [H | overwrite(T, Old)].

rand_as_count() ->
    Type = owllisp:rand(11),
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
        _Else -> owllisp:rand(1024)        
    end.

push_as(0, Tail)
    -> Tail;
push_as(N, Tail) ->
    push_as(N - 1, [97 | Tail]).

mutate_text_data(Lst, TxtMutators) ->
    mutate_text(owllisp:rand_elem(TxtMutators), Lst).

%% insert badness
mutate_text(insert_badness, []) -> random_badness(); %% empty list -- just insert random
mutate_text(insert_badness, Lst) ->
    P = owllisp:erand(length(Lst)), % in erlang lists starts from 1
    Bad = random_badness(),
    owllisp:led(Lst, P, fun (X) -> [X|Bad]  end); % in radamsa Bad ++ [X], optimized here
%% replace badness
mutate_text(replace_badness, []) -> random_badness(); %% empty list -- just replace with random
mutate_text(replace_badness, Lst) ->
    P = owllisp:erand(length(Lst)), % in erlang lists starts from 1
    Bad = random_badness(),
    lists:sublist(Lst, P - 1) ++ overwrite(lists:nthtail(P, Lst), Bad);
%% insert as
mutate_text(insert_aaas, []) -> push_as(rand_as_count(), []); %% empty list -- just insert random
mutate_text(insert_aaas, Lst) ->
    N = rand_as_count(),
    P = owllisp:erand(length(Lst)), % in erlang lists starts from 1    
    lists:sublist(Lst, P - 1) ++ push_as(N, lists:nthtail(P, Lst));
%% insert null
mutate_text(insert_null, Lst) ->
    Lst ++ [0];
%% insert delimeter
mutate_text(insert_delimeter, []) -> [owllisp:rand_elem(delimeters())]; %% empty list -- just insert random
mutate_text(insert_delimeter, Lst) ->
    P = owllisp:erand(length(Lst)), % in erlang lists starts from 1
    Bad = owllisp:rand_elem(delimeters()),
    owllisp:led(Lst, P, fun (X) -> [X|Bad]  end).


%% Generic ASCII Bad mutation
%% In Radamsa, this function will work only if Cs started as string
%% Else, if Cs contains only byte nodes, it will run infinetely
%% Here, we do maximum L/4 runs in this case
%% TODO: WARN: Ineffective, need rewrite/optimize
string_generic_mutate(Cs, _, L, R) when R > L/4 -> Cs;
string_generic_mutate(Cs, TxtMutators, L, R) ->
    P = owllisp:erand(L), % in erlang, list is beginning from index 1
    El = lists:nth(P, Cs),
    case El of 
        [text|Bs] -> 
            owllisp:led(Cs, P, fun (_X) -> [[text | mutate_text_data(Bs, TxtMutators)]] end); % [Node]
        [byte|_Bs] ->
            string_generic_mutate(Cs, TxtMutators, L, R + 1);
        [delimited, {Left, Bs, Right}] ->
            generic:applynth(P, Cs, fun (_E, Rest) -> [[delimited, {Left, mutate_text_data(Bs, TxtMutators), Right}]] ++ Rest end)
    end. 

construct_ascii_mutator(Fun, Name) ->
    fun Ascii_mutator (Rs, Ll = [H|T], Meta) -> 
        Data = binary_to_list(H),
        Cs = string_lex(Data),

        case stringy(Cs) of % in radamsa stringy_length
            true ->   % do something bad...
                Ms = Fun(Cs),
                D = rand_delta(Rs),        
                BinData = list_to_binary(string_unlex(Ms)),
                {Ascii_mutator, Rs,
                    [BinData | T], 
                    [{Name, D}|Meta], D};
            false ->  % not a string at all (even 1 node), skipping
                {Ascii_mutator, Rs, Ll, Meta, -1} 
        end
    end.


construct_ascii_bad_mutator() ->    
    construct_ascii_mutator(
        fun (Cs) -> string_generic_mutate(Cs, 
                    [insert_badness, replace_badness, insert_aaas, insert_null], 
                    length(Cs), 0)
        end,
        ascii_bad).

%% drop one(both) or zero delimeters
drop_delimeter(0, [delimited, {Left, Bs, _Right}]) -> % drop right
    [text, Left] ++ Bs;
drop_delimeter(1, [delimited, {_Left, Bs, Right}]) -> % drop left
    [text | Bs] ++ [Right];
drop_delimeter(2, [delimited, {_Left, Bs, _Right}]) -> % drop both
    [text | Bs];
drop_delimeter(_, El) -> % drop none
    El.

%% Play with delimeters
string_delimeter_mutate(Cs, L, R) when R > L/4 -> Cs;
string_delimeter_mutate(Cs, L, R) ->
    P = owllisp:erand(L), % in erlang, list is beginning from index 1
    El = lists:nth(P, Cs),
    case El of 
        [text|Bs] -> %% insert or drop special delimeter(s)
            owllisp:led(Cs, P, fun (_X) -> [[text | mutate_text_data(Bs, [insert_delimeter])]] end); % [Node]
        [byte|_Bs] -> %% do nothing
            string_delimeter_mutate(Cs, L, R + 1);
        [delimited, {_Left, _Bs, _Right}] ->
            owllisp:led(Cs, P, fun (_X) -> [drop_delimeter(owllisp:rand(4), El)] end)
    end.

construct_ascii_delimeter_mutator() ->    
    construct_ascii_mutator(
        fun (Cs) -> string_delimeter_mutate(Cs, length(Cs), 0)
        end,
        ascii_delimeter).


%%
%% Guessed Parse-tree Mutations
%%

%% ?? could be settable from command line
usual_delims(40) -> 41;   % ()
usual_delims(91) -> 93;   % []
usual_delims(60) -> 62;   % <>
usual_delims(123) -> 125; % {}
usual_delims(34) -> 34;   % ""
usual_delims(39) -> 39;   % ''
usual_delims(_) -> false.

%% -> lst #false = ran out of data trying to parse up to close, but lst is the same with partial parsing
%% -> lst tail-lst = did successfully parse up to close. ready node is lst, tail is following data
grow([], _Close, Rout) ->  %% out of data, didn't find close. return partial parse.
    {lists:reverse(Rout), false};    
grow([H|T], Close, Rout) when H =:= Close -> %% match complete, return with rest of list
    %%io:write({[H|T], Close, Rout}),
    {lists:reverse([Close | Rout]), T};
grow([H|T], Close, Rout) ->
    %%io:write({[H|T], Close, Rout}),
    %%io:format("~n"),
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

%% count how many list nodes are in a tree structure
count_nodes(Lst) -> count_nodes(Lst, 0).

count_nodes([], N) -> N;
count_nodes([H|T], N) when is_list(H) -> 
    count_nodes(T, count_nodes(H, N+1));
count_nodes([_|T], N) ->
    count_nodes(T, N).

%% lst -> a list of lists (not counting tails) in lst, when walked recursively, not counting lst itself
sublists(Lst) -> sublists(Lst, []).

sublists([], Found) -> Found;
sublists([H|T], Found) when is_list(H) ->
    sublists(T, sublists(H, [H|Found]));
sublists([_H|T], Found) ->
    sublists(T, Found).

pick_sublist(_Rs, Lst) ->
    Subs = sublists(Lst),
    case Subs of
        [] -> false;
        _Else ->
            owllisp:rand_elem(Subs) %% TODO: FIXME: CHECK IF it's correct!
    end.

%% replace the node (sub . tail) with (op (sub . tail))
edit_sublist(Lst = [H|_T], Sub, Op) when H =:= Sub ->
    Op(Lst);
edit_sublist([H|T], Sub, Op) ->
    [edit_sublist(H, Sub, Op) | edit_sublist(T, Sub, Op)];
edit_sublist(Lst, _Sub, _Op) -> Lst.

%% lst (ff of node -> (node -> node)) -> lst' ; <- could also be done with a recursive-mapn
edit_sublists([Hd|T], OpFF) when is_list(Hd) -> 
    MaybeOp = owllisp:get(Hd, false, OpFF),
    case MaybeOp of
        false -> 
            [edit_sublists(Hd, OpFF) | edit_sublists(T, OpFF)];
        _ ->
            [MaybeOp(Hd) | edit_sublists(T, OpFF)]            
    end;
edit_sublists([H|T], OpFF) -> 
    [H | edit_sublists(T, OpFF)];
edit_sublists(Lst, _) -> Lst.

partial_parse(Lst) -> partial_parse(Lst, []).

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

flatten([], Tl) -> Tl;
flatten([H|T], Tl) ->
    flatten(H, flatten(T, Tl));
flatten(Node, Tl) ->
    [Node | Tl].

sed_tree_op(Op, Name) ->
    fun F (Rs, Ll = [H|T], Meta) ->
        case binarish(H) of
            true -> {F, Rs, Ll, Meta, -1};
            false -> 
                NewMeta = [{Name, 1} | Meta],
                Lst = partial_parse(binary_to_list(H)),
                Sub = pick_sublist(Rs, Lst), %% choose partially parsed node to mutate ;; fixme: not checked for F
                NewLst = edit_sublist(Lst, Sub, Op),
                {F, Rs, [list_to_binary(flatten(NewLst, [])) | T], NewMeta, 1}
        end    
    end.

sed_tree_dup() ->
    sed_tree_op(fun (Node = [H|_T]) -> [H|Node] end, tree_dup).

sed_tree_del() ->
    sed_tree_op(fun ([_H|T]) -> T end, tree_del).

%% overwrite one node with one of the others
sed_tree_swap_one(Lst, Subs) ->
    ToSwap = owllisp:reservoir_sample(Subs, 2),
    [A | [B | _]] = owllisp:random_permutation(ToSwap),
    edit_sublist(Lst, A, fun ([_|Tl]) -> [B | Tl] end).

%% pairwise swap of two nodes
%% TODO: here there is a bug (also in original radamsa) that causes to swap child with the parent node, could be feature xD
sed_tree_swap_two(Lst, Subs) ->
    ToSwap = owllisp:reservoir_sample(Subs, 2),
    [A | [B | _]] = ToSwap,
    Mapping = gb_trees:enter(B, fun(_X) -> A end, gb_trees:enter(A, fun (_X) -> B end, gb_trees:empty())),    
    edit_sublists(Lst, Mapping).


construct_sed_tree_swap(Op, Name) ->
    fun F (Rs, Ll = [H|T], Meta) ->
        case binarish(H) of
            true -> {F, Rs, Ll, Meta, -1};
            false -> 
                Lst = partial_parse(binary_to_list(H)),
                Subs = sublists(Lst),
                N = length(Subs),
                case N < 2 of
                    true -> {F, Rs, Ll, Meta, -1};
                    false ->
                        NewLst = Op(Lst, Subs),
                        {F, Rs, [list_to_binary(flatten(NewLst, [])) | T], [{Name, 1} | Meta], 1}
                end
        end
    end.

%% tree stutter

repeat_path(Parent, _Child, N) when N < 2 ->
    Parent; %% <- causes at least 1 extra path to be made, saves one useless replace cycle
repeat_path(Parent, Child, N) ->
    edit_sublist(Parent, Child, 
        fun ([_H|T]) -> [repeat_path(Parent, Child, N-1) | T] end).

choose_child(Node) ->
    Subs = sublists(Node),
    case Subs of 
        [] -> false;
        _Else -> owllisp:rand_elem(Subs)
    end.

choose_stutr_nodes(_Rs, []) -> {false, false}; %% no child nodes
choose_stutr_nodes(Rs, [H|T]) ->
    Childp = choose_child(H),
    case Childp of
        false -> choose_stutr_nodes(Rs, T);
        _Else -> {H, Childp}
    end.

sed_tree_stutter(Rs, Ll = [H|T], Meta) ->
    case binarish(H) of
        true -> {fun sed_tree_stutter/3, Rs, Ll, Meta, -1};
        false -> 
            Lst = partial_parse(binary_to_list(H)), %% (byte|node ...)
            Subs = sublists(Lst),
            RandSubs = owllisp:random_permutation(Subs),
            {Parent, Child} = choose_stutr_nodes(Rs, RandSubs),
            N_reps = owllisp:rand_log(10),
            case Parent of 
                false -> {fun sed_tree_stutter/3, Rs, Ll, Meta, -1};
                _Else ->
                    NewLst = edit_sublist(Lst, Child, 
                            fun ([_H|Tl]) -> [repeat_path(Parent, Child, N_reps)|Tl] end),
                    {fun sed_tree_stutter/3, Rs, 
                        [list_to_binary(flatten(NewLst, [])) | T], 
                        [{tree_stutter, 1} | Meta], 1}
            end
    end.


%%
%% UTF-8
%%

%% grab low 6 bits of a number
ext(N) ->
    (N band 2#111111) bor 2#10000000.
    
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

sed_utf8_widen(Rs, [H|T], Meta) ->    
    P = owllisp:rand(size(H)),
    D = rand_delta(Rs),
    {fun sed_utf8_widen/3, Rs, 
     [edit_byte_vector(H, P, 
        fun (B) when B =:= B band 2#111111 -> N = B bor 2#10000000, <<2#11000000:8, N:8>>;
            (B) -> <<B:8>>
        end)
      | T], [{sed_utf8_widen, D}|Meta], D}.

sed_utf8_insert(Rs, [H|T], Meta) ->    
    P = owllisp:rand(size(H)),
    D = rand_delta(Rs),
    Bin = list_to_binary(owllisp:rand_elem(funny_unicode())),
    {fun sed_utf8_insert/3, Rs, 
     [edit_byte_vector(H, P, 
        fun (B) -> <<B:8, Bin/binary>> end)
      | T], [{sed_utf8_insert, D}|Meta], D}.

%%
%%  Main Mutation Functions
%%

%% limit to [min-score .. max-score]
adjust_priority(Pri, 0) -> Pri;
adjust_priority(Pri, Delta) ->
    max(?MIN_SCORE, min(?MAX_SCORE, Pri + Delta)).

%% [{Score, Priority, _, _}, ...] -> [{Rand(S*P), {...}}, ...] -> sort -> [{...}, ...]
weighted_permutations([]) -> [];
weighted_permutations(Pris) ->
    shared:debug("weighted-permutations", Pris),
    PPris = lists:map(fun (X = {S, P, _, _}) -> {owllisp:rand(S*P), X} end, Pris),
    SPPris = lists:sort(fun ({A,_},{B,_}) -> A>=B end, PPris),
    lists:map(fun ({_, X}) -> X end, SPPris).

%% Mutators have a score they can change themselves (1-100) and a priority given by
%% the user at command line. Activation probability is (score*priority)/SUM(total-scores).

%% (#(score priority mutafn name) ...) -> merged-mutafn :: rs ll meta -> merged-mutafn' rs' ll' meta'
mux_fuzzers(Fs) ->
    shared:debug("calling mux-fuzzer with ", Fs),
    fun L(Rs, [], Meta) -> {mux_fuzzers(Fs), Rs, <<>>, Meta};
        L(Rs, Ll, Meta) when is_list(Ll) ->
            shared:debug("calling mux-fuzzers-loop with ", {Ll, weighted_permutations(Fs), [], Rs, Meta}),
            mux_fuzzers_loop(Ll, weighted_permutations(Fs), [], Rs, Meta) ;
        L(Rs, Ll, Meta) -> L(Rs, Ll(), Meta) %% TODO: strange behaviour
    end.

mux_fuzzers_loop(Ll, [], Out, Rs, Meta) -> {mux_fuzzers(Out), Rs, Ll, Meta};
mux_fuzzers_loop(Ll, [Node|Tail], Out, Rs, Meta) ->
    {Mscore, MPri, Fn, Mname} = Node,
    shared:debug("mux-fuzzers-loop calling mutation: ", Node),
    Res = Fn(Rs, Ll, Meta),
    shared:debug("Res:", Res),
    {MFn, RsN, Mll, MMeta, Delta} = Res, %% in radamsa (mfn rs mll mmeta delta) = Fn(...), that strange, TODO: check it
    shared:debug("mux-fuzzers-loop /calling mutation: ", {MFn, RsN, Mll, MMeta, Delta}),
    NOut = [{adjust_priority(Mscore, Delta), MPri, MFn, Mname} | Out], %% in radamsa mfn instead of fn
    IsMllPair = owllisp:is_pair(Mll),
    IsHEq = (hd(Mll) == hd(Ll)),
    if
        IsMllPair andalso IsHEq -> mux_fuzzers_loop(Ll, Tail, NOut, RsN, MMeta);
        true -> shared:stderr_probe({used, Mname}, {mux_fuzzers(NOut ++ Tail), RsN, Mll, MMeta})
    end.

%% default mutations list
%default_mutations() -> [{10, 1, fun sed_num/3, num}].
%default_mutations() -> [{10, 1, fun sed_utf8_widen/3, uw}].
%default_mutations() -> [{10, 1, fun sed_utf8_insert/3, ui}].
default_mutations() -> [{10, 1, construct_ascii_bad_mutator(), ab}].
%default_mutations() -> [{10, 1, construct_ascii_delimeter_mutator(), ad}].
%default_mutations() -> [{10, 1, sed_tree_dup(), tr2}].
%default_mutations() -> [{10, 1, sed_tree_del(), td}].
%default_mutations() -> [{10, 1, construct_sed_tree_swap(fun sed_tree_swap_one/2, tree_swap_one), ts1}].
%default_mutations() -> [{10, 1, construct_st_line_muta(fun generic:st_list_ins/2, list_ins, [0]), lis}].
%default_mutations() -> [{10, 1, construct_st_line_muta(fun generic:st_list_replace/2, list_replace, [0]), lrs}].
%default_mutations() -> [{10, 1, fun sed_tree_stutter/3, ts}].
%%default_mutations() -> [{10, 1, construct_sed_tree_swap(fun sed_tree_swap_two/2, tree_swap_two), ts2}].
%default_mutations() -> [{10, 1, construct_sed_byte_drop(), bd}, 
%                        {10, 1, construct_sed_byte_inc(), bei}, 
%                        {10, 1, construct_sed_byte_dec(), bed}].
%default_mutations() -> [{10, 1, construct_sed_byte_flip(), bf}].
%default_mutations() -> [{10, 1, construct_sed_byte_insert(), bi}].
%default_mutations() -> [{10, 1, construct_sed_byte_random(), ber}].
%default_mutations() -> [{10, 1, construct_sed_byte_repeat(), br}].
%default_mutations() -> [{10, 1, construct_sed_bytes_perm(), sp}]. %% aka 'bp' in radamsa
%default_mutations() -> [{10, 1, construct_sed_bytes_repeat(), sr}].
%default_mutations() -> [{10, 1, construct_sed_bytes_drop(), sd}].
%default_mutations() -> [{10, 1, construct_line_muta(fun generic:list_del/2, line_del), ld}].
%default_mutations() -> [{10, 1, construct_line_muta(fun generic:list_del_seq/2, line_del_seq), lds}].
%default_mutations() -> [{10, 1, construct_line_muta(fun generic:list_dup/2, line_dup), lr2}].
%default_mutations() -> [{10, 1, construct_line_muta(fun generic:list_clone/2, line_clone), lri}].
%default_mutations() -> [{10, 1, construct_line_muta(fun generic:list_repeat/2, line_repeat), lr}].
%default_mutations() -> [{10, 1, construct_line_muta(fun generic:list_swap/2, line_swap), ls}].
%default_mutations() -> [{10, 1, construct_line_muta(fun generic:list_perm/2, line_perm), lp}].
%default_mutations() -> [{10, 1, fun sed_fuse_this/3, ft}].
%default_mutations() -> [{10, 1, fun sed_fuse_next/3, fn}].
%default_mutations() -> [{10, 1, fun sed_fuse_old/3, fo}].

%% randomize mutator scores
mutators_mutator(Rs, Mutas) ->
    mutators_mutator(Rs, Mutas, []).

mutators_mutator(Rs, [], Out) ->
    {Rs, mux_fuzzers(Out)};
mutators_mutator(Rs, [{_, Pri, F, Name}|T], Out) ->
    N = owllisp:rand(?MAX_SCORE),
    mutators_mutator(Rs, T, [{max(2, N), Pri, F, Name} | Out]).


