-module(erlamsa_gf).           
-export([static/1, rbyte/1, rword/1, rdword/1, rddword/1, pick/1, rbinary/2, rbinary/3,
        pick_pref/3, sizer/4, block/2, block/1, range/3, loop/4,
        fuzz/3, rbits/2, session_get_bits/3, session_get/2]).       

inc_ms(Rnd, Prob) -> io:format("Ms+1: ~w =< ~w~n", [Rnd, Prob]).

try_mutate(Value, Func) ->
    fun(_Mode, Prob, Session) ->
        Rnd = erlamsa_rnd:rand_float(),
        if 
            Rnd =< Prob ->
                inc_ms(Rnd, Prob),
                {{1, 1, Session}, Func(Value)};
            true ->
                {{1, 0, Session}, Value}
        end
    end.

flatten_list_of_bits([H|L], Acc) -> 
    flatten_list_of_bits(L, <<H/bitstring, Acc/bitstring>>);    
flatten_list_of_bits([], Acc) -> 
    Acc.

flatten_list_of_bits(L) -> flatten_list_of_bits(L, <<>>).    


flatten({Mode, BaseProb, Session}, F, {AccC, AccM, AccB}) when is_function(F, 0) -> 
    flatten({Mode, BaseProb, Session}, F(), {AccC, AccM, AccB}); 
flatten({Mode, BaseProb, Session}, [H|L], {AccC, AccM, AccB}) ->
    {C, M, B} = flatten({Mode, BaseProb, Session}, H, {AccC, AccM, AccB}),
    flatten({Mode, BaseProb/(M + 1), Session}, L, {C, M, B});   
flatten({Mode, BaseProb, Session}, F, {AccC, AccM, AccB}) when is_function(F, 3) -> 
    {{C, M, _Session}, B} = F(Mode, BaseProb, Session),
    flatten({Mode, BaseProb/(AccM + M + 1), Session}, B, {AccC + C, AccM + M, AccB}); 
flatten({_Mode, _BaseProb, _Session}, {{C, M, _Session}, B}, {AccC, AccM, AccB}) when not is_function(B) -> 
    {AccC + C, AccM + M, [B|AccB]};
flatten(_Conf, B, {AccC, AccM, AccB}) when is_binary(B); is_bitstring(B) -> 
    {AccC, AccM, [B|AccB]};
flatten(_Conf, [], Acc) -> Acc;
flatten(_Conf, A, Acc) -> io:format("Unknown: ~w ~w ~w~n", [_Conf, A, Acc]).

flatten(Mode, BaseProb, Session, Grammar) ->
    {Cnt, Ms, L} = flatten({Mode, BaseProb, Session}, Grammar, {0, 0, [<<>>]}),
    %io:format("!!!~w~n", [{Cnt, Ms, L}]),
    {Cnt, Ms, flatten_list_of_bits(L)}.


static(Static) -> 
    fun(_Mode, _Prob, Session) ->
        {{1, 0, Session}, Static}
    end.

range(L, R, BitSize) -> 
    fun(_Mode, _Prob, Session) ->
        El = erlamsa_rnd:rand_range(L, R+1),
        {{1, 0, Session}, <<El:BitSize>>}
    end.

rbits(Bitstring, Bits) -> 
    try_mutate(<<Bitstring:Bits/bitstring>>, 
        fun (_V) -> erlamsa_rnd:random_bitstring(Bits) end).

rbyte(Byte) -> 
    try_mutate(Byte, fun (_V) -> erlamsa_rnd:random_block(1) end).

rword(Word) -> 
    try_mutate(Word, fun (_V) -> erlamsa_rnd:random_block(2) end).

rdword(DWord) -> 
    try_mutate(DWord, fun (_V) -> erlamsa_rnd:random_block(4) end).

rddword(DDWord) -> 
    try_mutate(DDWord, fun (_V) -> erlamsa_rnd:random_block(8) end).

rbinary(L, R) ->
    fun(_Mode, _Prob, Session) ->
        {{1, 0, Session}, erlamsa_rnd:random_block(erlamsa_rnd:rand_range(L, R))}
    end.

rbinary(Binary, L, R) ->
    try_mutate(Binary, fun (_V) -> 
        erlamsa_rnd:random_block(erlamsa_rnd:rand_range(L, R)) 
    end).

pick(List) -> 
    fun(_Mode, _Prob, Session) ->
        %io:format("Block1~n"),
        {{1, 0, Session}, erlamsa_rnd:rand_elem(List)}
    end.

session_get(Key, Default) ->
    fun(_Mode, _Prob, Session) ->
        Elem = maps:get(Key, Session, Default),
        {{1, 0, Session}, Elem}
    end.

session_get_bits(Key, Bits, Default) ->
    fun(_Mode, _Prob, Session) ->
        Elem = maps:get(Key, Session, Default),
        {{1, 0, Session}, <<Elem:Bits/bitstring>>}
    end.

pick_pref(Item, Prob, List) -> 
    fun(_Mode, _Prob, Session) ->
        %io:format("PickPref~n"),
        Rnd = erlamsa_rnd:rand_float(),
        if 
            Rnd > Prob -> 
                {{1, 0, Session}, Item};
            true -> 
                {{1, 0, Session}, erlamsa_rnd:rand_elem(List)}
        end
    end.

%% TODO: type
loop(Settings, Prefix, Element, Postfix) when not is_list(Postfix) ->
    loop(Settings, Prefix, Element, [Postfix]);
loop({Min, Max, Size, FuzzProb, _Type = count}, Prefix, Element, Postfix) ->    
    Elements_Count = erlamsa_rnd:rand_range(Min, Max),
    Elements = lists:map(fun(_V) -> Element end, lists:seq(1, Elements_Count)),
    [fun (_, Prob, Session) ->
        %io:format("Loop~n"),
        Rnd = erlamsa_rnd:rand_float(), 
        if 
            Rnd > Prob*FuzzProb -> 
                {{1, 0, Session}, <<Elements_Count:Size>>};  
            true ->
                inc_ms(Rnd, Prob*FuzzProb),
                New_Elements_Count = erlamsa_rnd:random_bitstring(Size),
                {{1, 1, Session}, New_Elements_Count } 
        end
     end,
     Prefix, Elements | Postfix].
   
%% TODO: Endiannes
%% TODO: "Smart" Length mutations 
sizer({Size, _Endiannes, FuzzProb}, Prefix, Sizer, Postfix) ->
    fun(Mode, Prob, Session) ->
        %io:format("Sizer~n"),
        Rnd = erlamsa_rnd:rand_float(),
        {PrefixC, PrefixMs, PrefixBin} = flatten(Mode, Prob, Session, Prefix),
        {MiddleC, MiddleMs, MiddleBin} = flatten(Mode, Prob/(1 + PrefixMs), Session, Sizer),
        MiddleSize = size(MiddleBin),
        {Length, IsM} = if 
                            Rnd > Prob*FuzzProb/(1 + PrefixMs)/(MiddleMs+1) -> 
                                {<<MiddleSize:Size>>, 0};
                            true ->
                                inc_ms(Rnd, FuzzProb*Prob/(1 + PrefixMs)/(MiddleMs+1)),
                                {erlamsa_rnd:random_bitstring(Size), 1}
                        end,
        %io:format("~w ~w ~w ~w~n", [Prob, PrefixMs, MiddleMs, 1 + IsM]),                        
        {PostfixC, PostfixMs, PostfixBin} = flatten(Mode, Prob/(1+PrefixMs)/(1+MiddleMs)/(1 + IsM), Session, Postfix),
        {{PrefixC + MiddleC + PostfixC + 1, IsM + PrefixMs + MiddleMs + PostfixMs, Session},
            <<Length/bitstring, PrefixBin/bitstring, MiddleBin/bitstring, PostfixBin/bitstring>>}
    end.

block({UserProb}, Block) -> 
    fun (count, _, Session) ->
            {{1, 0, Session}, Block};
        (_Mode, _, Session) ->
            Rnd = erlamsa_rnd:rand_float(),
            if 
                Rnd > UserProb ->
                    {{1, 0, Session}, <<>>};
                true ->
                    {{1, 0, Session}, Block}
            end
    end.

block(Block) -> block({1.0}, Block).

fuzz(Session, BaseProb, Header) ->  
    io:format("Calculating average depth...~n"),
    {C1, _, _} = flatten(count, BaseProb, Session, Header),
    {C2, _, _} = flatten(count, BaseProb, Session, Header),
    AvgDepth = (C1+C2)/2,
    io:format("Depth is: ~w, now Fuzzing...~n", [AvgDepth]),
    {_C, _M, Bin} = flatten(fuzz, BaseProb*erlamsa_rnd:rand_range(2, round(AvgDepth))/2, Session, Header),
    io:format("~w ~p~n", [[_C,_M], Bin]), 
    <<Bin/binary>>.
    
             

