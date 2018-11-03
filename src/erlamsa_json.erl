% Copyright (c) 2014-2018 Alexander Bolshev aka dark_k3y
% Initial implementation was a little slow, so:
% - several optimizations approaches is heavilly based on JSONE implementation
% by Copyright (c) 2013-2016, Takeru Ohta <phjgt308@gmail.com> (MIT LICENSE)
% see https://github.com/sile/jsone/blob/master/src/jsone_decode.erl
% for more details
% Usage of original JSONE was not possible due to the fact, that
% erlamsa should be able to parse badly crafted JSON docs
%
% LICENSE
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

-module(erlamsa_json).

-author("dark_k3y").

-compile([export_all]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

-include("erlamsa.hrl").

%% API
-export([tokenize/1, tokens_to_erlang/1]).

-define(NOT_SEPARATOR(C),
        C =/= $ , C =/= $\n, C =/= $\r, C =/= $\t, C =/= $,, C =/= $], C =/= $}, C =/= $:).

%% TODO: add specs

%%%
%%% Generic functions
%%%

fold_list(F, [H], Acc) ->
    R = F(H),
    fold_list(F, [], [R | Acc]);
fold_list(F, [H|T], Acc) ->
    R = F(H),
    fold_list(F, T, [[R, $,] | Acc]);
fold_list(_F, [], Acc) ->
    lists:reverse(Acc).

%%%
%%% /Generic functions
%%%

%%%
%%% JSON tokenizer
%%%

% Grammar derived from https://tools.ietf.org/html/rfc7159
% document: ws value ws
% ws : \x20 | \t | \r | \n
% value: string | number | object | array | true | false | null
% elements: value | value ws , ws elements
% array: ws [ ws ] | ws [ ws elements ws ]
% object: ws { ws }  | ws { ws members ws }
% members: pair | pair ws , ws members
% pair: string : value

%% document is a ws
tokenize(Bin) ->
    lists:reverse(ws(Bin, [value], [])).

ws(<<$\t, Rest/binary>>, Context, Acc) -> ws(Rest, Context, Acc);
ws(<<$\n, Rest/binary>>, Context, Acc) -> ws(Rest, Context, Acc);
ws(<<$\r, Rest/binary>>, Context, Acc) -> ws(Rest, Context, Acc);
ws(<<$  , Rest/binary>>, Context, Acc) -> ws(Rest, Context, Acc);
ws(<<>>, _, Acc) -> Acc;
ws(Bin, Context = [Term|RestContext], Acc) ->
    %io:format("111111 ~p~n", [Bin]),
    case Term of
        array ->                array(Bin, [array_end|RestContext], Acc);
        {elements, List} ->     elements(Bin, RestContext, List, Acc);
        object ->               object(Bin, [object_end|RestContext], Acc);
        {members, Pairs} ->     members(Bin, RestContext, Pairs, Acc);
        pair ->                 pair(Bin, RestContext, Acc);
        pair_delim ->           pair(Bin, Context, Acc);
        value ->                value(Bin, RestContext, Acc)
    end;
ws(_, _, _) -> throw(incorrect_json).

value(<<$[, Rest/binary>>, Context, Acc) ->
    ws(Rest, [array|Context], Acc);
value(<<${, Rest/binary>>, Context, Acc) ->
    ws(Rest, [object|Context], Acc);
value(<<"true", Rest/binary>>, Context, Acc) ->
    push(Rest, Context, {constant, true}, Acc);
value(<<"false", Rest/binary>>, Context, Acc) ->
    push(Rest, Context, {constant, false}, Acc);
value(<<"null", Rest/binary>>, Context, Acc) ->
    push(Rest, Context, {constant, null}, Acc);
value(<<$", Rest/binary>>, Context, Acc) ->
    string(Rest, Context, [], Acc);
value(Bin, Context, Acc) ->
    number(Bin, Context, Acc).

%% Arrays
array(<<$], Rest/binary>>, [array_end|Context],  Acc) ->
    %io:format("0]~n"),
    push(Rest, Context, {array, []}, Acc);
array(Bin, Context, Acc) ->
    ws(Bin, [value, {elements, []}|Context], Acc).

elements(<<$], Rest/binary>>, [array_end|Context], List, Acc) ->
    %io:format("1]~n"),
    push(Rest, Context, {array, lists:reverse(List)}, Acc);
elements(<<$,, Rest/binary>>, Context, List, Acc) ->
    ws(Rest, [value, {elements, List}|Context], Acc);
elements(<<_C:8, _Rest/binary>>, _Context, _List, _Acc) ->
    throw(incorrect_json).

%% Objects
object(<<$}, Rest/binary>>, [object_end|Context],  Acc) ->
    %io:format("mmmmmm~n"),
    push(Rest, Context, {object, []}, Acc);
object(Bin, Context, Acc) ->
    ws(Bin, [pair, {members, []}|Context], Acc).

members(<<$}, Rest/binary>>, [object_end|Context], Pairs, Acc) ->
    %io:format("nnnnnnnnn ~p~n", [{Rest, [value|Context], [{object, lists:reverse(Pairs)}|Acc]}]),
    push(Rest, Context, {object, lists:reverse(Pairs)}, Acc);
members(<<$,, Rest/binary>>, Context, Pairs, Acc) ->
    ws(Rest, [pair, {members, Pairs}|Context], Acc);
members(<<_C:8, _Rest/binary>>, _Context, _Pairs, _Acc) ->
    throw(incorrect_json).

pair(<<$:, Rest/binary>>, [pair_delim | Context], Acc) ->
    %io:format("1: ~p~n", [{Rest, Context, Acc}]),
    ws(Rest, [value, pair_end | Context], Acc);
pair(Bin, Context, Acc) ->
    %io:format("2: ~p~n", [{Bin, Context, Acc}]),
    ws(Bin, [value, pair_delim | Context], Acc).

%% Primitive values parsing
push(Bin, [], Value, Acc) ->
    ws(Bin, [], [Value|Acc]);
push(Bin, [{elements, List} | Context], Value, Acc) ->
    %io:format("!!!!~p, ~p, ~p~n", [Bin, [{elements, [Value|List]} | Context], Acc]),
    ws(Bin, [{elements, [Value|List]} | Context], Acc);
push(Bin, [{members, List} | Context], Value, Acc) ->
    %io:format("!!!!~p, ~p, ~p~n", [Bin, [{members, [Value|List]} | Context], Acc]),
    ws(Bin, [{members, [Value|List]} | Context], Acc);
push(Bin, [pair_delim | Context], Key, Acc) ->
    ws(Bin, [pair_delim, {pair_start, Key} | Context], Acc);
push(Bin, [pair_end, {pair_start, Key} | Context], Value, Acc) ->
    push(Bin, Context, {pair, Key, Value}, Acc);
push(_, _, _, _) -> throw(incorrect_json).


%% TODO: more effective way to handle big strings
%% this may be slow in some cases
string(<<$", Rest/binary>>, Context, RevStr, Acc) ->
    push(Rest, Context, {string, lists:reverse(RevStr)}, Acc);
string(<<C:8, Rest/binary>>, Context, RevStr, Acc) ->
    string(Rest, Context, [C|RevStr], Acc);
string(<<>>, Context, RevStr, Acc) ->
    push(<<>>, Context, {junkstring, lists:reverse([$"|RevStr])}, Acc).

number(<<C:8, Rest/binary>>, Context, Acc) when ?NOT_SEPARATOR(C) ->
    number_rest(Rest, Context, [C], Acc);
number(_, _, _) -> throw(incorrect_json).

number_rest(<<C:8, Rest/binary>>, Context, N, Acc) when ?NOT_SEPARATOR(C) ->
    number_rest(Rest, Context, [C|N], Acc);
number_rest(Bin, Context, N, Acc) ->
    push(Bin, Context, {number, lists:reverse(N)}, Acc).

%%%
%%% /JSON tokenizer
%%%

%%%
%%% JSON AST -> Erlang dictionaries
%%%

tokens_to_erlang(Ast) when is_list(Ast) ->
    lists:map(fun tokens_to_erlang/1, Ast);
tokens_to_erlang({object, Lst}) ->
    Pairs = lists:map(fun tokens_to_erlang/1, Lst),
    lists:foldl(fun ({Key, Value}, Acc) -> maps:put(Key, Value, Acc) end,
                maps:new(), Pairs);
tokens_to_erlang({array, Lst}) ->
    lists:map(fun tokens_to_erlang/1, Lst);
tokens_to_erlang({pair, Key, Value}) ->
    {tokens_to_erlang(Key), tokens_to_erlang(Value)};
tokens_to_erlang({string, Value}) ->
    Value;
%% TODO: nested try/catch, fix me:
tokens_to_erlang({number, Value}) ->
    try list_to_integer(Value) of
        Int -> Int
    catch error:badarg ->
        try list_to_float(Value) of
            Float -> Float
        catch error:badarg ->
            invalid_number
        end
    end;
tokens_to_erlang(Token) ->
    Token.

%%%
%%% /JSON AST -> Erlang dictionaries
%%%

%%%
%%% JSON AST Folder
%%%

%% TODO: beautify parameter to insert ' ' and '\n'

%% handle incorrect AST that could appear during mutations
fold_ast_noarray(H, Acc) when is_list(H), length(H) > 1 ->
    M = fold_list(fun (A) -> fold_ast(A, []) end, H, []),
    fold_ast([], [M | Acc]);
fold_ast_noarray(H, Acc) ->
    fold_ast(H, Acc).

fold_ast([H], Acc) ->
    fold_ast(H, Acc);
fold_ast({pair, P1, P2}, Acc) -> 
    P1Json = fold_ast(P1, []),
    P2Json = fold_ast(P2, []),
    [P1Json, ":", P2Json | Acc];
fold_ast({string, Value}, Acc) -> 
    [$", Value, $" | Acc];
fold_ast({number, Value}, Acc) ->
    [Value | Acc];
fold_ast({object, Els}, Acc) -> 
    ["{", fold_ast_noarray(Els, []), "}" | Acc];
fold_ast({array, Els}, Acc) -> 
    ["[", fold_ast_noarray(Els, []), "]" | Acc];
fold_ast([], Acc) -> 
    lists:flatten(Acc);
%% handle incorrect AST that could appear during mutations
fold_ast(H, Acc) when is_list(H), length(H) > 1 ->
    M = fold_list(fun (A) -> fold_ast(A, []) end, H, []),
    fold_ast([], ["[", M, "]" | Acc]).
fold_ast(Ast) ->
    list_to_binary(fold_ast(Ast, [])).

%%%
%%% /JSON AST Folder
%%%

%%%
%%% JSON AST utils
%%%

walk_reverse(L) when is_list(L) -> lists:reverse(L);
walk_reverse(L) -> L.

walk_uncons1(L) when is_list(L), length(L) =:= 1 -> hd(L);
walk_uncons1(L) -> L.

walk(Ast, Fun, InitAcc) ->
    {Res, ResT, ResN} = lists:foldl(
        fun
            Walker({Type, Els}, {Acc, CountT, Count}) when Type =:= object; Type =:= array ->
                %io:format("Object or Array: ~p : ~p~n", [{Type, Els}, {Acc, Count}]),
                {ChildAcc, ChildTagCnt, ChildCnt} = Walker(Els, {InitAcc, CountT + 1, Count + 1}),
                {Fun({Type, walk_reverse(ChildAcc)}, Acc, CountT + 1, Count + 1), ChildTagCnt, ChildCnt};
            Walker({pair, El1, El2}, {Acc, CountT, Count}) ->
                %io:format("Pair: ~p : ~p~n", [{pair, El1, El2}, {Acc, Count}]),
                {ChildAcc1, ChildTagCnt1, ChildCnt1} = Walker(El1, {InitAcc, CountT, Count + 1}),
                {ChildAcc2, ChildTagCnt2, ChildCnt2} = Walker(El2, {InitAcc, ChildTagCnt1, ChildCnt1}),
                {Fun({pair, walk_uncons1(walk_reverse(ChildAcc1)), 
                            walk_uncons1(walk_reverse(ChildAcc2))}, Acc, CountT, Count + 1), ChildTagCnt2, ChildCnt2};
            Walker(El, {Acc, CountT, Count}) when is_list(El) ->
                %io:format("List: ~p : ~p~n", [El, {Acc, Count}]),
                lists:foldl(fun(A, B) -> Walker(A, B) end, {Acc, CountT, Count}, El);
            Walker(Elem, {Acc, CountT, Count}) ->
                %io:format("Single elem: ~p : ~p~n", [Elem, {Acc, Count}]),
                {Fun(Elem, Acc, CountT, Count + 1), CountT, Count + 1}
        end,
        {InitAcc, 0, 0}, Ast),
    {walk_reverse(Res), ResT, ResN}.

select(Ast, Fun) ->
    lists:foldl(
        fun
            Walker(El = {Type, Els}, {Acc, CountT, Count}) when Type =:= object; Type =:= array ->
                case Fun(El, CountT + 1, Count + 1) of
                    false -> 
                        Walker(Els, {Acc, CountT + 1, Count + 1});
                    Res -> 
                        {Res, CountT + 1, Count + 1}
                end;
            Walker(El = {pair, El1, El2}, {Acc, CountT, Count}) ->
                case Fun(El, CountT, Count + 1) of
                    false ->
                        case Walker(El1, {Acc, CountT, Count + 1}) of
                            {false, ChildTagCnt1, ChildCnt1} ->
                                case Walker(El2, {Acc, ChildTagCnt1, ChildCnt1}) of
                                    {false, ChildTagCnt2, ChildCnt2} -> 
                                        {Acc, ChildTagCnt2, ChildCnt2};
                                    Res2 -> Res2
                                end;
                            Res1 -> Res1
                        end;
                    Res -> {Res, CountT, Count + 1}
                end;
            Walker(El, {Acc, CountT, Count}) when is_list(El) ->
                lists:foldl(fun(A, B) -> Walker(A, B) end, {Acc, CountT, Count}, El);
            Walker(Elem, {Acc, CountT, Count}) ->
                case Fun(Elem, CountT, Count + 1) of 
                    false -> {Acc, CountT, Count + 1};
                    Res -> {Res, CountT, Count + 1}
                end
        end,
        {false, 0, 0}, Ast).

test_walk(Ast) ->
    walk(Ast,
        fun  (E, Acc, _, _N) ->
            %io:format("~p is ~w~n", [E, N]),
            [E|Acc] 
        end,
        []).

count(Ast) ->
    walk(Ast,
        fun
            ({Type, Els}, Acc, _, _N) when Type =:= object; Type =:= array ->
                Els + Acc + 1;
            ({pair, El1, El2}, Acc, _, _N) ->
                El1 + El2 + Acc + 1;
            (_E, Acc, _, _N) ->
                Acc + 1
        end, 0).

select_elem(Ast, N) ->
    {Res, _, _} = select(Ast,
            fun
                (Elem, _, I) when I =:= N ->
                    {Elem, I};
                (_Elem, _, _I) ->
                    false
            end),
    Res.

select_tag(Ast, N) ->
    {Res, _, _} = select(Ast,
            fun
                ({object, _} = Elem, I, C) when I =:= N ->
                    {Elem, I, C};                
                ({array, _} = Elem, I, C) when I =:= N ->
                    {Elem, I, C};                
                (_, _, _) ->
                    false
            end),
    Res.

replace_elem(Ast, R, El) ->
    walk(Ast,
        fun
            (_Elem, Tree, _, I) when I == R ->
                [El | Tree];
            (Elem, Tree, _, _I) ->
                [Elem | Tree]
        end, []).

repeat_listhd(L, N) when N < 1 ->
    L;
repeat_listhd([H | T], N) ->
    repeat_listhd([H, H | T], N - 1).

repeat_elem(Ast, R, Times) ->
    walk(Ast,
        fun
            (Elem, Tree, _, I) when I == R ->
                repeat_listhd([Elem | Tree], Times);
            (Elem, Tree, _, _I) ->
                [Elem | Tree]
        end, []).

insert_elem(Ast, R, NewElem) ->
    walk(Ast,
        fun
            (Elem, Tree, _, I) when I == R ->
                [NewElem, Elem | Tree];
            (Elem, Tree, _, _I) ->
                [Elem | Tree]
        end, []).

%%%
%%% /JSON AST utils
%%%

%%%
%%% JSON AST -> Erlang simple structs
%%%

tokens_to_simpleast(Ast) when is_list(Ast) ->
    lists:map(fun tokens_to_simpleast/1, Ast);
tokens_to_simpleast({object, Lst}) ->
    {lists:map(fun tokens_to_simpleast/1, Lst)};
tokens_to_simpleast({array, Lst}) ->
    lists:map(fun tokens_to_simpleast/1, Lst);
tokens_to_simpleast({pair, Key, Value}) ->
    {tokens_to_simpleast(Key), tokens_to_simpleast(Value)};
tokens_to_simpleast({string, Value}) ->
    {string, Value};
tokens_to_simpleast({number, Value}) ->
    try list_to_integer(Value) of
        Int -> Int
    catch error:badarg ->
        try list_to_float(Value) of
            Float -> Float
        catch error:badarg ->
            invalid_number
        end
    end;
tokens_to_simpleast(Token) ->
    Token.

%%%
%%% /JSON AST -> Erlang simple structs
%%%


%%%
%%% JSON Simple AST Folder
%%%

fold_simpleast(H, Acc) when is_list(H) ->
    lists:flatten([$[, fold_list(fun (A) -> fold_simpleast(A, []) end, H, []), $] | Acc]);
fold_simpleast({H}, Acc) when is_list(H) ->
    lists:flatten([${, fold_list(fun (A) -> fold_simpleast(A, []) end, H, []), $} | Acc]);
fold_simpleast({string, A}, Acc) ->
    lists:flatten([$", A, $" | Acc]);
fold_simpleast({A, B}, Acc) ->
    lists:flatten([fold_simpleast(A, Acc), $:, fold_simpleast(B, Acc) | Acc]);
fold_simpleast(A, Acc) when is_integer(A);is_float(A) ->
    [lists:flatten(io_lib:format("~p", [A])) | Acc];
fold_simpleast([], Acc) ->
    lists:flatten(Acc);
fold_simpleast(A, Acc) ->
    [A | Acc].
%%%
%%% /JSON Simple AST Folder
%%%

%%%
%%% Mutators
%%%

%% TODO: sometimes it creates object: values pairs, FIXME: fix it

pump_path(Start, _End, 0) -> Start;
pump_path(Start, End, N) ->
    %io:format("Pumping... iter ~w start ~w I =  ~w~n", [N, Start, End]),
    {PumpedTag, _, _} = 
        walk([Start],
            fun
                (_Elem, Tree, _, I) when I =:= End ->
                    [Start | Tree];
                (Elem, Tree, _, _) ->
                    [Elem | Tree]
            end, []),
    %io:format("Pumping... iter ~w res ~w~n", [N, PumpedTag]),
    pump_path(hd(PumpedTag), End*2 - 1, N - 1).


json_pump(Ast, 0) -> {Ast, 0, 0};
json_pump(Ast, T) ->
    %io:format("Cnt: ~w ~w~n", [Ast, N]),
    R = erlamsa_rnd:erand(T),
    %io:format("R:!!!!!!!!!!!!!!!!!! ~w~n", [R]),
    {Start, R, StartPlace} = select_tag(Ast, R),
    %io:format("Start:!!!!!!!!!!!!!!!!!! ~w ~p ~p~n", [Res, T, R]),
    {_, _, SubElems} = count([Start]),
    %io:format("SubElems:!!!!!!!!!!!!!!!!!! ~w~n", [SubElems]),
    E = erlamsa_rnd:erand(SubElems - 1) + 1, %% not the tag itself
    %io:format("E:!!!!!!!!!!!!!!!!!! ~w~n", [E]),
    %{End, _, _} = select_elem([Start], E),
    %io:format("End:!!!!!!!!!!!!!!!!!! ~w~n", [End]),
    PumpCnt = 2, %erlamsa_rnd:erand( trunc(1000.0/(100.0 + SubElems)) ),
    % ^-- limiting the depth of the pump
    %io:format("PumpCnt:!!!!!!!!!!!!!!!!!! ~w~n", [PumpCnt]),
    Pumped = pump_path(Start, E, PumpCnt),
    %io:format("PUMPED!!!!!!!!!! ~w~n", [Pumped]),
    replace_elem(Ast, StartPlace, Pumped).

json_dup(Ast, N) ->
    R = erlamsa_rnd:erand(N),
    repeat_elem(Ast, R, 1).

json_repeat(Ast, N) ->
    R = erlamsa_rnd:erand(N),
    repeat_elem(Ast, R, erlamsa_rnd:erand(100)).

json_swap(Ast, N) ->
    R1 = erlamsa_rnd:erand(N),
    R2 = erlamsa_rnd:erand(N),
    {Elem1, R1} = select_elem(Ast, R1),
    {Elem2, R2} = select_elem(Ast, R2),
    walk(Ast,
        fun
            (_Elem, Tree, _, I) when I == R1 ->
                [Elem2 | Tree];
            (_Elem, Tree, _, I) when I == R2 ->
                [Elem1 | Tree];
            (Elem, Tree, _, _I) ->
                [Elem | Tree]
        end, []).

json_insert(Ast, N) ->
    R1 = erlamsa_rnd:erand(N),
    R2 = erlamsa_rnd:erand(N),
    {NewElem, R1} = select_elem(Ast, R1),
    insert_elem(Ast, R2, NewElem).

%%%
%%% /Mutators
%%%

%%%
%%% Mutations
%%%
json_mutation(Ast, {N, NT}) ->
    json_mutation(Ast, {N, NT}, erlamsa_rnd:rand(5)).

json_mutation(Ast, {N, _NT}, 0) ->
    {Res, _, _} = json_swap(Ast, N),
    {[{json_swap, 1}], Res, 1};
json_mutation(Ast, {N, _NT}, 1) ->
    {Res, _, _} = json_dup(Ast, N),
    {[{sgml_dup, 1}], Res, 1};
json_mutation(Ast, {_N, NT}, 2) ->
    {Res, _, _} = json_pump(Ast, NT),
    {[{json_pump, 1}], Res, -2}; %% don't allow too much pumps...
json_mutation(Ast, {N, _NT}, 3) ->
    {Res, _, _} = json_repeat(Ast, N),
    {[{json_repeat, 1}], Res, 1};
json_mutation(Ast, {N, _NT}, 4) ->
    {Res, _, _} = json_insert(Ast, N),
    {[{sgml_insert, 1}], Res, 1}.

json_mutate(Ll = [H|T], Meta) ->
    %io:format("Trying to parse... ~p~n", [size(H)]),
    %file:write_file("./last_json.txt", H),
    %% FIXME: count tags while tokenizing?
    try tokenize(H) of
        Tokens ->
            {N, NT, N} = count(Tokens),
            %%io:format("Ast ready ~p~n", [Tokens]),
            {NewMeta, Res, D} = json_mutation(Tokens, {N, NT}),
            NewBinStr = fold_ast(Res),
            if
                NewBinStr =:= H ->
                    {fun json_mutate/2, Ll, NewMeta, -1};
                true ->
                    {fun json_mutate/2, [NewBinStr | T],  [NewMeta|Meta], 
                    D + trunc(size(NewBinStr)/(?AVG_BLOCK_SIZE*10))} %% limiting next rounds based on a size
            end
    catch
        incorrect_json ->
            {fun json_mutate/2, Ll, Meta, -1}
    end.