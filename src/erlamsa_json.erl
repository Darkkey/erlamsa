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
    ws(Rest, [value, {elements, List}|Context], Acc).

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
    ws(Rest, [pair, {members, Pairs}|Context], Acc).

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
    push(Bin, Context, {pair, Key, Value}, Acc).


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

fold_list(F, [H], Acc) ->
    R = F(H),
    fold_list(F, [], [R | Acc]);
fold_list(F, [H|T], Acc) ->
    R = F(H),
    fold_list(F, T, [[R, $,] | Acc]);
fold_list(_F, [], Acc) ->
    lists:reverse(Acc).

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
    ["{", fold_ast(Els, []), "}" | Acc];
fold_ast([], Acc) -> 
    lists:flatten(Acc);
fold_ast(H, Acc) when is_list(H) ->
    M = fold_list(fun (A) -> fold_ast(A, []) end, H, []),
    fold_ast([], [M | Acc]).

fold_ast(Ast) ->
    list_to_binary(fold_ast(Ast, [])).

%%%
%%% /JSON AST Folder
%%%

%%%
%%% Mutations
%%%

json_mutate(Ll = [H|T], Meta) ->
    %io:format("Trying to parse... ~p~n", [size(H)]),
    %file:write_file("./last_json.txt", H),
    try tokenize(H) of
        Tokens ->
            %%io:format("Ast ready ~p~n", [Tokens]),
            % {NewMeta, Res, D} = json_mutation(Tokens),
            NewMeta = [], D = 1,
            NewBinStr = fold_ast(hd(Tokens)),
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