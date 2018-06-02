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
%%% Lexing/unlexing string in the data flow.
%%% @end
%%%-------------------------------------------------------------------
-module(erlamsa_strlex).
-author("dark_k3y").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

-include("erlamsa.hrl").

%% API
-export([lex/1, unlex/1]).

%% minimum texty bytes in sequence to be considered interesting
%% note - likely to happen in longish data (a few kb) anyway
-define(MIN_TEXTY, 6).

-spec texty(byte()) -> true | false.
texty(B) when B < 9 -> false;
texty(B) when B > 126 -> false;
texty(B) when B > 31 -> true;
texty(9) -> true;
texty(10) -> true;
texty(13) -> true;
texty(_) -> false.

-spec texty_enough(string()) -> true | false.
texty_enough(Lst) -> texty_enough(Lst, ?MIN_TEXTY).

-spec texty_enough(string(), non_neg_integer()) -> true | false.
texty_enough([], _) -> true; % match short textual input, accidentally also short trailing ones
texty_enough(_, 0) -> true;
texty_enough([H|T], N) ->
    case texty(H) of
        true -> texty_enough(T, N - 1);
        false -> false
    end.

%% TODO: fix spec of chunk
-spec flush_type_node(chunk_type(), string(), chunk_list()) -> chunk_list().
flush_type_node(Type, Bytes, Chunks) ->
    [{Type, lists:reverse(Bytes)} | Chunks].

%% (byte ..) -> (node ...)
%%  node = #(byte bytes...) | #(text bytes) | #(delimited byte (byte ...) byte)
%% TODO: fix spec of chunk
-spec lex(string()) -> chunk_list().
lex(Lst) -> string_lex_step(Lst, [], []).

%% TODO: fix spec of chunk
-spec string_lex_step(string(), string(), chunk_list()) -> chunk_list().
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


%% TODO: fix spec of chunk
-spec step_text(string(), string(), chunk_list()) -> chunk_list().
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
%% TODO: fix spec of chunk
-spec step_delimited(string(), byte(), byte(), string(), string(), chunk_list()) -> chunk_list().
step_delimited([], _Start, _End, AfterR, PrevR, Chunks) ->
    lists:reverse(flush_type_node(text, AfterR ++ PrevR, Chunks));
%% finish text chunk
step_delimited(_Lst = [H|T], Start, End, AfterR,
               _PrevR = [_PrevrH | PrevrT], Chunks) when H =:= End ->
    Node = {delimited, Start, lists:reverse(AfterR), End},
    case PrevrT =:= [] of
        true -> string_lex_step(T, [], [Node | Chunks]);
        false -> string_lex_step(T, [], [Node, {text, lists:reverse(PrevrT)} | Chunks])
    end;
%% skip byte after quotation, if it seems texty
step_delimited(_Lst = [H|T], Start, End, AfterR, PrevR, Chunks) when H =:= 92, T =:= [] ->
    step_delimited(T, Start, End, [92 | AfterR], PrevR, Chunks);
step_delimited(_Lst = [H|T], Start, End, AfterR, PrevR, Chunks) when H =:= 92 ->
    case texty(hd(T)) of
        true -> step_delimited(tl(T), Start, End, [hd(T), 92 | AfterR], PrevR, Chunks);
        false -> step_delimited(T, Start, End, [H | AfterR], PrevR, Chunks)
    end;
step_delimited(Lst = [H|T], Start, End, AfterR, PrevR, Chunks) ->
    case texty(H) of
        true -> step_delimited(T, Start, End, [H | AfterR], PrevR, Chunks);
        false -> string_lex_step(Lst, [], flush_type_node(text, AfterR ++ PrevR, Chunks))
    end.

%% TODO: fix spec of chunk
-spec unlex(chunk_list()) -> string().
unlex(Chunks) ->
    lists:foldr(
        fun
            ({byte, T}, Tail) ->
                T ++ Tail;
            ({delimited, LD, L, RD}, Tail) ->
                [LD | L] ++ [RD | Tail];
            ({text, T}, Tail) ->
                T ++ Tail
        end
        , [], Chunks).