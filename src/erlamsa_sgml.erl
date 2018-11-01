% Copyright (c) 2014-2015 Alexander Bolshev aka dark_k3y
% The following SGML parser tokenizer is hardly based on the Mats Cronqvist
% "trane" html parser: https://github.com/massemanet/trane
%
% Copyright (c) 2010 Mats Cronqvist
%
% Permission is hereby granted, free of charge, to any person
% obtaining a copy of this software and associated documentation
% files (the "Software"), to deal in the Software without
% restriction, including without limitation the rights to use,
% copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the
% Software is furnished to do so, subject to the following
% conditions:

% The above copyright notice and this permission notice shall be
% included in all copies or substantial portions of the Software.

% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
% OTHER DEALINGS IN THE SOFTWARE.

% TODO:
% * correct <!NAME PARAM PARAM handling

-module(erlamsa_sgml).

-author("dark_k3y").

-compile([export_all]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("erlamsa.hrl").

%% API
-export([]).

%%%
%%% SGML tokenizer
%%%

-define(m(X,B),<<X,B/binary>>). % match
-define(d(X,B),<<X:8/integer,B/binary>>). % decompose
-define(M(X,B),<<X,_/binary>> = B). % match
-define(D(X,B),<<X:8/integer,_/binary>> = B). % decompose

%TODO: original
%-define(ok(X),$a=<X,X=<$z;$A=<X,X=<$Z;$0=<X,X=<$9;X==$_;X==$-;X==$:).
-define(ok(X),X=/=$>;X=/=$<;X=/=$';X=/=$";X=/=$!;X=/=$/).
-define(ws(X),X==$\s;X==$\r;X==$\n;X==$\t).
-define(dq(X),X==$").
-define(sq(X),X==$').

-define(j3(A,B,C), <<A/binary, B/binary, C/binary>>).  % join 3

-define(ev(X), ?ws(X); X==$>; X==$=).

tokenize(Str) when is_integer(hd(Str))-> tokenize(list_to_binary(Str));
tokenize(Str) when is_binary(Str) -> tokenize(tz(nil,Str));

tokenize({TZ,Str}) ->
  tokenize(tz(TZ,Str));
tokenize({text,Str,Token}) ->
  %% io:format("=====> ~s ~w~n", [Str, Tst]),
  %% io:format("========> ~w~n", [ff(text,Str)]),
  case ff(text,Str) of
    {{tag,""},EStr,{text,Txt}} ->
      try
        %% io:format("=========try> ~w~n", [{{tag,""},EStr}]),
        [Token,{text,Txt}] ++ tokenize({{tag,""},EStr})
      catch _:_ ->
          tokenize({bad_text, <<Txt/binary>>, <<"<">>, <<EStr/binary>>, Token})
      end;
    {{text,Str},"",eof} ->
      [Token,{eof,{text,Str}}]
  end;
%% TODO: very *DIRTY* hack for handling incorrect SGML (but "good" for browser's HTML) docs
tokenize({bad_text, Part1, BadSymbol, Part2, Token}) ->
  case ff(text,Part2) of
    {{tag,""},EStr,{text,Txt}} ->
      try
        [Token,{text, ?j3(Part1, BadSymbol, Txt)}] ++ tokenize({{tag,""},EStr})
      catch _:_ ->
          tokenize({bad_text, ?j3(Part1, BadSymbol, Txt), <<"<">>, <<EStr/binary>>, Token})
      end;
    {{text,Part2},"",eof} ->
      [Token,{eof,{text, ?j3(Part1, BadSymbol, Part2)}}]
  end;
tokenize({TZ,Str,Token}) ->
  [{Token,{TZ,Str}}].

tz(nil,?m("<",Str))                       -> {{tag,""},ws(Str)};
tz(nil,?d(_,Str))                         -> {nil,Str};
tz(nil, <<>>)                         	  -> throw(incorrect_sgml);

tz({tag,""},?m("!--",Str))                -> {{'!--',""},Str};
tz({tag,""},?m("!",Str))                  -> {{'!',""},ws(Str)};
tz({tag,""},?m("?",Str))                  -> {{que,""},ws(Str)};
tz({tag,""},?m("/",Str))                  -> {{end_tag,""},ws(Str)};
tz({tag,Tag},?m("/>",Str))                -> {text,Str,{sc,Tag,[]}};
tz({tag,Tag},?D(X,S)) when ?ev(X)         -> {{attr,"",{Tag,[]}},ws(S)};
tz({tag,Tag},?d(X,Str)) when ?ok(X)       -> {{tag,Tag++[X]},Str};
tz({tag,_}, <<>>)                     	  -> throw(incorrect_sgml);

tz({'!',DT},?m(">",Str))                  -> {text,Str,{'!',DT}};
tz({'!',DT},?d(X,Str))                    -> {{'!',DT++[X]},Str};
tz({'!',_}, <<>>)                         -> throw(incorrect_sgml);

tz({'!--',DT},?m("-->", Str))              -> {text,Str,{'!--',DT}};
tz({'!--',DT},?d(X,Str))                   -> {{'!--',DT++[X]},Str};

tz({que,DT},?m("?>",Str))                 -> {text,Str,{que,DT}};
tz({que,DT},?d(X,Str))                    -> {{que,DT++[X]},Str};
tz({que,_}, <<>>)                         -> throw(incorrect_sgml);

tz({etag,Tag,Attrs},?m("/>",Str))         -> {text,Str,{sc,Tag,Attrs}};
tz({etag,Tag,Attrs},?m(">",Str))          -> {text,Str,{open,Tag,Attrs}};
tz({etag,_Tag,_Attrs}, _)          		  -> throw(incorrect_sgml);

tz({end_tag,Tag},?D(X,S)) when ?ev(X)     -> {{end_tag,Tag,'>'},ws(S)};
tz({end_tag,Tag,'>'},?m(">",Str))         -> {text,Str,{close,Tag,string:to_lower(Tag)}};
tz({end_tag,Tag},?d(X,Str))               -> {{end_tag,Tag++[X]},Str};
tz({end_tag,_,_},_)              	      -> throw(incorrect_sgml);
tz({end_tag,_},_)              			  -> throw(incorrect_sgml);

tz({attr,"",{Tag,As}},?D(X,S))when ?ev(X) -> {{etag,Tag,As},S};
tz({attr,"",{Tag,As}},?M("/>",S))         -> {{etag,Tag,As},S};
tz({attr,A,{T,As}},?D(X,S))when ?ev(X)    -> {{eatt,{A,T,As}},ws(S)};
tz({attr,A,{T,As}},?M("/>",S))            -> {{eatt,{A,T,As}},ws(S)};
tz({attr,A,TAs},?d(X,Str))when ?ok(X)     -> {{attr,A++[X],TAs},Str};
tz({attr,_,_},_)						  -> throw(incorrect_sgml); % if '/"" unclose

tz({eatt,ATAs},?m("=",Str))               -> {{val,ATAs},ws(Str)};
tz({eatt,{A,T,As}},S)                     -> {{attr,"",{T,As++[{A,"",[]}]}},ws(S)};

tz({val,ATAs},?m("'",Str))                -> {{sqval,"",ATAs},Str}; % singlequoted
tz({val,ATAs},?m("\"",Str))               -> {{dqval,"",ATAs},Str}; % doublequoted
tz({val,ATAs},Str)                        -> {{uqval,"",ATAs},Str}; % unquoted

tz({sqval,V,{A,T,As}},?m("'",S))          -> {{attr,"",{T,As++[{A,V,"'"}]}},ws(S)};
tz({sqval,V,ATAs},?d(X,Str))              -> {{sqval,V++[X],ATAs},Str};
tz({sqval,_,_},_)						  -> throw(incorrect_sgml); % if ' unclose

tz({dqval,V,{A,T,As}},?m("\"",S))         -> {{attr,"",{T,As++[{A,V,"\""}]}},ws(S)};
tz({dqval,V,ATAs},?d(X,Str))              -> {{dqval,V++[X],ATAs},Str};
tz({dqval,_,_},_)						  -> throw(incorrect_sgml); % if ' unclose


tz({uqval,V,{A,T,As}},?D(X,S))when ?ev(X) -> {{attr,"",{T,As++[{A,V,[]}]}},ws(S)};
tz({uqval,V,{A,T,As}},?M("/>",S))         -> {{attr,"",{T,As++[{A,V,[]}]}},ws(S)};
tz({uqval,V,ATAs},?d(X,Str))              -> {{uqval,V++[X],ATAs},Str};
tz({uqval,_,_},<<>>)                      -> throw(incorrect_sgml);

tz(text,Str)                              -> ff(text,Str);

tz(X,"")                                  -> {X,"",eof}.

ff(What,Str) -> ff(What,Str,0,Str).

ff(text,<<"<",Str/binary>>,N,Bin) ->
  {Scr,_} = split_binary(Bin,N),
  {{tag,""},ws(Str),{text,Scr}};
ff(_,<<>>,_,Str) ->
  {{text,Str},"",eof};
ff(What,<<_,Str/binary>>,N,Bin) ->
  ff(What,Str,N+1,Bin).

ws(?d(X,Str)) when ?ws(X) -> ws(Str);
ws(Str) -> Str.

%%%
%%% /SGML tokenizer
%%%

%%%
%%% AST builder
%%%

push_till([H|T], H) -> T;
push_till([_|T], H) -> push_till(T, H);
push_till([], _) -> [].

parse(Str) ->
    parse(Str, erlamsa_utils:binarish(Str)).

parse(Str, false) ->
    Tokens = tokenize(Str),
    %io:format("~w~n", [Tokens]),
    build_ast2(Tokens, [], [], {0, 0});
parse(_Str, true) ->
    throw(incorrect_sgml).

% list(token()), list(ast_elem()), list(strings) -> atom(), list(ast_elem()), list(ast_elem(),
%		{non_neg_int(), non_neg_int()})
% non_neg_int() is tag (paired)
build_ast2([{open, Tag, Params}|T], Ast, Tags, {N, NT}) ->
    TolowerTag = string:to_lower(Tag),
    %io:format("!!! Open ~s (~s), >>~w<<, ~w~n", [Tag, TolowerTag, Ast, Tags]),
    Test = build_ast2(T, [], [TolowerTag | Tags], {0, 0}),
    %io:format("Test is ~w~n", [Test]),
    case Test of
        {ok, [{tagclose, TagClose} | TagInternals], TagOuters, {K, KT}} ->
            %io:format("open_ok for ~w: ~w, ~w, ~w, +1~n", [Tag, TagInternals, TagOuters, Tags]),
            build_ast2(TagOuters, [{tag, Tag, TagClose, Params, TagInternals}|Ast],
                                    Tags, {N + K + 1, NT + KT + 1});
        % {error, no_pair_tag, Tokens, NewAst, TagsTail, {K, KT}} ->
        % 	io:format("Last tag ~w, ~w, ~s~n", [Tokens, NewAst, Tag]),
        % 	io:format("Go d33per ~w, ~w, ~w, ~w, ~w, tags: ~w~n",
        %			  [Tokens, Ast, NewAst, [{open, Tag, Params}], Tag, TagsTail]),
        % 	build_ast2(Tokens, NewAst ++ [{open, Tag, Params}] ++ Ast , TagsTail,
        %				{N + K + 1, NT + KT});	%% [] -- ????
        {error, no_pair_tag, Tokens, NewAst, NewTags, {K, KT}} ->
            %io:format("No_pair tag ~w, ~w, ~w~n", [Tokens, NewAst, NewTags]),
            %io:format("Go d33per ~w, ~w, ~w, ~w, ~w~n", [Tokens, Ast, NewAst,
            % 											  [{open, Tag, Params}], Tag]),
            build_ast2(Tokens, NewAst ++ [{open, Tag, Params}] ++ Ast , NewTags,
                      {N + K + 1, NT + KT});
        {error, closed_earlier, {TolowerTag, CloseTag}, Tokens, NewAst, NewTags, {K, KT}} ->
            %io:format("Err_close_earlier ~w, ~w, ~w, +1~n", [Tokens, NewAst, NewTags]),
            build_ast2(Tokens, [{tag, Tag, CloseTag, Params, lists:reverse(NewAst)}|Ast],
                       NewTags, {N + K + 1, NT + KT + 1});
        {error, closed_earlier, {NewTag, NewCloseTag}, Tokens, NewAst, NewTags, {K, KT}} ->
            %io:format("Err_close_earlier upper pass ~w, ~w, ~w~n", [Tokens, NewAst, NewTags]),
            {error, closed_earlier, {NewTag, NewCloseTag}, Tokens,
             NewAst ++ [{open, Tag, Params}] ++ Ast, NewTags,
             {N + K + 1, NT + KT}
            }
    end;
build_ast2([{close, Tag, TagName}|T], Ast, [TagName|_Tags], {N, NT}) ->
    %io:format("Close ~s, ~w, ~w~n", [Tag, Ast, [TagName|_Tags]]),
    {ok, [{tagclose, Tag} | lists:reverse(Ast)], T, {N, NT}};
build_ast2([{close, Tag, TagName}|T], Ast, [_OtherTag|Tags] = AllTags, {N, NT}) ->
    %io:format("Close no pair ~s, ~w, ~w~n", [Tag, Ast, AllTags]),
    case lists:member(TagName, Tags) of
        false ->
            % io:format("Close invalid ~s, +1~n", [Tag]),
            build_ast2(T, [{close, Tag} | Ast], AllTags, {N + 1, NT});
        true ->
            % io:format("Close earlier ~s~n", [Tag]),
            {error, closed_earlier, {TagName, Tag}, T, Ast, push_till(Tags, TagName), {N, NT}}
    end;
build_ast2([{close, Tag, _TagName}|T], Ast, [], {N, NT}) ->
    %io:format("Close no pair empty ~s, ~w, ~w, +1~n", [Tag, Ast, []]),
    build_ast2(T, [{close, Tag} | Ast], [], {N + 1, NT});
build_ast2([{text, <<>>}|T], Ast, Tags, {N, NT}) ->
    %io:format("Empty text ~w, ~w, ~w~n", [<<>>, Ast, Tags]),
    build_ast2(T, Ast, Tags, {N, NT});
build_ast2([{text, Text}|T], Ast, Tags, {N, NT}) ->
    %io:format("Text ~w, ~w, ~w, +1~n", [Text, Ast, Tags]),
    build_ast2(T, [{text, Text}|Ast], Tags, {N + 1, NT});
build_ast2([{'!', Params}|T], Ast, Tags, {N, NT}) ->
    %io:format("! ~w, ~w, ~w, +1~n", [Params, Ast, Tags]),
    build_ast2(T, [{'!', Params}|Ast], Tags, {N + 1, NT});
build_ast2([{'!--', Params}|T], Ast, Tags, {N, NT}) ->
    %io:format("! ~w, ~w, ~w, +1~n", [Params, Ast, Tags]),
    build_ast2(T, [{'!--', Params}|Ast], Tags, {N + 1, NT});
build_ast2([{que, Params}|T], Ast, Tags, {N, NT}) ->
    %io:format("! ~w, ~w, ~w, +1~n", [Params, Ast, Tags]),
    build_ast2(T, [{que, Params}|Ast], Tags, {N + 1, NT});
build_ast2([{sc, Text, Params}|T], Ast, Tags, {N, NT}) ->
    %io:format("SingleTag ~w, ~w, ~w, +1~n", [Text, Ast, Tags]),
    build_ast2(T, [{sc, Text, Params}|Ast], Tags, {N + 1, NT});
build_ast2([{eof, {text, <<>>}}|_T], Ast, [], {N, NT}) ->
    %io:format("Eof empty_text ~w, ~w, Tags: ~w~n", [<<>>, Ast, []]),
    {ok, lists:reverse(Ast), [], {N, NT}};
build_ast2([{eof, {text, Text}}|_T], Ast, [], {N, NT}) ->
    %io:format("Eof ~w, ~w, Tags: ~w, +1~n", [Text, Ast, []]),
    {ok, lists:reverse([{text, Text}|Ast]), [], {N + 1, NT}};
build_ast2([{eof, {text, _Text}}|_T] = Tokens, Ast, [_Tag|Tags], {N, NT}) ->
    %io:format("Eof no_pair ~w, ~w, ~w, Tag: ~s ~n", [Text, Ast, Tags, Tag]),
    {error, no_pair_tag, Tokens, Ast, Tags, {N, NT}}.

%%%
%%% /AST builder
%%%


%%%
%%% AST folder
%%%

enclose_param_value("'", Value) ->
    [<<"'">>, list_to_binary(Value), <<"'">>];
enclose_param_value("\"", Value) ->
    [<<"\"">>, list_to_binary(Value), <<"\"">>];
enclose_param_value([], Value) ->
    [list_to_binary(Value)].

fold_params([{ParamName, [], _Type}|T], Acc) ->
    fold_params(T, [list_to_binary(ParamName), <<" ">> | Acc]);
fold_params([{ParamName, Value, Type}|T], Acc) ->
    fold_params(T, [[<<" ">>, list_to_binary(ParamName), <<"=">>,
                enclose_param_value(Type, Value)] | Acc]);
fold_params([], Acc) ->
    lists:reverse(Acc).

fold_ast([{tag, OpenName, CloseName, Params, Internals} | T], Acc) ->
    fold_ast(T, [list_to_binary([<<"<">>, OpenName, fold_params(Params, []), <<">">>,
                 fold_ast(Internals, []), <<"</">>, CloseName, <<">">>]) | Acc]
            );
fold_ast([{text, Text}| T], Acc) ->
    fold_ast(T, [Text | Acc]);
fold_ast([{sc, Name, Params}| T], Acc) ->
    fold_ast(T, [list_to_binary([<<"<">>, Name, fold_params(Params, []), <<" />">>]) | Acc]);
fold_ast([{que, Params}| T], Acc) ->
    fold_ast(T, [list_to_binary([<<"<">>, <<"?">>, Params, <<"?>">>])
     | Acc]);
fold_ast([{'!', Params}| T], Acc) ->
    fold_ast(T, [list_to_binary([<<"<">>, <<"!">>, Params, <<">">>])
     | Acc]);
fold_ast([{'!--', Params}| T], Acc) ->
    fold_ast(T, [list_to_binary([<<"<">>, <<"!--">>, Params, <<"-->">>])
     | Acc]);
fold_ast([{open, Tag, Params} | T], Acc) ->
    fold_ast(T, [list_to_binary([<<"<">>, Tag, fold_params(Params, []), <<">">>]) | Acc]);
fold_ast([{close, Tag} | T], Acc) ->
    fold_ast(T, [list_to_binary([<<"</">>, Tag, <<">">>]) | Acc]);
fold_ast([{tagclose, _Tag} | T], Acc) ->
    fold_ast(T, Acc);
fold_ast([[] | T], Acc) ->  %% in case of invalid AST after tag breaking
    fold_ast(T, Acc);
fold_ast([], Acc) ->
    list_to_binary(lists:reverse(Acc)).

%%%
%%% /AST folder
%%%

%%%
%%% AST utils
%%%

walk_reverse(L) when is_list(L) -> lists:reverse(L);
walk_reverse(L) -> L.

walk(Ast, Fun, InitAcc) ->
    {Res, ResT, ResN} = lists:foldl(
        fun
            Walker({tag, OpenName, CloseName, Params, Internals} = _Elem, {Acc, TagCnt, Count}) ->
                %io:format("~w is ~w (if tag then ~w)~n", [_Elem, Count + 1, TagCnt + 1]),
                {ChildAcc, ChildTagCnt, ChildCount} = lists:foldl(Walker,
                                                                  {InitAcc, TagCnt + 1, Count + 1},
                                                                   Internals),
                {Fun({tag, OpenName, CloseName, Params, walk_reverse(ChildAcc)},
                 Acc, TagCnt + 1, Count + 1),
                 ChildTagCnt, ChildCount
                };
            Walker(Elem, {Acc, TagCnt, Count}) ->
                %io:format("~w is ~w (if tag then ~w)~n", [Elem, Count + 1, TagCnt]),
                {Fun(Elem, Acc, TagCnt, Count + 1), TagCnt, Count + 1}
        end,
        {InitAcc, 0, 0}, Ast),
    {walk_reverse(Res), ResT, ResN}.

walk2acc(Ast, Fun, InitAcc) ->
    {Res, Res2} = lists:foldl(
        fun
            Walker({tag, OpenName, CloseName, Params, Internals} = _Elem, {Acc, Acc2}) ->
                %io:format("Elem = ~p, Acc = ~p, Acc2 = ~p~n", [_Elem, Acc, Acc2]),
                {ChildAcc, ChildAcc2} = lists:foldl(Walker, InitAcc, Internals),                                            
                %io:format("!CAcc = ~p, CAcc2 = ~p : ~p~n", [ChildAcc, ChildAcc2, Res1]),
                {UAcc, UAcc2} = Fun({tag, OpenName, CloseName, Params, walk_reverse(ChildAcc)}, 
                                    Acc, Acc2),
                %io:format("&UAcc = ~p, UAcc2 = ~p~n", [UAcc, UAcc2]),
                {UAcc, [ChildAcc2|UAcc2]};
            Walker(Elem, {Acc, Acc2}) ->
                %io:format("@Elem = ~p, CAcc = ~p, CAcc2 = ~p~n", [Elem, Acc, Acc2]),
                Fun(Elem, Acc, Acc2)                
        end,
        InitAcc, Ast),
    {walk_reverse(Res), lists:reverse(lists:flatten(Res2))}.

select(Ast, Selector) ->
    lists:foldl(
        fun
            Walker({tag, _, _, _, Internals} = Elem, {Acc, TagCnt, Count}) ->
                %io:format("~w is ~w (if tag then ~w)~n", [Elem, Count + 1, TagCnt + 1]),
                case Selector(Elem, TagCnt + 1, Count + 1) of
                    false ->
                        %io:format("selector return false~n"),
                        lists:foldl(Walker, {Acc, TagCnt + 1, Count + 1}, Internals);
                    Res ->
                        {Res, TagCnt + 1, Count + 1}
                end;
            Walker(Elem, {Acc, TagCnt, Count}) ->
                %io:format("~w is ~w (if tag then ~w)~n", [Elem, Count + 1, TagCnt]),
                case Selector(Elem, TagCnt, Count + 1) of
                    false ->
                        %io:format("selector return false~n"),
                        {Acc, TagCnt, Count + 1};
                    Res ->
                        {Res, TagCnt, Count + 1}
                end
        end,
        {[], 0, 0},
        Ast).

test_walk(Ast) ->
    walk(Ast,
        fun  (_E, Acc, _T, _N) ->
            %io:format("~w is ~w (if tag then ~w)~n", [E, N, T]),
                Acc end,
        []).

count(Ast) ->
    walk(Ast,
        fun
            ({tag, _, _, _, ChildAcc}, Acc, _, _) ->
                Acc + ChildAcc + 1;
            ({NonTag, _} = _E, Acc, _, _) when NonTag == text; NonTag == '!'; NonTag == sc ->
                Acc;
            (_, Acc, _, _) ->
                Acc + 1
        end, 0).

select_tag(Ast, N) ->
    {Res, _, _} = select(Ast,
            fun
                ({tag, _, _, _, _} = Elem, I, C) when I =:= N ->
                    {Elem, I, C};
                (_, _, _) ->
                    false
            end),
    Res.


select_elem(Ast, N) ->
    {Res, _, _} = select(Ast,
            fun
                (Elem, T, I) when I =:= N ->
                    {Elem, T, I};
                (_, _, _) ->
                    false
            end),
    Res.

replace_elem(Ast, R, El) ->
    walk(Ast,
        fun
            (_Elem, Tree, _, I) when I == R ->
                %io:format("~w No!!!!!. ~w (~w)~n", [_Elem, I, R]),
                [El | Tree];
            (Elem, Tree, _, _I) ->
                %io:format("~w No. ~w (~w)~n", [Elem, I, R]),
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
%%% /AST utils
%%%

%%%
%%% Mutators
%%%


pump_path(Start, _End, 0) -> Start;
pump_path(Start, End, N) ->
    %io:format("Pumping... iter ~w start ~w I =  ~w~n", [N, Start, End]),
    {PumpedTag, _, _} = walk([Start],
            fun
                (_Elem, Tree, _, I) when I =:= End ->
                    [Start | Tree];
                (Elem, Tree, _, _) ->
                    [Elem | Tree]
            end, []),
    %io:format("Pumping... iter ~w res ~w~n", [N, PumpedTag]),
    pump_path(hd(PumpedTag), End*2 - 1, N - 1).


sgml_pump(Ast, 0) -> {Ast, 0, 0};
sgml_pump(Ast, T) ->
    %io:format("Cnt: ~w ~w~n", [Ast, N]),
    R = erlamsa_rnd:erand(T),
    %io:format("R:!!!!!!!!!!!!!!!!!! ~w~n", [R]),
    {Start, R, StartPlace} = select_tag(Ast, R),
    %io:format("Start:!!!!!!!!!!!!!!!!!! ~w~n", [Start]),
    {_, _, SubElems} = count([Start]),
    %io:format("SubElems:!!!!!!!!!!!!!!!!!! ~w~n", [SubElems]),
    E = erlamsa_rnd:erand(SubElems - 1) + 1, %% not the tag itself
    %io:format("E:!!!!!!!!!!!!!!!!!! ~w~n", [E]),
    %{End, _, _} = select_elem([Start], E),
    %io:format("End:!!!!!!!!!!!!!!!!!! ~w~n", [End]),
    PumpCnt = erlamsa_rnd:erand( trunc(1000.0/(100.0 + SubElems)) ),
    % ^-- limiting the depth of the pump
    %io:format("PumpCnt:!!!!!!!!!!!!!!!!!! ~w~n", [PumpCnt]),
    Pumped = pump_path(Start, E, PumpCnt),
    %io:format("PUMPED!!!!!!!!!! ~w~n", [Pumped]),
    replace_elem(Ast, StartPlace, Pumped).

sgml_dup(Ast, N) ->
    R = erlamsa_rnd:erand(N),
    repeat_elem(Ast, R, 1).

sgml_repeat(Ast, N) ->
    R = erlamsa_rnd:erand(N),
    repeat_elem(Ast, R, erlamsa_rnd:erand(100)).

sgml_swap(Ast, N) ->
    R1 = erlamsa_rnd:erand(N),
    R2 = erlamsa_rnd:erand(N),
    {Elem1, _, R1} = select_elem(Ast, R1),
    {Elem2, _, R2} = select_elem(Ast, R2),
    walk(Ast,
        fun
            (_Elem, Tree, _, I) when I == R1 ->
                [Elem2 | Tree];
            (_Elem, Tree, _, I) when I == R2 ->
                [Elem1 | Tree];
            (Elem, Tree, _, _I) ->
                [Elem | Tree]
        end, []).



sgml_insert(Ast, N) ->
    R1 = erlamsa_rnd:erand(N),
    R2 = erlamsa_rnd:erand(N),
    {NewElem, _, R1} = select_elem(Ast, R1),
    case NewElem of
        {tag, Tag, TagClose, Params, _Int} ->
            walk(Ast,
                fun
                    (Elem, Tree, _, I) when I == R2 ->
                        [{tag, Tag, TagClose, Params, [Elem]}| Tree];
                    (Elem, Tree, _, _I) ->
                        [Elem | Tree]
                end, []);
        _Else ->
            insert_elem(Ast, R2, NewElem)
    end.


sgml_insert2(Ast, N) ->
    R1 = erlamsa_rnd:erand(N),
    R2 = erlamsa_rnd:erand(N),
    {NewElem, _, R1} = select_elem(Ast, R1),
    insert_elem(Ast, R2, NewElem).

sgml_permparams(Ast, T) ->
    R = erlamsa_rnd:erand(T),
    walk(Ast,
        fun
            ({tag, Tag, TagClose, Params, Internals}, Tree, I, _) when I == R ->
                [{tag, Tag, TagClose, erlamsa_rnd:random_permutation(Params), Internals}| Tree];
            (Elem, Tree, _, _I) ->
                [Elem | Tree]
        end, []).

sgml_breaktag(Ast, T) ->
    R = erlamsa_rnd:erand(T),
    walk(Ast,
        fun
            ({tag, Tag, TagClose, Params, Internals}, Tree, I, _) when I == R ->
                case erlamsa_rnd:rand(1) of
                    0 -> Internals ++ [{open, Tag, Params} | Tree];
                    1 -> Internals ++ [{close, TagClose} | Tree]
                end;
            (Elem, Tree, _, _I) ->
                [Elem | Tree]
        end, []).

xmlns_modify_params(Params) ->
    NewParams = xmlns_modify_params(Params, []),
    case Params =:= NewParams of
        true -> 
            Uri = lists:flatten(["http", erlamsa_mutations:get_ssrf_uri()]),
            [{"xmlns", Uri, "\""},
             {"xmlns:xsi", Uri, "\""},
             {"xsi:schemaLocation", Uri, "\""}
             | Params];
        _Else -> NewParams
    end.

xmlns_modify_params([{Name = [$x, $m, $l, $n, $s | _NameTail], Uri, Delim} | T], Acc) ->
    NewUri = lists:flatten(
                case erlamsa_rnd:erand(2) of
                    1 -> Uri ++ " http" ++ erlamsa_mutations:get_ssrf_uri();
                    2 -> "http" ++ erlamsa_mutations:get_ssrf_uri()
                end),
    xmlns_modify_params(T, [{Name, NewUri, Delim}|Acc]);
xmlns_modify_params([H | T], Acc) ->
    xmlns_modify_params(T, [H|Acc]);
xmlns_modify_params([], Acc) -> 
    lists:reverse(Acc).

xmlns_modify(Tag = {tag, Name1, Name2, Params, Childs}, T) ->
    case erlamsa_rnd:erand(trunc(T*1.5)) of
        1 -> 
            NewParams = xmlns_modify_params(Params),
            {tag, Name1, Name2, NewParams, Childs};
        _Else ->
            Tag
    end.

sgml_xmlfeatures(Ast = [H|T], NT, 0) when NT > 0 ->
    case H of
        {que, _} -> 
            Elem = erlamsa_rnd:erand(NT),
            {Tag, _, Place} = select_tag(T, Elem),
            {tag, TagName, TagName, P, L} = Tag,
            SSRFUri = erlamsa_mutations:get_ssrf_uri(),
            DocType = erlamsa_rnd:rand_elem([
                %% TODO: add more tests for XML eXternal entities
                %% plain XXEs
                io_lib:format("<!DOCTYPE ~s PUBLIC \"...\" \"http~s\">", [TagName, SSRFUri]),
                io_lib:format("<!DOCTYPE ~s SYSTEM \"/dev/zero\">", [TagName]),
                io_lib:format("<!DOCTYPE ~s SYSTEM \"http~s\">", [TagName, SSRFUri]),
                io_lib:format("<!DOCTYPE ~s [<!ELEMENT ~s ANY ><!ENTITY xxe SYSTEM \"http~s\" >]>", 
                              [TagName, TagName, SSRFUri]),
                %% billion of lol'z overload
                io_lib:format("<!DOCTYPE ~s [~n <!ENTITY lol \"lol\">~n <!ELEMENT lolz (#PCDATA)>~n<!ENTITY lol1 \"&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;\">~n<!ENTITY lol2 \"&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;\">~n<!ENTITY lol3 \"&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;\">~n<!ENTITY lol4 \"&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;\">~n<!ENTITY lol5 \"&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;\">~n<!ENTITY lol6 \"&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;\">~n<!ENTITY lol7 \"&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;\">~n<!ENTITY lol8 \"&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;\">~n<!ENTITY xxe \"&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;\">~n]>", [TagName])                             
                ]),
            XXE = {text, list_to_binary(lists:flatten(DocType))},           
            NewTag = {tag, TagName, TagName, P, [{text,<<"&xxe;">>}|L]},
            {NewT, _, _} = replace_elem(T, Place, NewTag),
            {[H, XXE | NewT], [{sgml_xmlfeatures, xxe}], 1};
        _Else -> {Ast, [{sgml_xmlfeatures, failed}], -1}
    end;
sgml_xmlfeatures(Ast, NT, 1) when NT > 0 ->
    {NewAst, _, _} = walk(Ast,
        fun (Tag = {tag,_, _, _, _}, Tree, T, _) ->
                NewTag = xmlns_modify(Tag, T),
                [NewTag | Tree]; 
            (Elem, Tree, _, _) ->
                [Elem | Tree]
        end,
        []),
    case Ast =:= NewAst of
        true -> {Ast, [{sgml_xmlfeatures, failed}], -1};
        _Else -> {NewAst, [{sgml_xmlfeatures, xmlns}], 1}
    end;
sgml_xmlfeatures(Ast, _NT, _) ->
    {Ast, [{sgml_xmlfeatures, -1}], -1}.

mutate_innertext_prob(Binary, _Muta, Prob, Rnd) when Rnd > Prob ->
    {[], Binary}; %% No mutations
mutate_innertext_prob(Binary, Muta, _Prob, _Rnd) ->
    {_NewMuta, NewLstBin, Meta} = Muta([Binary], []),
    %%io:format("res: ~p~n", [{NewLstBin, Meta}]),
    {Meta, hd(NewLstBin)}.

mutate_innertext(Binary, Muta, NT) ->
    case length([X || X <- binary_to_list(Binary), X =/= 0, X =/= 10, X =/=13, X =/= 32]) of
        NW when NW > 0, NT > 0 ->
            %% Probability of text mutation is 3/tags, we're thinking that each 3rd tag has inner text
            mutate_innertext_prob(Binary, Muta, 3/NT, erlamsa_rnd:rand_float());
        _Else ->
            {[], Binary}
    end.

try_mutate_innertext({tag, OpenName, CloseName, Params, Spec}, Muta, NT) ->
    NP = length(Params),
    {NewParams, Meta} = lists:mapfoldl(
        fun ({Name, Value, Delim}, Acc) ->
            {Meta, NewVal} = mutate_innertext(list_to_binary(Value), Muta, NT + NP), 
            {{Name, binary_to_list(NewVal), Delim}, [Meta|Acc]}
        end, [], Params),
    {Meta, {tag, OpenName, CloseName, NewParams, Spec}};
try_mutate_innertext({text, Binary}, Muta, NT) when is_binary(Binary) ->
    {Meta, NewEl} = mutate_innertext(Binary, Muta, NT), {Meta, {text, NewEl}};
try_mutate_innertext(El, _Muta, _NT) -> {[], El}.


sgml_mutation(Ast, {N, NT}) ->
    % io:format("~n~n~w~n~n", [Ast]),
    sgml_mutation(Ast, {N, NT}, erlamsa_rnd:rand(12)).

sgml_mutation(Ast, {N, _NT}, 0) ->
    {Res, _, _} = sgml_swap(Ast, N),
    {[{sgml_swap, 1}], Res, 1};
sgml_mutation(Ast, {N, _NT}, 1) ->
    {Res, _, _} = sgml_dup(Ast, N),
    {[{sgml_dup, 1}], Res, 1};
sgml_mutation(Ast, {_N, NT}, 2) ->
    {Res, _, _} = sgml_pump(Ast, NT),
    {[{sgml_pump, 1}], Res, -2}; %% don't allow too much pumps...
sgml_mutation(Ast, {N, _NT}, 3) ->
    {Res, _, _} = sgml_repeat(Ast, N),
    {[{sgml_repeat, 1}], Res, 1};
sgml_mutation(Ast, {N, _NT}, 4) ->
    {Res, _, _} = sgml_insert2(Ast, N),
    {[{sgml_insert2, 1}], Res, 1};
sgml_mutation(Ast, {_N, NT}, 5) ->
    {Res, _, _} = sgml_permparams(Ast, NT),
    {[{sgml_permparams, 1}], Res, 1};
sgml_mutation(Ast, {_N, NT}, 6) ->
    {Res, _, _} = sgml_breaktag(Ast, NT),
    {[{sgml_breaktag, 1}], Res, 1};
sgml_mutation(Ast, {N, _NT}, 7) ->
    {Res, _, _} = sgml_insert(Ast, N),
    {[{sgml_insert, 1}], Res, 1};
sgml_mutation(Ast, {_N, NT}, 8) ->
    {Res, Meta, D} = sgml_xmlfeatures(Ast, NT, 1), %erlamsa_rnd:erand(2)
    {Meta, Res, D};
sgml_mutation(Ast, {_N, NT}, _R) ->  %% Prob = 25% for inner mutation 
    %%TODO: here we're guessing that mutation was successfull
    %%FIXME: may be count text elements before going to mutate?
    Muta = erlamsa_mutations:mutators_mutator(erlamsa_mutations:inner_mutations()),
    {Res, Meta} = walk2acc(Ast,
                    fun
                        (Elem, Tree, InnerMeta) ->
                            {NewMeta, NewEl} = try_mutate_innertext(Elem, Muta, NT),
                            {[NewEl | Tree], [NewMeta | InnerMeta]} 
                    end, {[], []}),
    {[Meta, {sgml_innertext,1}], Res, 1}.

sgml_mutate(Ll = [H|T], Meta) ->
    %io:format("Trying to parse... ~p~n", [size(H)]),
    %file:write_file("./last_sgml.txt", H),
    try parse(H) of
        {ok, ParsedStr, _, Cnts} ->
            %io:format("Ast ready ~p~n~n", [ParsedStr]),
            {NewMeta, Res, D} = sgml_mutation(ParsedStr, Cnts),
            NewBinStr = fold_ast(Res, []),
            if
                NewBinStr =:= H ->
                    {fun sgml_mutate/2, Ll, NewMeta, -1};
                true ->
                    {fun sgml_mutate/2, [NewBinStr | T],  [NewMeta|Meta], 
                    D + trunc(size(NewBinStr)/(?AVG_BLOCK_SIZE*10))} %% limiting next rounds based on a size
            end
    catch
        incorrect_sgml ->
            {fun sgml_mutate/2, Ll, Meta, -1}
    end.

%%%
%%% /Mutators
%%%


%%%
%%% Test functions
%%%

verify(Str) ->
    BinStr = list_to_binary(Str),
    {ok, ParsedStr, _, _} = parse(BinStr),
    BinStr2 = fold_ast(ParsedStr, []),
    %io:format("~s~n~n~s~n~n", [BinStr, BinStr2]),
    BinStr = BinStr2.

verify_mutation(Str, R) ->
    BinStr = list_to_binary(Str),
    {ok, ParsedStr, _, N} = parse(BinStr),
    %io:format("N                    = ~w~n", [N]),
    {_, Res} = sgml_mutation(ParsedStr, N, R),
    %io:format("!!!!!!!!!!!!!!!>>>>>>>>> ~w", [Res]),
    NewBinStr = fold_ast(Res, []),
    binary_to_list(NewBinStr).

walk_verify(Ast) ->
    lists:reverse(lists:foldl(Ast,
        fun
            Walker({tag, OpenName, CloseName, Params, Internals}, Acc) ->
                [{tag, OpenName, CloseName, Params,
                 lists:reverse(lists:foldl(Walker, [], Internals))}
                    | Acc];
            Walker(Elem, Acc) ->
                [Elem | Acc]
        end,
        [], Ast)).

-ifdef(TEST).

-endif.

%%%
%%% /Test functions
%%%
