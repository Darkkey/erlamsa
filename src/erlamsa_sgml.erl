% Copyright (c) 2014-2015 Alexander Bolshev aka dark_k3y
% The following SGML parser tokenizer is hardly based on the Mats Cronqvist 
% "trane" html parser: https://github.com/massemanet/trane

% Copyright (c) 2010 Mats Cronqvist

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

-define(ok(X),$a=<X,X=<$z;$A=<X,X=<$Z;$0=<X,X=<$9;X==$_;X==$-;X==$:).
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

tz({tag,""},?m("!--",Str))                -> {{comm,""},Str};
tz({tag,""},?m("!",Str))                  -> {{'!',""},ws(Str)};
tz({tag,""},?m("?",Str))                  -> {{que,""},ws(Str)};
tz({tag,""},?m("/",Str))                  -> {{end_tag,""},ws(Str)};
tz({tag,Tag},?m("/>",Str))                -> {text,Str,{sc,Tag,[]}};
tz({tag,Tag},?D(X,S)) when ?ev(X)         -> {{attr,"",{Tag,[]}},ws(S)};
tz({tag,Tag},?d(X,Str)) when ?ok(X)       -> {{tag,Tag++[X]},Str};

tz({'!',DT},?m(">",Str))                  -> {text,Str,{'!',DT}};
tz({'!',DT},?d(X,Str))                    -> {{'!',DT++[X]},Str};

tz({que,DT},?m("?>",Str))                 -> {text,Str,{'?',DT}};
tz({que,DT},?d(X,Str))                    -> {{que,DT++[X]},Str};

tz({comm,Comm},?m("-->",Str))             -> {text,Str,{comment,Comm}};
tz({comm,Comm},?d(X,Str))                 -> {{comm,Comm++[X]},Str};

tz({etag,Tag,Attrs},?m("/>",Str))         -> {text,Str,{sc,Tag,Attrs}};
tz({etag,Tag,Attrs},?m(">",Str))          -> {text,Str,{open,Tag,Attrs}};

tz({end_tag,Tag},?D(X,S)) when ?ev(X)     -> {{end_tag,Tag,'>'},ws(S)};
tz({end_tag,Tag,'>'},?m(">",Str))         -> {text,Str,{close,Tag,string:to_lower(Tag)}};
tz({end_tag,Tag},?d(X,Str))               -> {{end_tag,Tag++[X]},Str};

tz({attr,"",{Tag,As}},?D(X,S))when ?ev(X) -> {{etag,Tag,As},S};
tz({attr,"",{Tag,As}},?M("/>",S))         -> {{etag,Tag,As},S};
tz({attr,A,{T,As}},?D(X,S))when ?ev(X)    -> {{eatt,{A,T,As}},ws(S)};
tz({attr,A,{T,As}},?M("/>",S))            -> {{eatt,{A,T,As}},ws(S)};
tz({attr,A,TAs},?d(X,Str))when ?ok(X)     -> {{attr,A++[X],TAs},Str};

tz({eatt,ATAs},?m("=",Str))               -> {{val,ATAs},ws(Str)};
tz({eatt,{A,T,As}},S)                     -> {{attr,"",{T,As++[{A,"",[]}]}},ws(S)};

tz({val,ATAs},?m("'",Str))                -> {{sqval,"",ATAs},Str}; % singlequoted
tz({val,ATAs},?m("\"",Str))               -> {{dqval,"",ATAs},Str}; % doublequoted
tz({val,ATAs},Str)                        -> {{uqval,"",ATAs},Str}; % unquoted

tz({sqval,V,{A,T,As}},?m("'",S))          -> {{attr,"",{T,As++[{A,V,"'"}]}},ws(S)};
tz({sqval,V,ATAs},?d(X,Str))              -> {{sqval,V++[X],ATAs},Str};

tz({dqval,V,{A,T,As}},?m("\"",S))         -> {{attr,"",{T,As++[{A,V,"\""}]}},ws(S)};
tz({dqval,V,ATAs},?d(X,Str))              -> {{dqval,V++[X],ATAs},Str};

tz({uqval,V,{A,T,As}},?D(X,S))when ?ev(X) -> {{attr,"",{T,As++[{A,V,[]}]}},ws(S)};
tz({uqval,V,{A,T,As}},?M("/>",S))         -> {{attr,"",{T,As++[{A,V,[]}]}},ws(S)};
tz({uqval,V,ATAs},?d(X,Str))              -> {{uqval,V++[X],ATAs},Str};

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
	Tokens = tokenize(Str),
	%io:format("~w~n", [Tokens]),
	build_ast2(Tokens, [], [], {0, 0}).

% list(token()), list(ast_elem()), list(strings) -> atom(), list(ast_elem()), list(ast_elem(), {non_neg_int(), non_neg_int()})
% non_neg_int() is tag (paired)
build_ast2([{open, Tag, Params}|T], Ast, Tags, {N, NT}) ->
	%io:format("Open ~s, ~w, ~w~n", [Tag, Ast, Tags]),
	Test = build_ast2(T, [], [string:to_lower(Tag) | Tags], {0, 0}),
	%io:format("!!!Open ~s, ~w, ~w~n", [Tag, Ast, Tags]),	
	case Test of 
		{ok, [{tagclose, TagClose} | TagInternals], TagOuters, {K, KT}} ->
			%io:format("open_ok for ~w: ~w, ~w, ~w, +1~n", [Tag, TagInternals, TagOuters, Tags]),	
			build_ast2(TagOuters, [{tag, Tag, TagClose, Params, TagInternals}|Ast], Tags, {N + K + 1, NT + KT + 1});
		{error, no_pair_tag, Tokens, NewAst, [Tag], {K, KT}} -> 
			%io:format("Last tag ~w, ~w, ~s~n", [Tokens, NewAst, Tag]),			
			%io:format("Go d33per ~w, ~w, ~w, ~w, ~w~n", [Tokens, Ast, NewAst, [{open, Tag, Params}], Tag]),						
			build_ast2(Tokens, NewAst ++ [{open, Tag, Params}] ++ Ast , [], {N + K + 1, NT + KT});	
		{error, no_pair_tag, Tokens, NewAst, NewTags, {K, KT}} -> 
			%io:format("No_pair tag ~w, ~w, ~w~n", [Tokens, NewAst, NewTags]),			
			%io:format("Go d33per ~w, ~w, ~w, ~w, ~w~n", [Tokens, Ast, NewAst, [{open, Tag, Params}], Tag]),						
			build_ast2(Tokens, NewAst ++ [{open, Tag, Params}] ++ Ast , NewTags, {N + K + 1, NT + KT});
		{error, closed_earlier, {Tag, CloseTag}, Tokens, NewAst, NewTags, {K, KT}} -> 
			%io:format("Err_close_earlier ~w, ~w, ~w, +1~n", [Tokens, NewAst, NewTags]),			
			build_ast2(Tokens, [{tag, Tag, CloseTag, Params, lists:reverse(NewAst)}|Ast], NewTags, {N + K + 1, NT + KT + 1});
		{error, closed_earlier, {NewTag, NewCloseTag}, Tokens, NewAst, NewTags, {K, KT}} -> 
			%io:format("Err_close_earlier upper pass ~w, ~w, ~w~n", [Tokens, NewAst, NewTags]),			
			{error, closed_earlier, {NewTag, NewCloseTag}, Tokens, NewAst ++ [{open, Tag, Params}] ++ Ast, NewTags, {N + K + 1, NT + KT}}			
	end;
build_ast2([{close, Tag, TagName}|T], Ast, [TagName|_Tags], {N, NT}) ->
	%io:format("Close ~s, ~w, ~w~n", [Tag, Ast, [TagName|_Tags]]),
	{ok, [{tagclose, Tag} | lists:reverse(Ast)], T, {N, NT}};
build_ast2([{close, Tag, TagName}|T], Ast, [_OtherTag|Tags] = AllTags, {N, NT}) ->
	%io:format("Close no pair ~s, ~w, ~w~n", [Tag, Ast, AllTags]),
	case lists:member(TagName, Tags) of
		false -> 
			%io:format("Close invalid ~s, +1~n", [Tag]),
			build_ast2(T, [{close, Tag} | Ast], AllTags, {N + 1, NT});
		true ->
			%io:format("Close earlier ~s~n", [Tag]),			
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
build_ast2([{sc, Text, Params}|T], Ast, Tags, {N, NT}) ->
	%io:format("SingleTag ~w, ~w, ~w, +1~n", [Text, Ast, Tags]),
	build_ast2(T, [{sc, Text, Params}|Ast], Tags, {N + 1, NT});		
build_ast2([{eof, {text, <<>>}}|_T], Ast, [], {N, NT}) ->
	%io:format("Eof empty_text ~w, ~w, Tags: ~w~n", [<<>>, Ast, []]),
	{ok, lists:reverse(Ast), [], {N, NT}};
build_ast2([{eof, {text, Text}}|_T], Ast, [], {N, NT}) ->
	%io:format("Eof ~w, ~w, Tags: ~w, +1~n", [Text, Ast, []]),
	{ok, lists:reverse([{text, Text}|Ast]), [], {N + 1, NT}};
build_ast2([{eof, {text, Text}}|_T] = Tokens, Ast, [Tag|Tags], {N, NT}) ->
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
	fold_params(T, [[<<" ">>, list_to_binary(ParamName), <<"=">>, enclose_param_value(Type, Value)] | Acc]);
fold_params([], Acc) ->
	lists:reverse(Acc).

fold_ast([{tag, OpenName, CloseName, Params, Internals} | T], Acc) -> 
	fold_ast(T, [list_to_binary([<<"<">>, OpenName, fold_params(Params, []), <<">">>, fold_ast(Internals, []), <<"</">>, CloseName, <<">">>]) | Acc]);
fold_ast([{text, Text}| T], Acc) -> 
	fold_ast(T, [Text | Acc]);
fold_ast([{sc, Name, Params}| T], Acc) -> 
	fold_ast(T, [list_to_binary([<<"<">>, Name, fold_params(Params, []), <<" />">>]) | Acc]);
fold_ast([{'!', Params}| T], Acc) -> 
	fold_ast(T, [list_to_binary([<<"<">>, <<"!">>, Params, <<">">>])
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
				{ChildAcc, ChildTagCnt, ChildCount} = lists:foldl(Walker, {InitAcc, TagCnt + 1, Count + 1}, Internals),
				{Fun({tag, OpenName, CloseName, Params, walk_reverse(ChildAcc)}, Acc, TagCnt + 1, Count + 1), ChildTagCnt, ChildCount}; 
			Walker(Elem, {Acc, TagCnt, Count}) -> 
				%io:format("~w is ~w (if tag then ~w)~n", [Elem, Count + 1, TagCnt]),	
				{Fun(Elem, Acc, TagCnt, Count + 1), TagCnt, Count + 1}
		end,
		{InitAcc, 0, 0}, Ast),
	{walk_reverse(Res), ResT, ResN}.

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
		fun  (E, Acc, T, N) -> 
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
	PumpCnt = erlamsa_rnd:erand(10),
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

sgml_mutation(Ast, {N, NT}) -> 
	% io:format("~n~n~w~n~n", [Ast]),
	sgml_mutation(Ast, {N, NT}, erlamsa_rnd:rand(8)).		

sgml_mutation(Ast, {N, _NT}, 0) -> 
	{Res, _, _} = sgml_swap(Ast, N),
	{sgml_swap, Res};
sgml_mutation(Ast, {N, _NT}, 1) -> 
	{Res, _, _} = sgml_dup(Ast, N),
	{sgml_dup, Res};
sgml_mutation(Ast, {_N, NT}, 2) -> 
	{Res, _, _} = sgml_pump(Ast, NT),
	{sgml_pump, Res};
sgml_mutation(Ast, {N, _NT}, 3) -> 
	{Res, _, _} = sgml_repeat(Ast, N),
	{sgml_repeat, Res};
sgml_mutation(Ast, {N, _NT}, 4) -> 
	{Res, _, _} = sgml_insert2(Ast, N),
	{sgml_insert2, Res};
sgml_mutation(Ast, {_N, NT}, 5) -> 
	{Res, _, _} = sgml_permparams(Ast, NT),
	{sgml_permparams, Res};
sgml_mutation(Ast, {_N, NT}, 6) -> 
	{Res, _, _} = sgml_breaktag(Ast, NT),
	{sgml_breaktag, Res};		
sgml_mutation(Ast, {N, _NT}, _R) -> 
	{Res, _, _} = sgml_insert(Ast, N),
	{sgml_insert, Res}.

sgml_mutate(Ll = [H|T], Meta) ->
	io:format("Trying to parse... ~w~n~n", [H]),
	{ok, ParsedStr, _, Cnts} = parse(H),
    {Name, Res} = sgml_mutation(ParsedStr, Cnts),    
    NewBinStr = fold_ast(Res, []),
    if
        NewBinStr =:= H ->
            {fun sgml_mutate/2, Ll, Meta, -1};
        true ->
        	{fun sgml_mutate/2, [NewBinStr | T], [{Name, 1} | Meta], 1}
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
	BinStr = fold_ast(ParsedStr, []).

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
				[{tag, OpenName, CloseName, Params, lists:reverse(lists:foldl(Walker, [], Internals))} | Acc];
			Walker(Elem, Acc) -> 
				[Elem | Acc]			
		end,
		[], Ast)).

-ifdef(TEST).

-endif.

%%% 
%%% /Test functions
%%%
	
% build_ast2([{open, Tag, Params}|T], Ast, Tags) ->
% 	io:format("Open ~s, ~w, ~w~n", [Tag, Ast, Tags]),
% 	case build_ast2(T, [], [string:to_lower(Tag) | Tags]) of
% 		{ok, TagInternals, TagOuters} ->
% 			build_ast2(TagOuters, [{tag, Tag, Tag, Params, TagInternals}|Ast], Tags);
% 		{error, no_pair_tag} -> 
% 			build_ast2(T, [{open, Tag, Params}|Ast], Tags)
% 	end;
% build_ast2([{close, Tag, TagName}|T], Ast, [TagName|_Tags]) ->
% 	io:format("Close ~s, ~w, ~w~n", [Tag, Ast, [TagName|_Tags]]),
% 	{ok, lists:reverse([{tagclose, Tag} | Ast]), T};
% build_ast2([{close, Tag, TagName}|T], Ast, [_OtherTag|Tags] = AllTags) ->
% 	io:format("Close no pair~s, ~w, ~w~n", [Tag, Ast, AllTags]),
% 	case lists:member(Tag, Tags) of
% 		false -> 
% 			build_ast2(T, [{close, Tag} | Ast], AllTags);
% 		true ->
% 			{error, no_pair_tag}  
% 	end;
% build_ast2([{close, Tag, TagName}|T], Ast, []) ->
% 	io:format("Close no pair empty ~s, ~w, ~w~n", [Tag, Ast, []]),
% 	build_ast2(T, [{close, Tag} | Ast], []);	
% build_ast2([{text, Text}|T], Ast, Tags) ->
% 	io:format("Text ~w, ~w, ~w~n", [Text, Ast, Tags]),
% 	build_ast2(T, [{text, Text}|Ast], Tags);
% build_ast2([{eof, {text, Text}}|_T], Ast, []) ->
% 	io:format("Eof ~w, ~w, ~w~n", [Text, Ast, []]),
% 	{ok, lists:reverse([{text, Text}|Ast]), []};
% build_ast2([{eof, {text, Text}}|_T], Ast, Tags) ->
% 	io:format("Eof no_pair ~w, ~w, ~w~n", [Text, Ast, Tags]),
% 	{error, no_pair_tag}.

%% list(token()), list(ast_elem()), list(strings) -> list(ast_elem()), list(ast_elem()), atom()
% build_ast([{open, Tag, Params}|T], Ast, Tags) ->
% 	io:format("o!!!!!! ~s ~w ~n", [Tag, Tag]),
% 	case build_ast(T, [], [string:to_lower(Tag) | Tags]) of
% 		{ok, [{tagclose, TagClose}|TagInternals], TagOuters} ->
% 			io:format("k!!!!!! ~w~n", [Tags]),
% 			build_ast(TagOuters, [{tag, Tag, TagClose, Params, TagInternals}|Ast], Tags);	
% 		{ok, TagInternals, TagOuters} ->
% 			io:format("kN!!!!!! ~w~n", [{TagInternals, TagOuters, Tags}]),
% 			build_ast(TagOuters, [{tag, Tag, Tag, Params, TagInternals}|Ast], Tags);				
% 			%%{ok, [{open, Tag, Params} | TagInternals] ++ Ast, TagOuters};
% 		{brokenseq, TagInternals, TagOuters} ->
% 			io:format("b!!!!!! ~w~n", [Tags]),
% 			{ok, [{open, Tag, Params} | lists:reverse(TagInternals)] ++ Ast, TagOuters}
% 	end;
% build_ast([{text, Text}|T], Ast, Tags) ->
% 	io:format("ct!!!!!!! ~w~n", [Tags]),
% 	build_ast(T, [{text, Text}|Ast], Tags);
% build_ast([{'!', Params}|T], Ast, Tags) ->
% 	io:format("cD!!!!!!! ~w~n", [Tags]),
% 	build_ast(T, [{'!', Params}|Ast], Tags);	
% build_ast([{sc, Text, Params}|T], Ast, Tags) ->
% 	io:format("cs!!!!!!! ~w~n", [Tags]),
% 	build_ast(T, [{sc, Text, Params}|Ast], Tags);	
% build_ast([{close, Tag, TagName}|T], Ast, [TagName|_Tags]) ->
% 	io:format("cc!!!!!!! ~s~n", [Tag]),
% 	{ok, [{tagclose, Tag} | lists:reverse(Ast)], T};
% build_ast([{close, Tag, _TagName}|T], Ast, [NonMatchedTag|Tags] = AllTags) ->
% 	io:format("cI!!!!!!! ~w~n", [{Tag, [NonMatchedTag|Tags]}]),
% 	case lists:member(Tag, Tags) of
% 		false -> 
% 			io:format("cu!!!!!!! ~w~n", [{Tag, NonMatchedTag}]),
% 			build_ast(T, [{close, Tag} | Ast], AllTags);
% 		true ->
% 			io:format("cb!!!!!!! ~w~n", [{Tag, NonMatchedTag}]),
% 			{brokenseq, [{tagclose, Tag} | Ast], T}  
% 	end;
% build_ast([{eof, {text, Text}}|_T], Ast, []) ->
% 	{ok, lists:reverse([{text, Text}|Ast]), []};
% build_ast([{eof, {text, Text}}|_T], Ast, _Tags) ->
% 	io:format("cE!!!!!!! ~w~n", [_Tags]),
% 	{brokenseq, lists:reverse([{text, Text}|Ast]), []};
% build_ast([], Ast, []) ->
% 	{ok, [], Ast};
% build_ast([], Ast, _Tags) ->
% 	io:format("c !!!!!!! ~w~n", [_Tags]),
% 	{brokenseq, [], Ast}.

