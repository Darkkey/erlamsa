%%%-------------------------------------------------------------------
%%% @author dark_k3y
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%% Alignment-based sequence fusing
%%% @end
%%%-------------------------------------------------------------------
-module(fuse).
-author("dark_k3y").

-compile([export_all]).

%% API
-export([]).

%% WARN: This module could be wrongly implemented
%% TODO: ReCheck, ReFactor and Comment

-define(SEARCH_FUEL, 100000).
-define(SEARCH_STOP_IP, 8).

jump(F, F, To) -> 
	To; 
jump([H|T], F, To) -> 
	[H|jump(T, F, To)].

suffixes([]) -> [];
suffixes(Lst = [_H|T]) ->
	[Lst | suffixes(T)].

fix_empty_list([[]]) -> [];
fix_empty_list(L) -> L.

char_suffixes(S) ->
	lists:foldl(
		fun ([H|T], Subs) ->
				%% TODO: ugly, need refactor
				El = fix_empty_list([T|owllisp:get(H, [], Subs)]), 
				gb_trees:enter(H, El, Subs);			
			([], Subs) -> 
				Subs
		end, gb_trees:empty(), S).

any_position_pair(Nodes) ->	
	%io:write({nodes, Nodes}),
	[Froms|Tos] = owllisp:rand_elem(Nodes),	
	%io:write({nodesFromTos, Froms, Tos}),
	From = owllisp:rand_elem(Froms),
	To = owllisp:rand_elem(Tos),
	{From, To}.

%%          .------------------------> source list suffixes after a shared prefix
%%          |               .--------> target -||-
%%          |               v
%%          v
%% node = ((suffix ...) . (suffix ...))
split([H|T], Acc) ->
	Sas = char_suffixes(H),	
	Sbs = char_suffixes(T),		
	lists:foldl(
		fun 
			({_Char,[]}, Tl) ->
				%% WARN: Ugly, but | is not like cons in owllisp
				[[[[]], []]| Tl];
			({Char,Sufs}, Tl) ->
			Bs = owllisp:get(Char, notfound, Sbs),			
			case Bs of
				notfound -> Tl;
				_Else -> [[Sufs|Bs]|Tl]
			end
		end,	
		Acc,  gb_trees:to_list(Sas)).


find_jump_points(A, B) ->
	Al = suffixes(A),		
	Bl = suffixes(B),	
	Nodes = [[Al|Bl]],
	find_jum_points_loop(Nodes, ?SEARCH_FUEL).


%% TODO: WARN: check whether I'm correct
%% in radamsa:
%% (let loop ((rs rs) (nodes nodes) (fuel search-fuel))
%%	(if (< search-fuel 0) ;; <-- but this is a CONSTANT. Bug?
find_jum_points_loop(Nodes, Fuel) when Fuel < 0 ->
	any_position_pair(Nodes);
%% TODO: very UGLY code, need rewrite
find_jum_points_loop(Nodes, Fuel) ->
	X = owllisp:rand(?SEARCH_STOP_IP),
	case X of
		0 -> any_position_pair(Nodes);
		_Else -> 	
			NoDesp = lists:foldl(fun split/2, [], Nodes),
			case NoDesp of
				[] -> any_position_pair(Nodes);
				_Else2 -> find_jum_points_loop(NoDesp, Fuel - length(NoDesp))
			end
		end.

fuse([], Bl) -> Bl;
fuse(Al, []) -> Al;
fuse(Al, Bl) ->
	{A, B} = find_jump_points(Al, Bl),
	jump(Al, A, B).