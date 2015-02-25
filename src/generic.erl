%%%-------------------------------------------------------------------
%%% @author dark_k3y
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(generic).
-author("dark_k3y").

-compile([export_all]).

%% API
-export([]).

-define(STORED_ELEMS, 10).

%% More effective operations for changing at a position of a list
%% for explanaction while we're not using lists:... see 
%% http://stackoverflow.com/questions/4776033/how-to-change-an-element-in-a-list-in-erlang
%% , comment by rvirding. Thank you, rvirding!
%% WARN: it's not tail-recursive, so could consume some memory, 
%% but it's really faster on LONG lists

%% applynth(Index, List, Fun) -> List.
%% Fun (Elem, Rest) -> RestList
applynth(1, [E|Rest], Fun) -> Fun(E, Rest);
applynth(I, [E|Rest], Fun) -> [E|applynth(I-1, Rest, Fun)].

%%
%% Simple Generic Linear Mutations 
%%

%% operatioon(List, Len) -> List
%% Len here for more clear pattern matching and optimizing 
%% (see http://www.erlang.org/doc/efficiency_guide/commoncaveats.html, 3.3)

%% delete a random element
list_del([], 0) -> [];
list_del(L, Len) ->
	P = owllisp:erand(Len),
	applynth(P, L, fun (_, R) -> R end).

%% delete a sequence of things
list_del_seq([], 0) -> [];
list_del_seq(L, Len) ->
	Start = owllisp:erand(Len),
	N = owllisp:erand(Len - Start + 1), % in erlang lists are starting with 1  
	%% TODO: check if sublist is effective
	applynth(Start, L, fun (_, R) -> lists:sublist(R, N, Len) end).

%% duplicate a random element      
list_dup([], 0) -> [];
list_dup(L, Len) -> 
	P = owllisp:erand(Len),
	applynth(P, L, fun (E, R) -> [E,E|R] end).

%% repeat an element
list_repeat([], 0) -> [];
list_repeat(L, Len) -> 
	P = owllisp:erand(Len),
	N = max(2, owllisp:rand_log(10)),
	%% FIXME: don't like ++ here ;(
	applynth(P, L, fun (E, R) -> [E || _X <- lists:seq(1, N)] ++ R end).

%% clone a value to another position
list_clone([], 0) -> [];
list_clone(L, Len) -> 
	From = owllisp:erand(Len),
	To = owllisp:erand(Len),
	Elem = lists:nth(From, L),
	applynth(To, L, fun(E, R) -> [Elem, E | R] end).

%% swap two adjecent values 
list_swap([], 0) -> [];
list_swap(L, Len) when Len < 2 -> L;
list_swap(L, Len) -> 
	P = owllisp:erand(Len - 1),
	applynth(P, L, fun (E, [H|T]) -> [H,E|T] end).

%% permute values
%% FIXME: the behaviour of this function differs from radamsa (list-perm ...)
%% FIXME: should be optimized
list_perm([], 0) -> [];
list_perm(L, Len) when Len < 3 -> L;
list_perm(L, Len) -> 
	From = owllisp:erand(Len - 1), % in erlang lists are starting with 1
	A = owllisp:rand_range(2, Len - From), 
	B = owllisp:rand_log(10),
	N = max(2, min(A, B)),
	io:write({From, A, B, N}),
	applynth(From, L, 
		fun (E, R) -> % WARN: may be ineffective
			{ToPerm, Rest} = lists:split(N - 1, R),
			owllisp:random_permutation([E|ToPerm]) ++ Rest 
		end).

%% mutations which keep some old elements for mutations in (n-elems elem-1 elem2 ...)

update_prob() -> ?STORED_ELEMS bsl 1.

step_state([H|T], L, Len) when H < ?STORED_ELEMS ->
%% add elements until there are enough stored elements
	P = owllisp:erand(Len),
	step_state( 
		[H + 1, lists:nth(P, L) | T]
		, L, Len);
step_state(St, L, Len) -> 
	Up = owllisp:erand(update_prob()),
	case Up < ?STORED_ELEMS of
		true -> 
			Ep = owllisp:erand(Len),
			New = lists:nth(Ep, L),
			applynth(Up + 1, St, fun ([_|T], R) -> [[New | T] | R] end); % +1 for len			
		false -> 
			St
	end.

pick_state([H|T]) ->
	P = owllisp:erand(H),
	lists:nth(P, T).

st_list_mod(St, L, Fun) ->
	N = length(L),
	Stp = step_state(St, L, N),
	X = pick_state(Stp),
	P = owllisp:erand(N),
	Lp = applynth(P, L, Fun(X)),
	{Stp, Lp}.

%% ins 
st_list_ins(St, L) ->
	st_list_mod(St, L, fun(X) -> fun (T, R) -> [X, T | R] end end).

%% replace
st_list_replace(St, L) ->
	st_list_mod(St, L, fun(X) -> fun (_, R) -> [X | R] end end).
 









