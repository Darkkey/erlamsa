-module(mutations_test).

-include_lib("eunit/include/eunit.hrl").

%%
%% Tests helper functions
%%

init_randr() -> random:seed(now()).

sprintf(Format, Vars) -> 
	lists:flatten(io_lib:format(Format, Vars)).

recursive_tester(_Run, _Check, Max, Max) -> false;
recursive_tester(Run, Check, Max, N) ->
	case Check(Run()) of
		true -> true;
		false -> recursive_tester(Run, Check, Max, N + 1)
	end.

recursive_regex_tester(InStr, Re, Muta, Iters) -> 
	init_randr(),
	TestString = sprintf(InStr, []),
	{ok, MP} = re:compile(Re),	
	recursive_tester(
				fun () -> {_F, _Rs, Ll, _Meta, _D} = Muta(1, [list_to_binary(TestString)], []),	binary_to_list(hd(Ll)) end, 								
				fun (X) -> re:run(X, MP) =/= nomatch end,
				Iters,
				0
			).

%% 
%% Number mutation test
%%

sed_num_test() ->
	?assert(recursive_regex_tester(
		" 100 + 100 + 100 ", "101", fun mutations:sed_num/3, 1500
		) =:= true). 

%% 
%% ASCII bad mutators test
%% 

ascii_bad_test() ->	
	?assert(recursive_regex_tester(
		"----------------------------------------\"\"--------------------------------------------------",
		"^-*\".*[%|a].*\"-*$", mutations:construct_ascii_bad_mutator(), 20
		) =:= true). 

ascii_delimeter_test() ->	
	?assert(recursive_regex_tester(
		"----------------------------------------\"\"--------------------------------------------------",
		"^-*\"-*$", mutations:construct_ascii_delimeter_mutator(), 20
		) =:= true). 
	
%%
%% Line mutations tests
%%

line_muta_tester(InStr, MutaFun, Check) ->
	init_randr(),
	TestString = sprintf(InStr, []),
	Muta = mutations:construct_line_muta(MutaFun, temp),
	{_F, _Rs, Ll, _Meta, _D} = Muta(1, [list_to_binary(TestString)], []),
	Check(TestString, binary_to_list(hd(Ll))).


line_del_test() -> 
	?assert(line_muta_tester("1~n 2~n  3~n  4~n    5~n", fun generic:list_del/2,
			fun (S, R) -> length(string:tokens(R,[10])) + 1 =:= length(string:tokens(S, [10])) end)).

line_del_seq_statistics_test() -> 
	init_randr(),
	Iters = 1000,
	TestString = sprintf("0~n1~n 2~n  3~n   4~n    5~n     6~n      7~n       8~n         9~n", []),
	Muta = mutations:construct_line_muta(fun generic:list_del_seq/2, line_del_seq),
	N = lists:foldl(
		fun (_, AccIn) -> 
			{_, _, Ll, _, _} = Muta(1, [list_to_binary(TestString)], []),
			AccIn + length(string:tokens(binary_to_list(hd(Ll)),[10])) end, 
			0, lists:seq(1, Iters)),		
	?assert((N*1.0)/Iters < (0.75 * length(string:tokens(TestString, [10])))). %% should be around 50% of original length

line_dup_test() -> 
	?assert(line_muta_tester("1~n", fun generic:list_dup/2,
			fun (S, R) -> R =:= S ++ S end)).

line_clone_test() -> 
	?assert(line_muta_tester("1~n2~n", fun generic:list_clone/2,
			fun (S, R) -> 
				(R =:= S) or
				(R =:= sprintf("1~n1~n", [])) or
				(R =:= sprintf("2~n2~n", []))
			 end)).

line_repeat_test() -> 
	?assert(line_muta_tester("1~n 2~n  3~n  4~n    5~n", fun generic:list_repeat/2,
			fun (S, R) -> 
				length(string:tokens(R, [10])) > length(string:tokens(S, [10]))
			 end)).

line_swap_length_test() -> 
	?assert(line_muta_tester("1~n 2~n  3~n  4~n    5~n", fun generic:list_swap/2,
			fun (S, R) -> 
				length(string:tokens(R, [10])) =:= length(string:tokens(S, [10]))
			 end)).

line_swap_correct_test() -> 
	?assert(line_muta_tester("A~n B~n", fun generic:list_swap/2,
			fun (_S, R) -> 
				R =:= sprintf(" B~nA~n", [])
			 end)).

line_perm_length_test() -> 
	?assert(line_muta_tester("1~n 2~n  3~n  4~n    5~n", fun generic:list_perm/2,
			fun (S, R) -> 
				length(string:tokens(R, [10])) =:= length(string:tokens(S, [10]))
			 end)).

