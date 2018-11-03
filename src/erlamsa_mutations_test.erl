-module(erlamsa_mutations_test).

-include_lib("eunit/include/eunit.hrl").
-include("erlamsa.hrl").

%%
%% TODO: not covered by tests:
%% - utf9 (uw, ui): sed_utf8_widen, sed_utf8_insert
%% - st_list_replace (srs)
%% - fuses (fn, fo): sed_fuse_next, sed_fuse_old
%%

%%
%% Tests helper functions
%%

init_randr() -> erlamsa_rnd:seed(now()).

init_randr(Seed) -> erlamsa_rnd:seed(Seed).

sprintf(Format, Vars) -> 
	lists:flatten(io_lib:format(Format, Vars)).

%% Test until N will be eq to Max
recursive_tester(_Run, _CheckBug, Max, Max) -> false;
recursive_tester(Run, CheckBug, Max, N) ->
	case CheckBug(Run()) of
		true -> true;
		false -> recursive_tester(Run, CheckBug, Max, N + 1)
	end.

%% Test until N will be eq to Max OR at least one fail
recursive_fail_tester(_Run, _CheckBug, Max, Max) -> true;
recursive_fail_tester(Run, CheckBug, Max, N) ->
	case CheckBug(Run()) of
		false -> false;
		true -> recursive_tester(Run, CheckBug, Max, N + 1)
	end.

recursive_regex_tester(InStr, Re, Muta, Iters) -> 
	init_randr(),
	TestString = sprintf(InStr, []),
	{ok, MP} = re:compile(Re),	
	recursive_tester(
				fun () -> {_F, Ll, _Meta, _D} = Muta([list_to_binary(TestString)], []),	binary_to_list(hd(Ll)) end, 								
				fun (X) -> re:run(X, MP) =/= nomatch end,
				Iters,
				0
			).

random_lex_string() -> init_randr(), random_lex_string(erlamsa_rnd:rand(42), []).

random_lex_string(0, Out) -> Out;
random_lex_string(N, Out) -> 
	T = erlamsa_rnd:rand(8),
	case T of 
		0 -> random_lex_string(N - 1, [92 | Out]); % \
		1 -> random_lex_string(N - 1, [34 | Out]); % "
		2 -> random_lex_string(N - 1, [39 | Out]); % '
		3 -> random_lex_string(N - 1, [0 | Out]);  % \0
		4 -> random_lex_string(N - 1, [erlamsa_rnd:rand(256) | Out]);
		_Else -> random_lex_string(N - 1, [97 | Out]) % a
	end.

warn_false(true, _Fmt, _Lst) -> true;
warn_false(false, Fmt, Lst) ->
	?debugFmt(Fmt, Lst),
	false.

%% 
%% Number mutation test
%%

sed_num_test() ->
	?assert(recursive_regex_tester(
		" 100 + 100 + 100 ", "101", fun erlamsa_mutations:sed_num/2, 1500
		) =:= true). 

%% 
%% ASCII bad mutators test
%% 


string_lexer_test() -> ?assert(string_lexer_test(0, [233, 39, 39, 97, 97, 97, 0])).

string_lexer_test(10000, _Input) -> true;
string_lexer_test(N, Input) ->
	Chunks = erlamsa_strlex:lex(Input),
	Output = erlamsa_strlex:unlex(Chunks),
	case Output =:= Input of
		true -> string_lexer_test(N + 1, random_lex_string());
		false -> ?debugFmt("Lex/unlex fail onto: ~s =/= ~s from ~w~n", [Input, Output, {Input, Chunks, Output}]), false
	end.


ascii_bad_test() ->	
	?assert(recursive_regex_tester(
		"----------------------------------------\"\"--------------------------------------------------",
		"^-*\".*[%|a].*\"-*$", erlamsa_mutations:construct_ascii_bad_mutator(), 50
		) =:= true). 

ascii_delimeter_test() ->	
	ets:new(global_config, [public, named_table, bag]),
	ets:insert(global_config, [{cm_port, 12345}]),
    ets:insert(global_config, [{cm_host, {127,0,0,1}}]),
	?assert(recursive_regex_tester(
		"----------------------------------------\"\"--------------------------------------------------",
		"^-*\"-*$", erlamsa_mutations:construct_ascii_delimeter_mutator(), 50
		) =:= true). 
	
%%
%% Fuse mutator test
%%

sed_fuse_this_test() ->	
	?assert(recursive_regex_tester(
		"kittenslartibartfasterthaneelslartibartfastenyourseatbelts",
		"kittenslartibartfastenyourseatbelts", fun erlamsa_mutations:sed_fuse_this/2, 500
		) =:= true). 

%%
%% Tree mutators test
%%


sed_tree_stutter_test() ->	
	?assert(recursive_regex_tester(
		"(x (Y x))",
		"\\(x \\(x \\(x \\(x \\(Y x\\)\\)\\)\\)\\)", fun erlamsa_mutations:sed_tree_stutter/2, 500
		) =:= true). 


sed_tree_count_tester(TestString, Cnt, Swaps, Muta) ->
	init_randr(),
	Lst = lists:foldl(
		fun (_X, Acc) -> 
			{_F, Ll, _Meta, _D} = Muta([list_to_binary(TestString)], []),
			[binary_to_list(hd(Ll)) | Acc]
		end,
		[],
		lists:seq(1, Cnt)),
	ULst = lists:usort(fun(A, B) -> A =< B end, Lst),
	length(ULst) =:= Swaps.

sed_tree_dup_test() ->
	?assert(sed_tree_count_tester("(a) (b)", 30, 2, erlamsa_mutations:sed_tree_dup())).
	
sed_tree_swap_one_test() ->
	?assert(sed_tree_count_tester("A (a) (b) (c) B", 400, 6, erlamsa_mutations:construct_sed_tree_swap(fun erlamsa_mutations:sed_tree_swap_one/2, tree_swap_one))).

sed_tree_swap_two_test() ->
	?assert(sed_tree_count_tester("(a) (b (c))", 30, 3, erlamsa_mutations:construct_sed_tree_swap(fun erlamsa_mutations:sed_tree_swap_two/2, tree_swap_two))).


%%
%% Line mutations tests
%%

line_muta_tester(InStr, MutaFun, Check) ->
	init_randr(),
	TestString = sprintf(InStr, []),
	Muta = erlamsa_mutations:construct_line_muta(MutaFun, temp),
	{_F, Ll, _Meta, _D} = Muta([list_to_binary(TestString)], []),
	Check(TestString, binary_to_list(hd(Ll))).


line_del_test() -> 
	?assert(line_muta_tester("1~n 2~n  3~n  4~n    5~n", fun erlamsa_generic:list_del/2,
			fun (S, R) -> length(string:tokens(R,[10])) + 1 =:= length(string:tokens(S, [10])) end)).

line_del_seq_statistics_test() -> 
	init_randr(),
	Iters = 1000,
	TestString = sprintf("0~n1~n 2~n  3~n   4~n    5~n     6~n      7~n       8~n         9~n", []),
	Muta = erlamsa_mutations:construct_line_muta(fun erlamsa_generic:list_del_seq/2, line_del_seq),
	N = lists:foldl(
		fun (_, AccIn) -> 
			{_, Ll, _, _} = Muta([list_to_binary(TestString)], []),
			AccIn + length(string:tokens(binary_to_list(hd(Ll)),[10])) end, 
			0, lists:seq(1, Iters)),		
	?assert((N*1.0)/Iters < (0.75 * length(string:tokens(TestString, [10])))). %% should be around 50% of original length

line_dup_test() -> 
	?assert(line_muta_tester("1~n", fun erlamsa_generic:list_dup/2,
			fun (S, R) -> R =:= S ++ S end)).

line_clone_test() -> 
	?assert(line_muta_tester("1~n2~n", fun erlamsa_generic:list_clone/2,
			fun (S, R) -> 
				(R =:= S) or
				(R =:= sprintf("1~n1~n", [])) or
				(R =:= sprintf("2~n2~n", []))
			 end)).

line_repeat_test() -> 
	?assert(line_muta_tester("1~n 2~n  3~n  4~n    5~n", fun erlamsa_generic:list_repeat/2,
			fun (S, R) -> 
				length(string:tokens(R, [10])) > length(string:tokens(S, [10]))
			 end)).

line_swap_length_test() -> 
	?assert(line_muta_tester("1~n 2~n  3~n  4~n    5~n", fun erlamsa_generic:list_swap/2,
			fun (S, R) -> 
				length(string:tokens(R, [10])) =:= length(string:tokens(S, [10]))
			 end)).

line_swap_correct_test() -> 
	?assert(line_muta_tester("A~n B~n", fun erlamsa_generic:list_swap/2,
			fun (_S, R) -> 
				R =:= sprintf(" B~nA~n", [])
			 end)).

line_perm_length_test() -> 
	?assert(line_muta_tester("1~n 2~n  3~n  4~n    5~n", fun erlamsa_generic:list_perm/2,
			fun (S, R) -> 
				length(string:tokens(R, [10])) =:= length(string:tokens(S, [10]))
			 end)).

%%
%% Line-string mutations tests
%%

st_line_ins_test() ->
	init_randr({1,2,3}),
	TestString = "ABC DEF", 
	Muta = erlamsa_mutations:construct_st_line_muta(fun erlamsa_generic:st_list_ins/2, list_ins, [0]),
	{_F, Ll, _Meta, _D} = Muta([list_to_binary(TestString)], []),
	RStr = binary_to_list(hd(Ll)),
	{RLst1, RLst2} = lists:split(length(RStr) div 2, RStr),
	?assert(RLst1 =:= RLst2).

%%
%% Byte-level mutations test
%%

bytes_sum(<<B:8>>, N) -> N + B;
bytes_sum(<<B:8, T/binary>>, N) -> bytes_sum(T, N + B).

sed_byte_muta_tester(InStr, MutaFun, Check, Tries) ->
	init_randr(),
	?assert(recursive_fail_tester(
		fun () ->			
			{_F, Ll, _Meta, _D} = MutaFun([InStr], []),
			warn_false(Check(InStr, hd(Ll)), "Failed string: ~w~n", [{InStr, hd(Ll)}])
		end, fun (X) -> X end, Tries, 0)).

sed_byte_drop_test() ->
	sed_byte_muta_tester(
		erlamsa_rnd:random_block(erlamsa_rnd:erand(?MAX_BLOCK_SIZE)), %
		erlamsa_mutations:construct_sed_byte_drop(),
		fun (X, Y) -> size(X) - 1 =:= size(Y) end, 1000).

sed_byte_insert_test() ->
	sed_byte_muta_tester(		
		erlamsa_rnd:random_block(erlamsa_rnd:erand(?MAX_BLOCK_SIZE)), 
		erlamsa_mutations:construct_sed_byte_insert(),
		fun (X, Y) -> size(X) + 1 =:= size(Y) end, 1000).

sed_byte_repeat_test() ->
	sed_byte_muta_tester(		
		<<1>>, 
		erlamsa_mutations:construct_sed_byte_repeat(),
		fun (_X, Y) -> Y =:= <<1,1>> end, 1000).

sed_byte_flip_length_test() ->
	sed_byte_muta_tester(		
		erlamsa_rnd:random_block(erlamsa_rnd:erand(?MAX_BLOCK_SIZE)), 
		erlamsa_mutations:construct_sed_byte_flip(),
		fun (X, Y) -> size(X) =:= size(Y) end, 1000).

sed_byte_inc_test() ->
	sed_byte_muta_tester(		
		erlamsa_rnd:random_block(erlamsa_rnd:erand(?MAX_BLOCK_SIZE)), 
		erlamsa_mutations:construct_sed_byte_inc(),
		fun (X, Y) -> 
			B1 = bytes_sum(X, 0), B2 = bytes_sum(Y, 0),
			(B1 + 1 =:= B2) or (B1 - 255 =:= B2) end, 1000).

sed_byte_dec_test() ->
	sed_byte_muta_tester(		
		erlamsa_rnd:random_block(erlamsa_rnd:erand(?MAX_BLOCK_SIZE)), 
		erlamsa_mutations:construct_sed_byte_dec(),
		fun (X, Y) -> 
			B1 = bytes_sum(X, 0), B2 = bytes_sum(Y, 0),
			(B1 - 1 =:= B2) or (B1 + 255 =:= B2) end, 1000).

sed_byte_random_length_test() ->
	sed_byte_muta_tester(		
		erlamsa_rnd:random_block(erlamsa_rnd:erand(?MAX_BLOCK_SIZE)), 
		erlamsa_mutations:construct_sed_byte_random(),
		fun (X, Y) -> size(X) =:= size(Y) end, 1000).


sed_bytes_perm_length_test() ->
	sed_byte_muta_tester(		
		erlamsa_rnd:random_block(erlamsa_rnd:erand(?MAX_BLOCK_SIZE)), 
		erlamsa_mutations:construct_sed_bytes_perm(),
		fun (X, Y) -> size(X) =:= size(Y) end, 1000).

sed_bytes_drop_length_test() ->
	sed_byte_muta_tester(		
		erlamsa_rnd:random_block(erlamsa_rnd:erand(?MAX_BLOCK_SIZE)), 
		erlamsa_mutations:construct_sed_bytes_drop(),
		fun (X, Y) -> size(X) > size(Y) end, 1000).

sed_bytes_repeat_length_test() ->
	sed_byte_muta_tester(		
		erlamsa_rnd:random_block(erlamsa_rnd:erand(?MAX_BLOCK_SIZE)), 
		erlamsa_mutations:construct_sed_bytes_repeat(),
		fun (X, Y) -> size(X) < size(Y) end, 1000).

