-module(external_muta).

%% Example custom mutator that just puts fuzzed term inside pancake "tag"

-export([capabilities/0, mutations/0]).

capabilities() -> {mutations, external}.

mutations() -> 
	MaxScore = erlamsa_mutations:get_max_score(),
	%% maximum score, default pri, functions, short name, description
	[{MaxScore, 2, fun make_pancake/2, pan, "MORE PANCAKES!!!"}].

pancaked(Bin) ->
	list_to_binary([<<"<pancaked>">>, Bin, <<"</pancaked>">>]).

make_pancake([H|T], Meta) ->
	MutatedH = pancaked(H),
	Result = erlamsa_utils:flush_bvecs(MutatedH, T),
	{fun make_pancake/2, Result, [{muta_pancake, 1}|Meta], -1}.

