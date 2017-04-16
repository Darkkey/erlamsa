-module(external_test).

-export([capabilities/0, mutations/0, post/1, fuzzer/3]).

capabilities() -> {nomutations, fuzzer}.

mutations() -> [].

post(Bin) -> Bin.

fuzzer(_Proto, Data, _Opts) ->
	{ok, fuzz(Data, erlamsa_rnd:rand_float())}.

fuzz(Data, _) -> 
	Data.
