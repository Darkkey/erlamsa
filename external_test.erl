-module(external_test).

%% Example custom fuzzer that drops 1st byte for every binary data which length is more than 5 bytes.

-export([capabilities/0, fuzzer/3]).

capabilities() -> {fuzzer, external}.

fuzzer(_Prob, Data, _Opts) ->
	{ok, fuzz(Data, erlamsa_rnd:rand_float())}.

fuzz(Data, _) when byte_size(Data) > 5 -> 
	<<_FirstByte:8, Rest/binary>> = Data,
	Rest;
fuzz(Data, _) -> 
	Data.