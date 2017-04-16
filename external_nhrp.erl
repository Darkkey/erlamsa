-module(external_nhrp).

-export([capabilities/0, mutations/0, post/1, fuzzer/3]).

capabilities() -> {nomutations, fuzzer}.

mutations() -> [].

post(Bin) -> fix_checksum(Bin).

fuzzer(_Proto, Data, _Opts) ->
	{ok, Data}.

fix_checksum(<<HSRP:32, Hdr:96, _ChkSum:16, Rest/binary>>) ->
	StubPacket = <<Hdr:96, 0, 0, Rest/binary>>,
	NewChkSum = packet:makesum(StubPacket),
	<<HSRP:32, Hdr:96, NewChkSum:16, Rest/binary>>;
fix_checksum(Bin) ->
	Bin.
