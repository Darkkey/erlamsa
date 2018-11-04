% Copyright (c) 2014-2018 Alexander Bolshev aka dark_k3y
%
% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:
%
% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.
%
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
% SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR
% THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%
%%%-------------------------------------------------------------------
%%% @author dark_k3y
%%% @doc
%%% Length field predictors.
%%% @end
%%%-------------------------------------------------------------------
-module(erlamsa_field_predict).
-author("dark_k3y").

-include("erlamsa.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

%% API
-export([get_possible_simple_lens/1, get_possible_csum_locations/1, extract_blob/5, rebuild_blob/8, recalc_csum/2]).

-type lenfield_range() :: {integer(), integer()}.
-type sizer_location() :: {ok, integer(), integer(), big | little, integer(), integer()}.
-type csum_type() :: xor8 | crc32.
-type csum_location() :: {csum_type(), integer(), integer(), integer()}.
-type big_or_little() :: big | little.

%%%
%%% Tools for sizer predictions
%%%

-spec basic_u8len(lenfield_range(), binary()) -> [] | sizer_location().
basic_u8len({A, B}, Binary) when A < B, B > 0, A < size(Binary) ->
    Am8 = A*8,
    case Binary of
        <<_H:Am8, Len:8, _Rest/binary>> when Len =:= B - A - 1, Len > 2
            -> {ok, 8, big, Len, A, B};
        _Else -> []
    end;
basic_u8len(_Range, _Binary) -> [].

-spec simple_u8len(integer(), binary()) -> [] | sizer_location().
simple_u8len(A, Binary) ->
    lists:flatten([
        basic_u8len({A, (size(Binary)-X)}, Binary) || X <- lists:seq(0, 8)
    ]).

-spec basic_len(lenfield_range(), binary()) -> [] | sizer_location().
basic_len({A, B}, Binary) when A < B, B > 0, A < size(Binary) ->
    Am8 = A*8,
    case Binary of
        <<_H:Am8, Len:16/big, _Rest/binary>> when Len =:= B - A - 2, Len > 2 -> {ok, 16, big, Len, A, B};
        <<_H:Am8, Len:32/big, _Rest/binary>> when Len =:= B - A - 4, Len > 2 -> {ok, 32, big, Len, A, B};
        <<_H:Am8, Len:64/big, _Rest/binary>> when Len =:= B - A - 8, Len > 2 -> {ok, 64, big, Len, A, B};
        <<_H:Am8, Len:16/little, _Rest/binary>> when Len =:= B - A - 2, Len > 2 -> {ok, 16, little, Len, A, B};
        <<_H:Am8, Len:32/little, _Rest/binary>> when Len =:= B - A - 4, Len > 2 -> {ok, 32, little, Len, A, B};
        <<_H:Am8, Len:64/little, _Rest/binary>> when Len =:= B - A - 8, Len > 2 -> {ok, 64, little, Len, A, B};    
    _Else -> []
    end;
basic_len(_Range, _Binary) -> [].

-spec simple_len(lenfield_range(), binary()) -> [] | sizer_location().
simple_len({A, B}, Binary)->
    lists:flatten([
        basic_len({A, (B)}, Binary),
        basic_len({A, (B-1)}, Binary),
        basic_len({A, (B-2)}, Binary),
        basic_len({A, (B-4)}, Binary),
        basic_len({A, (B-8)}, Binary)
    ]).

-spec get_possible_simple_lens(binary()) -> list(sizer_location()).
get_possible_simple_lens(Binary) when size(Binary) > 10 ->
    Len = size(Binary),
    SubLen = min(trunc(Len/5), ?SIZER_MAX_FIRST_BYTES),
    FirstSeq = lists:seq(0, SubLen),
    VarBSeq = [erlamsa_rnd:rand_range(SubLen, Len) || _A <- FirstSeq],
    Ranges = [{X, Y} || X <- FirstSeq, Y <- VarBSeq],
    AllRanges = lists:foldr(fun (A, Acc) -> [{A, Len}|Acc] end, Ranges, FirstSeq),
    BigLens = lists:foldl(fun ({A, B}, Acc) ->
                [simple_len({A, B}, Binary) | Acc] end, [], AllRanges),
    SmallLens = [simple_u8len(A, Binary) || A <- FirstSeq],
    lists:flatten([SmallLens | BigLens]);
get_possible_simple_lens(Binary) ->
    lists:flatten([
        [simple_len({X, size(Binary)}, Binary), simple_u8len(X, Binary)]
            || X <- lists:seq(0, 3)]).

%%%
%%% Tools for extracting rebuilding binaries with sizers
%%%

-spec extract_blob(big_or_little(), binary(), integer(), integer(), integer()) -> {binary(), integer(), binary(), binary()}.
extract_blob(big, Binary, Am8, Size, Len8) ->
    <<H_e:Am8, Len_e:Size/big, Blob_e:Len8, Rest_e/binary>> = Binary,
    {H_e, Len_e, Blob_e, Rest_e};
extract_blob(little, Binary, Am8, Size, Len8) ->
    <<H_e:Am8, Len_e:Size/little, Blob_e:Len8, Rest_e/binary>> = Binary,
    {H_e, Len_e, Blob_e, Rest_e}.

-spec rebuild_blob(big_or_little(), binary(), integer(), integer(), integer(), binary(), integer(), binary()) -> binary().
rebuild_blob(big, H, Am8, Len, Size, Blob, Len8, Block) ->
    <<H:Am8, Len:Size/big, Blob:Len8, Block/binary>>;
rebuild_blob(little, H, Am8, Len, Size, Blob, Len8, Block) ->
    <<H:Am8, Len:Size/little, Blob:Len8, Block/binary>>.

%%%
%%% Tools for dealing with control sums
%%%

-spec calc_xor8(binary()) -> integer().
calc_xor8(B) ->
    lists:foldl(fun(A, Acc) -> A bxor Acc end, 0, binary_to_list(B)).

-spec has_xor8_checksum(binary(), integer(), integer()) -> csum_location().
has_xor8_checksum(Binary, Len, PreambleStart) -> 
    PreambleStart8 = PreambleStart*8,
    BodyLen8 = (Len - PreambleStart - 1)*8,
    <<_P:PreambleStart8, B:BodyLen8, C:8>> = Binary,
    case calc_xor8(<<B:BodyLen8>>) of 
        C -> {xor8, 8, PreambleStart, Len - PreambleStart - 1};
        _Else -> []
    end. 

-spec has_crc32_checksum(binary(), integer(), integer()) -> csum_location().
has_crc32_checksum(Binary, Len, PreambleStart) when Len - PreambleStart >= 4  -> 
    PreambleStart8 = PreambleStart*8,
    BodyLen8 = (Len - PreambleStart - 4)*8,
    <<_P:PreambleStart8, B:BodyLen8, C:32/integer>> = Binary,
    case erlang:crc32(<<B:BodyLen8>>) of 
        C -> {crc32, 32, PreambleStart, Len - PreambleStart - 4};
        _Else -> []
    end;
has_crc32_checksum(_Binary, _Len, _PreambleStart) -> [].

-spec get_possible_csum_locations(binary()) -> list(csum_location()).
get_possible_csum_locations(<<>>) ->
    [];
get_possible_csum_locations(Binary) ->
    Len = size(Binary),
    PreambleSeq = lists:seq(0, min(trunc(2*Len/3), lists:seq(0, ?PREAMBLE_MAX_BYTES))),
    lists:flatten([[has_xor8_checksum(Binary, Len, A) || A <- PreambleSeq], 
                   [has_crc32_checksum(Binary, Len, A) || A <- PreambleSeq]]).
    
-spec recalc_csum(csum_type(), binary()) -> integer().
recalc_csum(crc32, Binary) ->
    erlang:crc32(Binary);
recalc_csum(xor8, Binary) ->
    calc_xor8(Binary).