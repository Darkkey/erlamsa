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
-module(erlamsa_len_predict).
-author("dark_k3y").

-include("erlamsa.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

%% API
-export([get_possible_simple_lens/1]).

-type lenfield_range() :: {integer(), integer()}.
-type sizer_location() :: {ok, integer(), integer(), integer(), integer()}.

-spec basic_u8len(lenfield_range(), binary()) -> [] | sizer_location().
basic_u8len({A, B}, Binary) when A < B, B > 0, A < size(Binary) ->
    Am8 = A*8,
    RestLen = B - A,
    case Binary of
        <<_H:Am8, Len:8, _Rest/binary>> when Len =:= RestLen, Len > 2 -> {ok, 8, Len, A, B};
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
    RestLen = B - A,
    case Binary of
        <<_H:Am8, Len:16, _Rest/binary>> when Len =:= RestLen, Len > 2 -> {ok, 16, Len, A, B};
        <<_H:Am8, Len:32, _Rest/binary>> when Len =:= RestLen, Len > 2 -> {ok, 32, Len, A, B};
        <<_H:Am8, Len:64, _Rest/binary>> when Len =:= RestLen, Len > 2 -> {ok, 64, Len, A, B};
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