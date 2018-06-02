% Copyright (c) 2011-2014 Aki Helin
% Copyright (c) 2014-2015 Alexander Bolshev aka dark_k3y
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
%%% Input data generators.
%%% @end
%%%-------------------------------------------------------------------
-module(erlamsa_gen).
-author("dark_k3y").

-include("erlamsa.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

%% API
-export([make_generator/6, generators/0, default/0, tostring/1]).

%% fill the rest of the stream with padding random bytes
-spec finish(non_neg_integer()) -> [binary()].
finish(Len) ->
    N = erlamsa_rnd:rand(Len + 1), %% 1/(n+1) probability of possibly adding extra data
    if
        N =:= Len ->
            Bits = erlamsa_rnd:rand_range(1, 16),
            NLen = erlamsa_rnd:rand(1 bsl Bits),
            erlamsa_utils:check_empty([list_to_binary(erlamsa_rnd:random_numbers(256, NLen))]);
        true -> []
    end.

%% get random size of block
-spec rand_block_size(float()) -> non_neg_integer().
rand_block_size(BlockScale) ->
    max(erlamsa_rnd:rand(round(?MAX_BLOCK_SIZE*BlockScale)), round(?MIN_BLOCK_SIZE*BlockScale)).

%% work with input stream (from file/other stream)
-spec port_stream(input_inc(), float()) -> fun().
port_stream(Port, BlockScale) ->
    fun () -> stream_port(Port, false, rand_block_size(BlockScale), BlockScale, 0) end.

-spec stream_port(input_inc(), binary() | false, non_neg_integer(), float(), non_neg_integer())
        -> [binary()].
stream_port(Port, Last, Wanted, BlockScale, Len) ->
    case file:read(Port, Wanted) of
        {ok, Data} ->
            DataLen = byte_size(Data),
            if
                DataLen =:= Wanted ->
                    Block = erlamsa_utils:merge(Last, Data),
                    [Block | stream_port(Port, false, rand_block_size(BlockScale),
                                         BlockScale, Len + byte_size(Block))];
                true ->
                    stream_port(Port, erlamsa_utils:merge(Last, Data), Wanted - DataLen,
                                BlockScale, Len)
            end;
        eof ->
            file:close(Port),
            case Last of
                false -> finish(Len);
                _Else ->
                    [Last | finish(Len + byte_size(Last))]
            end;
        {error, _} ->
            file:close(Port),
            Last
    end.

%% stdin input
-spec stdin_generator(true | false, float()) -> fun().
stdin_generator(Online, BlockScale) ->
    %% TODO: investigate what is  (stdin-generator rs online?) in radamsa code and how
    %% force-ll correctly works...
    io:setopts(standard_io, [binary]),
    Ll = port_stream(standard_io, BlockScale),
    io:setopts(standard_io, []),
    LlM = if
              Online =:= true andalso is_function(Ll) -> Ll; %% TODO: ugly, rewrite
              true -> erlamsa_utils:forcell(Ll)
          end,
    fun () -> {LlM, {generator, stdin}} end.

%% file input
-spec file_streamer([string()], float()) -> fun().
file_streamer(Paths, BlockScale) ->
    N = length(Paths),
    fun () ->
        P = erlamsa_rnd:erand(N), %% lists indexing from 1 in erlang
        Path = lists:nth(P, Paths),
        {Res, Port} = file:open(Path, [read, raw, binary]), %% TODO: FIXME: could we use raw?
        case Res of
            ok ->
                Ll = port_stream(Port, BlockScale),
                {Ll, [{generator, file}, {source, path}]};
            _Else ->
                %% TODO: add printing filename, handling -r and other things...
                Err = lists:flatten(io_lib:format("Error opening file '~s'", [Path])),
                erlamsa_utils:error(Err)
        end
    end.

split_binary(Bin, BlockScale, Wanted) when byte_size(Bin) > byte_size(Wanted) ->
    Cut = Wanted * 8,
    <<Block:Cut, Rest/binary>> = Bin,
    [Block | split_binary(Rest, BlockScale, rand_block_size(BlockScale))];
split_binary(Bin, _BlockScale, _Wanted) ->
    %FIXME: Wanted - byte_size(Bin) gives too little chance, temp solution below
    [Bin|finish(byte_size(Bin))].

-spec direct_generator(binary(), non_neg_integer()) -> fun().
direct_generator(Input, BlockScale) ->
    fun () -> {split_binary(Input, BlockScale, rand_block_size(BlockScale)),
                {generator, direct}}
    end.

%% Random input stream
-spec random_stream(float()) -> [binary()].
random_stream(BlockScale) ->
    N = erlamsa_rnd:rand_range(32, round(?MAX_BLOCK_SIZE * BlockScale)),
    B = erlamsa_rnd:random_block(N),
    Ip = erlamsa_rnd:rand_range(1, 100),
    O = erlamsa_rnd:rand(Ip),
    case O of
        0 ->
            [B];
        _Else ->
            [B | random_stream(BlockScale)]
    end.

%% random generator
-spec random_generator(float()) -> {[binary()], [meta()]}.
random_generator(BlockScale) ->
    fun () -> {random_stream(BlockScale), {generator, random}} end.

%% [{Pri, Gen}, ...] -> Gen(rs) -> output end
-spec mux_generators(prioritized_list(), fun()) -> fun() | false.
mux_generators([], Fail) -> Fail("No generators!");
mux_generators([[_, T]|[]], _) -> T; %% TODO: Check here!
mux_generators(Generators, _) ->
    {SortedGenerators, N} = erlamsa_utils:sort_by_priority(Generators),
    fun () ->
        F = erlamsa_utils:choose_pri(SortedGenerators, erlamsa_rnd:rand(N)),
        F()
    end.

%% create a lambda-generator function based on the array
-spec make_generator_fun(list(), binary() | nil, float(), fun(), non_neg_integer()) -> fun().
make_generator_fun(Args, Inp, BlockScale, Fail, N) ->
    fun (false) -> Fail("Bad generator priority!");
        ({Name, Pri}) ->
            case Name of
                stdin when hd(Args) == "-" ->
                    {Pri, stdin_generator(N == 1, BlockScale)}; %% TODO: <<-- 1 in Radamsa
                stdin ->
                    false;
                file when Args =/= [] andalso Args =/= ["-"] andalso Args =/= [direct] ->
                    {Pri, file_streamer(Args, BlockScale)};
                file ->
                    false;
                direct when Inp =:= nil ->
                    false;
                direct ->
                    {Pri, direct_generator(Inp, BlockScale)};
                random ->
                    {Pri, random_generator(BlockScale)};
                _Else ->
                    Fail("Unknown generator name."),
                    false
            end
    end.

%% get a list of {GenAtom, Pri} and output the list of {Pri, Gen}
-spec make_generator(list(), list(), binary() | nil, float(), fun(), non_neg_integer())
        -> fun() | false.
make_generator(Pris, Args, Inp, BlockScale, Fail, N) ->
    Gs = [ A || A <- [(make_generator_fun(Args, Inp, BlockScale, Fail, N))(V1)
                      || V1 <- Pris], A =/= false],
    mux_generators(Gs, Fail).

-spec generators() -> list().
generators() -> [{random, 1, "generate random data"},
                {direct, 500, "read data directly from erlang function call arguments"},
                {file, 1000, "read data from given files"},
                {stdin, 100000,
                    "read data from standard input if no paths are given or - is among them"}].

-spec default() -> list().
default() ->[{Name, Pri} || {Name, Pri, _Desc} <- generators()].

-spec tostring(list()) -> string().
tostring(Lst) ->
    lists:foldl(fun ({Name, _Pri, _Desc}, Acc) -> atom_to_list(Name) ++ "," ++ Acc end, [], Lst).