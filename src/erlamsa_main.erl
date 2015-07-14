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
%%% Main fuzzer module.
%%% @end
%%%-------------------------------------------------------------------
-module(erlamsa_main).
-author("dark_k3y").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

-include("erlamsa.hrl").

%% API
-export([test/0, test/1, test_input/1, fuzzer/1]).

-spec urandom_seed() -> {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
urandom_seed() -> list_to_tuple(lists:map(fun(_) -> lists:foldl(fun(X, A) -> X + (A bsl 8) end, 0, binary_to_list(crypto:rand_bytes(2))) end, lists:seq(1,3))).

-spec maybe_meta_logger(string() | atom(), fun()) -> fun().
maybe_meta_logger(Path, _) ->  
  Verb = case Path of
      nil -> fun (X) -> X end;
      stderr -> erlamsa_utils:verb(stderr, 10);
      stdout -> erlamsa_utils:verb(stdout, 10);
      _Else -> 
        {Res, Fd} = file:open(Path, [write]),
        case Res of 
            ok -> 
                OutputFun = erlamsa_utils:verb(Fd, 10), 
                fun 
                    ({close, ok}) -> file:close(Fd);
                    (X) -> OutputFun(X)
                end;
            _Else ->
                Err = lists:flatten(io_lib:format("Error opening file '~s'", [Path])),  %% TODO: add printing filename, handling -r and other things...
                erlamsa_utils:error(Err),
                fun(X) -> X end
        end

  end,    
  fun (X) -> Verb(io_lib:format("~p", [X])) end.

-spec test() -> list().
test() -> fuzzer(maps:put(paths, ["test.1"], maps:put(verbose, 0,  maps:put(output, return, maps:new())))).

test_input(Inp) -> fuzzer(
                    maps:put(paths, [direct], 
                        maps:put(verbose, 0,  
                            maps:put(output, return, 
                                maps:put(input, Inp, maps:new()))))).

test(Seed) -> fuzzer(
                maps:put(paths, ["test.1"], 
                    maps:put(verbose, 0,  
                        maps:put(output, "-", 
                            maps:put(seed, Seed, maps:new()))))).

-spec fuzzer(#{}) -> [binary()].
fuzzer(Dict) ->    
    Seed = maps:get(seed, Dict, default),
    Mutas = maps:get(mutations, Dict, default),
    N = maps:get(n, Dict, default),
    Paths = maps:get(paths, Dict, ["-"]),
    if
        Seed =:= default ->
            fuzzer(maps:put(seed, urandom_seed(), Dict));
        Mutas =:= default ->
            fuzzer(maps:put(mutations, erlamsa_mutations:default(), Dict));
        N =:= default ->
            fuzzer(maps:put(n, 1, Dict));
        true ->
            random:seed(maps:get(seed, Dict)),
            %io:write(maps:get(seed, Dict)),
            file:write_file("./last_seed.txt", io_lib:format("~p", [maps:get(seed, Dict)])),
            Fail = fun(Why) -> io:write(Why), throw(Why) end,
            Muta = erlamsa_mutations:make_mutator(Mutas),
            DirectInput = maps:get(input, Dict, nil),
            Gen = erlamsa_gen:make_generator(maps:get(generators, Dict, erlamsa_gen:default()), Paths, DirectInput, Fail, N),
            Record_Meta = maybe_meta_logger( maps:get(metadata, Dict, nil), Fail),
            Record_Meta({seed, maps:get(seed, Dict)}),            
            Pat = erlamsa_patterns:make_pattern(maps:get(patterns, Dict, erlamsa_patterns:default())),
            Out = erlamsa_out:string_outputs(maps:get(output, Dict, "-")),
            Sleep = maps:get(sleep, Dict, 0),
            fuzzer_loop(Muta, Gen, Pat, Out, Record_Meta, 1, N, Sleep, [])
    end.

-spec record_result(binary(), list()) -> list().
record_result(<<>>, Acc) -> Acc;
record_result(X, Acc) -> [X | Acc].

-spec fuzzer_loop(fun(), fun(), fun(), fun(), fun(), non_neg_integer(), non_neg_integer(), non_neg_integer() | inf, list()) -> [binary()].
fuzzer_loop(_, _, _, _, RecordMetaFun, I, N, _, Acc) when is_integer(N) andalso N < I -> RecordMetaFun({close, ok}), lists:reverse(Acc); 
fuzzer_loop(Muta, Gen, Pat, Out, RecordMetaFun, I, N, Sleep, Acc) ->
    {Ll, GenMeta} = Gen(), 
    {NewOut, Fd, OutMeta} = Out(I, [{nth, I}, GenMeta]),    
    Tmp = Pat(Ll, Muta, OutMeta),
    {NewMuta, Meta, Written, Data} = erlamsa_out:output(Tmp, Fd),
    RecordMetaFun([{written, Written}| Meta]),    
    timer:sleep(Sleep),
    fuzzer_loop(NewMuta, Gen, Pat, NewOut, RecordMetaFun, I + 1, N, Sleep, record_result(Data, Acc)).



