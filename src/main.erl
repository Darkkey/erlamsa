%%%-------------------------------------------------------------------
%%% @author dark_k3y
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(main).
-author("dark_k3y").

-compile([export_all]).

%% API
-export([start/0, start/2]).

urandom_seed() -> list_to_tuple(lists:map(fun(_) -> lists:foldl(fun(X, A) -> X + (A bsl 8) end, 0, binary_to_list(crypto:rand_bytes(2))) end, lists:seq(1,3))).

verb(0) -> fun(X) -> X end;
verb(_) -> fun(X) -> io:write(X) end. %% TODO: standard_error, ?

maybe_meta_logger(Path, Verbose, _) ->
  _ = verb(Verbose),
  if
      Path =:= "-" -> fun(X) -> io:write(X) end %% TODO: standard_output, ?
  end.

start() -> start(maps:new(), ["test.1"]).

start(Dict, Paths) ->
    Seed = maps:get("seed", Dict, default),
    Mutas = maps:get("mutations", Dict, []),
    N = maps:get("n", Dict, default),
    if
        Seed =:= default ->
            start(maps:put("seed", urandom_seed(), Dict), Paths);
        Mutas =:= [] ->
            start(maps:put("mutations", mutations:default_mutations(), Dict), Paths);
        N =:= default ->
            start(maps:put("n", 1, Dict), Paths);
        true ->
            random:seed(maps:get("seed", Dict)),
            Fail = fun(Why) -> io:write(Why), throw(Why) end,
            N = N,
            Mutas = Mutas,
            Muta = mutations:mutators_mutator(Mutas),
            Gen = generators:generators_priorities_to_generator(maps:get("generators", Dict, generators:default()), Paths, Fail, N),
            Record_Meta = maybe_meta_logger( maps:get("metadata", Dict, "-"), maps:get("verbose", Dict, 1), Fail),
            Record_Meta(maps:put(seed, maps:get("seed", Dict), maps:new())),
            Pat = patterns:string_patterns(maps:get("patterns", Dict, patterns:patterns())),
            Out = erlamsa_out:string_outputs(maps:get("output", Dict, "-"), N),
            loop(Muta, Gen, Pat, Out, Record_Meta, 1, N)
    end.

loop(_, _, _, _, _Record_Meta, I, N) when is_integer(N) andalso N < I -> ok; %% record-meta("closed")
loop(Muta, Gen, Pat, Out, Record_Meta, I, N) ->
    {Ll, GenMeta} = Gen(), 
    {_, Fd, OutMeta} = Out([{nth, I} | GenMeta]),
    Tmp = Pat(Ll, Muta, OutMeta),
    {NewMuta, Meta, _Written} = erlamsa_out:output(Tmp, Fd),
    io:write(Meta),    
    loop(NewMuta, Gen, Pat, Out, Record_Meta, I + 1, N).



