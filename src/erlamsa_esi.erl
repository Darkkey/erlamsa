-module(erlamsa_esi).
-export([fuzz/3]).

parse_headers([{http_mutations, Mutators}|T], Acc) ->
    {ok, ML} = erlamsa_cmdparse:string_to_actions(Mutators, "mutations", erlamsa_utils:default([])),
	parse_headers(T, maps:put(mutations, ML, Acc));
parse_headers([{http_patterns, Patterns}|T], Acc) ->
    {ok, PL} = erlamsa_cmdparse:string_to_actions( Patterns, "patterns", erlamsa_patterns:default()),
	parse_headers(T, maps:put(patterns, PL, Acc));
parse_headers([{http_blockscale,B}|T], Acc) ->
    parse_headers(T, maps:put(blockscale, list_to_float(B), Acc));
parse_headers([{http_seed,Seed}|T], Acc) ->
    parse_headers(T, maps:put(seed, erlamsa_cmdparse:parse_seed(Seed), Acc));
parse_headers([{remote_addr,IP}|T], Acc) ->
    parse_headers(T, maps:put(remote_addr, IP, Acc));
parse_headers([], Acc) ->
    Acc;
parse_headers([_H|T], Acc) ->
    parse_headers(T, Acc).    

fuzz(Sid, Env, In) -> 
    try
        InBin = list_to_binary(In),
        Opts = parse_headers(Env, maps:new()),
        Dict = maps:put(paths, [direct],
                maps:put(output, return,
                    maps:put(input, InBin,
                    Opts))),    
        erlamsa_logger:log(info, "Request from IP ~s, session ~p", [maps:get(remote_addr, Dict, nil), Sid]),   
        erlamsa_logger:log_data(info, "Input data <session = ~p>", [Sid], InBin),
        Output = erlamsa_fsupervisor:get_fuzzing_output(Dict),
        erlamsa_logger:log_data(info, "Output data <session = ~p>", [Sid], Output),
        mod_esi:deliver(Sid, [Output])
    catch
        error:badarg ->
            erlamsa_logger:log(error, "Session ~p: invalid input options detected, ~p", [Sid, Env]),   
        	mod_esi:deliver(Sid, ["Invalid header option(s) specification!"]);
        UnknownError ->
            erlamsa_logger:log(error, "Session ~p: unknown error with code ~p", [Sid, UnknownError]),   
        	mod_esi:deliver(Sid, ["Unknown unrecoverable error!"])
    end.
