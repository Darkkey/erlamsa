-module(erlamsa_esi).
-export([fuzz/3, json/3, parse_json/2]).

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

%% Manual parsing of input json keys
%% cos converting them to atoms could consume erlang memory and cause DoD attack
parse_json_map_elem(Key, Value, AccIn) when is_list(Key) ->
    case Key of
        "mutations" -> [{http_mutations, Value}|AccIn];
        "patterns" -> [{http_patterns, Value}|AccIn];
        "blockscale" -> [{http_blockscale, Value}|AccIn];
        "seed" -> [{http_seed, Value}|AccIn];
        _Unknown -> AccIn
    end.

parse_json(Env, Json) ->
    T1 = erlamsa_json:tokenize(list_to_binary(Json)),
    %% TODO: handle tokens conversion errors
    JsonMap = hd(erlamsa_json:tokens_to_erlang(T1)),
    Data = maps:get("data", JsonMap, ""),
    %% TODO: handle base64 decode errors
    In = base64:decode(Data),
    %% TODO: handle incorrent params errors
    NewEnv = maps:fold(fun parse_json_map_elem/3, Env, JsonMap),
    {NewEnv, In}.

call(Sid, Env, In) -> 
    Opts = parse_headers(Env, maps:new()),
    Dict = maps:put(paths, [direct],
            maps:put(output, return,
                maps:put(input, In,
                Opts))),    
    erlamsa_logger:log(info, "Request from IP ~s, session ~p", [maps:get(remote_addr, Dict, nil), Sid]),   
    erlamsa_logger:log_data(info, "Input data <session = ~p>", [Sid], In),
    Output = erlamsa_fsupervisor:get_fuzzing_output(Dict),
    erlamsa_logger:log_data(info, "Output data <session = ~p>", [Sid], Output),
    Output.

fuzz(Sid, Env, In) -> 
    try
        InBin = list_to_binary(In),
        Output = call(Sid, Env, InBin),
        mod_esi:deliver(Sid, [Output])
    catch
        error:badarg ->
            erlamsa_logger:log(error, "Session ~p: invalid input options detected, ~p", [Sid, Env]),   
        	mod_esi:deliver(Sid, ["Invalid header option(s) specification!"]);
        UnknownError ->
            erlamsa_logger:log(error, "Session ~p: unknown error with code ~p", [Sid, UnknownError]),   
        	mod_esi:deliver(Sid, ["Unknown unrecoverable error!"])
    end.
    
json(Sid, Env, In) -> 
    try
        %spawn(erlamsa_esi, parse_json, [Env, In]),
        {NewEnv, Data} = parse_json(Env, In),
        Output = call(Sid, NewEnv, Data),
        Ret = io_lib:format("{\"data\": \"~s\"}", [base64:encode(Output)]),
        mod_esi:deliver(Sid, [Ret])      
    catch
        error:badarg ->
            erlamsa_logger:log(error, "Session ~p: invalid JSON options detected.", [Sid]),   
        	mod_esi:deliver(Sid, ["{\"error\": \"Invalid JSON option(s) specification!\"}"]);       
        UnknownError ->
            erlamsa_logger:log(error, "Session ~p: invalid JSON document provided, error code ~p", [Sid, UnknownError]),   
        	mod_esi:deliver(Sid, ["{\"error\": \"Invalid or insufficient JSON document provided!\"}"])
    end.
