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
%%% ESI module: handler of HTTP POST raw/json queries
%%% @end
%%%-------------------------------------------------------------------

-module(erlamsa_esi).
-export([fuzz/3, json/3, parse_json/2]).

-include("erlamsa.hrl").

-spec parse_headers(mod_esi:env(), options()) -> options().
parse_headers([{http_mutations, Mutators}|T], Acc) ->
    DefaultMutas = erlamsa_mutations:default(erlamsa_utils:make_mutas(maps:get(external_mutations, Acc, []))),
    {ok, M} = erlamsa_cmdparse:string_to_actions(Mutators, "mutations", DefaultMutas),
    parse_headers(T, maps:put(mutations, M, Acc));
parse_headers([{http_patterns, Patterns}|T], Acc) ->
    {ok, P} = erlamsa_cmdparse:string_to_actions(Patterns, "patterns", erlamsa_patterns:default()),
    parse_headers(T, maps:put(patterns, P, Acc));
parse_headers([{http_blockscale, B}|T], Acc) ->
    parse_headers(T, maps:put(blockscale, list_to_float(B), Acc));
parse_headers([{http_seed, Seed}|T], Acc) ->
    parse_headers(T, maps:put(seed, fun () -> erlamsa_cmdparse:parse_seed(Seed) end, Acc));
parse_headers([{remote_addr, IP}|T], Acc) ->
    parse_headers(T, maps:put(remote_addr, IP, Acc));
parse_headers([], Acc) ->
    Acc;
parse_headers([_H|T], Acc) ->
    parse_headers(T, Acc).

%% Manual parsing of input json keys
%% cos converting them to atoms could consume erlang memory and cause DoD attack
-spec parse_json_map_elem(string(), any(), list({atom, any()})) -> list({atom, any()}).
parse_json_map_elem(Key, Value, AccIn) when is_list(Key) ->
    case Key of
        "mutations" -> [{http_mutations, Value}|AccIn];
        "patterns" -> [{http_patterns, Value}|AccIn];
        "blockscale" -> [{http_blockscale, Value}|AccIn];
        "seed" -> [{http_seed, Value}|AccIn];
        _Unknown -> AccIn
    end.

-spec parse_json(mod_esi:env(), string()) -> {mod_esi:env(), binary()}.
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

-spec call(term(), mod_esi:env(), binary()) -> binary().
call(Sid, Env, In) ->
    InitOpts = case ets:match(global_config, {external_mutations, '$1'}) of
        [[Mutations]] -> maps:put(external_mutations, Mutations, maps:new());
        _ -> maps:new()
    end,
    Opts = parse_headers(Env, InitOpts),
    Dict = erlamsa_utils:get_direct_fuzzing_opts(In, Opts),
    erlamsa_logger:log(info, "Request from IP ~s, session ~p",
                        [maps:get(remote_addr, Dict, nil), Sid]),
    erlamsa_logger:log_data(info, "Input data <session = ~p>", [Sid], In),
    Output = erlamsa_fsupervisor:get_fuzzing_output(Dict),
    erlamsa_logger:log_data(info, "Output data <session = ~p>", [Sid], Output),
    Output.

-spec fuzz(term(), mod_esi:env(), binary()) -> ok | {error, any()}.
fuzz(Sid, Env, In) ->
    try
        InBin = list_to_binary(In),
        Output = call(Sid, Env, InBin),
        mod_esi:deliver(Sid, [Output])
    catch
        error:badarg ->
            erlamsa_logger:log(error, "Session ~p: invalid input options detected, ~p",
                                [Sid, Env]),
            mod_esi:deliver(Sid, ["Invalid header option(s) specification!"]);
        UnknownError ->
            erlamsa_logger:log(error, "Session ~p: unknown error with code ~p",
                                [Sid, UnknownError]),
            mod_esi:deliver(Sid, ["Unknown unrecoverable error!"])
    end.

-spec json(term(), mod_esi:env(), binary()) -> ok | {error, any()}.
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
            erlamsa_logger:log(error, "Session ~p: invalid JSON document provided, error code ~p",
                                [Sid, UnknownError]),
            mod_esi:deliver(Sid,
                            ["{\"error\": \"Invalid or insufficient JSON document provided!\"}"])
    end.
