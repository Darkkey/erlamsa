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
-export([fuzz/3, json/3, manage/3, parse_json/2]).

-include("erlamsa.hrl").

-spec parse_headers(mod_esi:env(), options()) -> options().
parse_headers([{http_erlamsa_mutations, Mutators}|T], Acc) ->
    DefaultMutas = erlamsa_mutations:default(erlamsa_utils:make_mutas(maps:get(external_mutations, Acc, []))),
    {ok, M} = erlamsa_cmdparse:string_to_actions(Mutators, "mutations", DefaultMutas),
    parse_headers(T, maps:put(mutations, M, Acc));
parse_headers([{http_erlamsa_patterns, Patterns}|T], Acc) ->
    {ok, P} = erlamsa_cmdparse:string_to_actions(Patterns, "patterns", erlamsa_patterns:default()),
    parse_headers(T, maps:put(patterns, P, Acc));
parse_headers([{http_erlamsa_blockscale, B}|T], Acc) ->
    parse_headers(T, maps:put(blockscale, list_to_float(B), Acc));
parse_headers([{http_erlamsa_seed, Seed}|T], Acc) ->
    parse_headers(T, maps:put(seed, fun () -> erlamsa_cmdparse:parse_seed(Seed) end, Acc));
parse_headers([{remote_addr, IP}|T], Acc) ->
    parse_headers(T, maps:put(remote_addr, IP, Acc));
parse_headers([{http_x_real_ip, IP}|T], Acc) ->
    parse_headers(T, maps:put(http_x_real_ip, IP, Acc));
parse_headers([{http_erlamsa_token, IP}|T], Acc) ->
    parse_headers(T, maps:put(erlamsa_token, IP, Acc));
parse_headers([{http_erlamsa_session, IP}|T], Acc) ->
    parse_headers(T, maps:put(erlamsa_session, IP, Acc));
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
    In = case maps:get("data", JsonMap, "") of 
        "" -> <<>>;
        %% TODO: handle base64 decode errors
        Data -> base64:decode(Data)
    end,
    %% TODO: handle incorrent params errors
    NewEnv = maps:fold(fun parse_json_map_elem/3, Env, JsonMap),
    {NewEnv, In}.


-spec call_fuzzer(term(), map(), binary()) -> binary().
call_fuzzer(Sid, Opts, In) -> 
    Dict = erlamsa_utils:get_direct_fuzzing_opts(In, Opts),
    erlamsa_logger:log(info, "Request from IP ~s, session ~p",
                        [maps:get(http_x_real_ip, Dict, maps:get(remote_addr, Dict, nil)), Sid]),
    erlamsa_logger:log(debug, "Session ~p options are ~p",
                        [Sid, Opts]),
    erlamsa_logger:log_data(info, "Input data <session = ~p>", [Sid], In),
    Output = erlamsa_fsupervisor:get_fuzzing_output(Dict),
    erlamsa_logger:log_data(info, "Output data <session = ~p>", [Sid], Output),
    Output.

-spec call(term(), mod_esi:env(), binary()) -> binary().
call(Sid, Env, In) ->
    InitOpts = case ets:match(global_config, {external_mutations, '$1'}) of
        [[Mutations]] -> maps:put(external_mutations, Mutations, maps:new());
        _ -> maps:new()
    end,
    Opts = parse_headers(Env, InitOpts),
    AuthRes = erlamsa_cmanager:get_client_context(maps:get('erlamsa_token', Opts, nil), 
                                                  maps:get('erlamsa_session', Opts, nil)),
    case AuthRes of 
        {ok, {Session, _CtxDict}} ->    
            {Session, call_fuzzer(Sid, Opts, In)};
        {error, timeout} ->
            throw(error_timeout);
        {error, unauth} ->
            throw(error_unauth)
    end.

-spec make_headers(integer(), string()) -> no_return().
make_headers(Status, nil) ->
    lists:flatten(io_lib:format("erlamsa-status: ~p\r\n\r\n", [Status]));
make_headers(Status, Session) ->
    lists:flatten(io_lib:format("erlamsa-status: ~p\r\nerlamsa-session: ~s\r\n\r\n", 
                                        [Status, Session])).

-spec fuzz(term(), mod_esi:env(), binary()) -> ok | {error, any()}.
fuzz(Sid, Env, In) ->
    try
        InBin = list_to_binary(In),
        {Session, Output} = call(Sid, Env, InBin),
        mod_esi:deliver(Sid, [make_headers(0, Session)]),
        mod_esi:deliver(Sid, [Output])
    catch
        error:badarg ->
            erlamsa_logger:log(error, "Session ~p: invalid input options detected, ~p",
                                [Sid, Env]),
            mod_esi:deliver(Sid, make_headers(500, nil)),
            mod_esi:deliver(Sid, "Invalid input parameters");
        error_unauth ->
            erlamsa_logger:log(error, "Session ~p: unauthenticated request, ~p",
                                [Sid, Env]),
            mod_esi:deliver(Sid, make_headers(401, nil)),
            mod_esi:deliver(Sid, <<>>);
        UnknownError ->
            erlamsa_logger:log(error, "Session ~p: unknown error with code ~p",
                                [Sid, UnknownError]),
            {error, unknown}
    end.

-spec json(term(), mod_esi:env(), binary()) -> ok | {error, any()}.
json(Sid, Env, In) ->
    try
        {NewEnv, Data} = parse_json(Env, In),
        {Session, Output} = call(Sid, NewEnv, Data),
        Ret = io_lib:format("{\"data\": \"~s\"}", [base64:encode(Output)]),
        mod_esi:deliver(Sid, make_headers(0, Session)),
        mod_esi:deliver(Sid, [Ret])
    catch
        error:badarg ->
            erlamsa_logger:log(error, "Session ~p: invalid JSON options detected.", [Sid]),
            mod_esi:deliver(Sid, make_headers(500, nil)),
            mod_esi:deliver(Sid, ["{\"error\": \"Invalid JSON option(s) specification!\"}"]);
        error_unauth ->
            erlamsa_logger:log(error, "Session ~p: unauthenticated request, ~p",
                                [Sid, Env]),
            mod_esi:deliver(Sid, make_headers(401, nil)),
            mod_esi:deliver(Sid, ["{\"error\": \"Invalid session or token id.\"}"]);
        UnknownError ->
            erlamsa_logger:log(error, "Session ~p: invalid JSON document provided, error code ~p",
                                [Sid, UnknownError]),
            mod_esi:deliver(Sid, "erlamsa-status: 500\r\n\r\n"),
            mod_esi:deliver(Sid,
                            ["{\"error\": \"Invalid or insufficient JSON document provided!\"}"])
    end.


-spec manage(term(), mod_esi:env(), binary()) -> binary().
manage(Sid, Env, In) ->
    try
        T1 = erlamsa_json:tokenize(list_to_binary(In)),
        Vars = hd(erlamsa_json:tokens_to_erlang(T1)),
        Hdrs = maps:from_list(Env),
        erlamsa_logger:log(info, "Managing request from IP ~s, session ~p",
                        [maps:get(http_x_real_ip, Hdrs, maps:get(remote_addr, Hdrs, nil)), Sid]),
        {Res, Data} = case maps:get("command", Vars, nil) of 
            "insert_token" -> 
                erlamsa_cmanager:insert_token(maps:get("authtoken", Vars, nil), maps:get("token", Vars, nil)); 
            "delete_token" -> 
                erlamsa_cmanager:delete_token(maps:get("authtoken", Vars, nil), maps:get("token", Vars, nil)); 
            "get_sessions" -> 
                erlamsa_cmanager:get_sessions(maps:get("authtoken", Vars, nil)); 
            _Else -> throw(badarg)
        end, 
        Ret = io_lib:format("{\"result\": \"~p\", \"data\": \"~p\"}", [Res, Data]),
        mod_esi:deliver(Sid, [Ret])
    catch
        error:badarg ->
            erlamsa_logger:log(error, "Session ~p: invalid JSON options detected.", [Sid]),
            mod_esi:deliver(Sid, make_headers(500, nil)),
            mod_esi:deliver(Sid, ["{\"error\": \"Invalid JSON option(s) specification!\"}"]);
        error_unauth ->
            erlamsa_logger:log(error, "Session ~p: unauthenticated request, ~p",
                                [Sid, Env]),
            mod_esi:deliver(Sid, make_headers(401, nil)),
            mod_esi:deliver(Sid, ["{\"error\": \"Invalid session or token id.\"}"]);
        UnknownError ->
            erlamsa_logger:log(error, "Session ~p: invalid JSON document provided, error code ~p",
                                [Sid, UnknownError]),
            mod_esi:deliver(Sid, "erlamsa-status: 500\r\n\r\n"),
            mod_esi:deliver(Sid,
                            ["{\"error\": \"Invalid or insufficient JSON document provided!\"}"])
    end.