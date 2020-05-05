% Copyright (c) 2014-2020 Alexander Bolshev aka dark_k3y
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
%%% Cloud manager
%%% @end
%%%-------------------------------------------------------------------

-module(erlamsa_cmanager).
-export([start/1, cmanager/1, get_supervisor_opts/1, 
         decode_token/1, 
         get_client_context/2,
         insert_token/2, delete_token/2, get_sessions/1]).

-include("erlamsa.hrl").

-spec get_supervisor_opts(options()) -> supervisor:child_spec().
get_supervisor_opts(Opts) ->
    #{id => ?MODULE,
    start => {?MODULE, start, [Opts]},
    restart => permanent,
    shutdown => brutal_kill,
    type => worker,
    modules => [?MODULE]}.

-spec get_client_context(string(), string()) -> map().
get_client_context(Token, Session) ->
    global:send(cloud_manager, {getcontext, self(), decode_token(Token), decode_token(Session)}),
    receive
        {ok, context, Data} -> 
            {ok, {NewSession, Context}} = Data,
            {ok, {encode_token(NewSession), Context}}
    after 
        ?TCP_TIMEOUT -> {error, timeout}
    end.

-spec insert_token(string(), string()) -> tuple().
insert_token(AuthToken, Token) ->
    global:send(cloud_manager, {inserttoken, self(), decode_token(AuthToken), decode_token(Token)}),
    receive
        {ok, insert, Result} -> Result
    after 
        ?TCP_TIMEOUT -> {error, timeout}
    end.

-spec delete_token(string(), string()) -> tuple().
delete_token(AuthToken, Token) ->
    global:send(cloud_manager, {deletetoken, self(), decode_token(AuthToken), decode_token(Token)}),
    receive
        {ok, delete, Result} -> Result
    after 
        ?TCP_TIMEOUT -> {error, timeout}
    end.

-spec get_sessions(string()) -> tuple().
get_sessions(AuthToken) ->
    global:send(cloud_manager, {sessions, self(), decode_token(AuthToken)}),
    receive
        {ok, sessions, Result} -> Result
    after 
        ?TCP_TIMEOUT -> {error, timeout}
    end.

-spec start(options()) -> {ok, pid()}.
start(Dict) ->
    Pid = spawn(erlamsa_cmanager, cmanager,
                    [maps:get(cloudsvc, Dict, nil)]),
    global:register_name(cloud_manager, Pid),
    {ok, Pid}.

-spec cmanager(map() | nil) -> no_return().
cmanager(nil) -> cmanager_loop(maps:put(admintoken, nil, maps:new()));
cmanager(AdminToken) ->
    mnesia:create_table(token, [{attributes, record_info(fields, token)}, {type, set}]),
    mnesia:create_table(session, [{attributes, record_info(fields, session)}, {index, [tokenid]}, {type, set}]),
    timer:send_interval(?NODES_CHECKTIMER, {cleanup_sessions}),
    cmanager_loop(maps:put(admintoken, AdminToken, maps:new())).

-spec cmanager_loop(list() | nil) -> no_return().
cmanager_loop(Opts) ->
    receive
        {cleanup_sessions} ->
            spawn(fun cleanup_sessions/0);
        {getcontext, Pid, Token, Session} ->
            Pid ! {ok, context, get_context(maps:get(admintoken, Opts, nil), Token, Session)};
        {inserttoken, Pid, AuthToken, TokenValue} ->
            Pid ! {ok, insert, auth_operation(maps:get(admintoken, Opts, nil), AuthToken, 
                                              fun insert_token/1 , [TokenValue])};
        {deletetoken, Pid, AuthToken, TokenValue} ->
            Pid ! {ok, delete, auth_operation(maps:get(admintoken, Opts, nil), AuthToken, 
                                              fun delete_token/1 , [TokenValue])};
        {tokens, Pid, AuthToken} ->
            Pid ! {ok, tokens, auth_operation(maps:get(admintoken, Opts, nil), AuthToken, 
                                              fun get_tokens/0 , [])};
        {sessions, Pid, AuthToken} ->
            Pid ! {ok, sessions, auth_operation(maps:get(admintoken, Opts, nil), AuthToken, 
                                              fun get_sessions/0 , [])}
        % {status, Pid, AuthToken} ->
        %     Pid ! {ok, status, get_status(maps:get(admintoken, Opts, nil), AuthToken)}
    end,
    cmanager_loop(Opts).

-spec encode_token(integer()) -> string().
encode_token(nil) -> nil;
encode_token(Int) ->
    base64:encode(<<Int:?SESSIONKEYBITS_LENGTH/integer>>).

-spec decode_token(string()) -> integer().
decode_token(nil) -> nil;
decode_token(Str) ->
    Bin = base64:decode(Str),
    binary:decode_unsigned(Bin).

%% WARN: will be moved to CouchDB eventually
-spec auth_client_token(integer()) -> ok | unauth.
auth_client_token(Token) -> 
    {atomic, Tokens} = mnesia:transaction(fun () -> mnesia:read(token, Token) end),
    case length(Tokens) of 
        1 -> ok;
        _Else -> unauth
    end.

-spec get_context(integer() | nil, integer() | nil, integer() | nil) -> {ok | error, {integer() | nil, integer() | nil} | atom()}.
get_context(nil, _, _) -> {ok, {nil, maps:new()}};
get_context(_Opts, nil, Session) when is_integer(Session) -> 
    {atomic, Sessions} = mnesia:transaction(fun () -> mnesia:read(session, Session) end),
    case length(Sessions) of
        1 ->
            mnesia:transaction(
                fun() ->
                    [S] = mnesia:read(session, Session, write),
                    New = S#session{lastaccess = erlang:system_time(seconds)},
                    mnesia:write(New)
                end), 
            {ok, {Session, maps:new()}};
        _Else -> 
            {error, unauth}
    end;
get_context(_Opts, Token, _) when is_integer(Token) ->  
    case auth_client_token(Token) of 
        ok -> 
            SessionKey = binary:decode_unsigned(crypto:strong_rand_bytes(?SESSIONKEY_LENGTH)),
            mnesia:transaction(fun () ->
                mnesia:write(#session{lastaccess = erlang:system_time(seconds), 
                                        tokenid = Token, sessionid = SessionKey})
            end),
            {ok, {SessionKey, maps:new()}};
        _Else ->
            {error, unauth}
    end;
get_context(_, _, _) -> {error, badarg}.

-spec auth_operation(integer() | nil, integer() | nil, fun(), list()) -> {ok | error, any()}.
auth_operation(nil, _, _, _) -> {error, unauth};
auth_operation(AuthToken, InpAuthToken, Fun, Args) when AuthToken =:= InpAuthToken -> 
    erlang:apply(Fun, Args);
auth_operation(A, B, _, _) when A =/= B -> {error, unauth};
auth_operation(_, _, _, _) -> {error, badarg}.

-spec insert_token(integer() | nil) -> {ok | error, atom()}.
insert_token(TokenValue) when is_integer(TokenValue) -> 
    mnesia:transaction(fun () ->
                mnesia:write(#token{date = erlang:system_time(seconds), 
                                    tokenid = TokenValue, type = 0})
            end),
    {ok, inserted};
insert_token(_) -> {error, badarg}.

-spec delete_token(integer() | nil) -> {ok | error, atom()}.
delete_token(TokenValue) when is_integer(TokenValue) -> 
    mnesia:transaction(fun () -> mnesia:delete(token, TokenValue, write) end),
    %TODO: delete all associated sessions
    case mnesia:transaction(fun() -> 
        TokenSessions = #session{tokenid = TokenValue, sessionid = '$1', _ = '_'},
		mnesia:select(session, [{TokenSessions, [], ['$1']}])    
     end) of
         {atomic, SessList} when is_list(SessList) -> 
            mnesia:transaction(fun () -> 
                lists:map(fun (S) -> mnesia:delete(session, S, write), ok end, SessList) 
            end),
            ok;
         {atomic, _Else} -> ok
    end,
    {ok, deleted};
delete_token(_) -> {error, badarg}.

%% WARN: Only for current purposes, will be deprecated
-spec get_tokens() -> {ok | error, any()}.
get_tokens() ->
    {atomic, List} = 
        mnesia:transaction(fun () ->
            mnesia:match_object(mnesia:table_info(token, wild_pattern)) 
        end),
    {ok, List}.

-spec get_sessions() -> {ok | error, any()}.
get_sessions() ->
    {atomic, List} = 
        mnesia:transaction(fun () ->
            mnesia:match_object(mnesia:table_info(session, wild_pattern)) 
        end),
    {ok, List}.

-spec cleanup_sessions() -> ok.
cleanup_sessions() ->
    case mnesia:transaction(fun() -> 
        OutdatedTime = erlang:system_time(seconds) - ?SESSION_EXPIRETIME,
        TokenSessions = #session{lastaccess = '$1', sessionid = '$2', _ = '_'},
        Guard = [{'<', '$1', OutdatedTime}],
		Res = mnesia:select(session, [{TokenSessions, Guard, ['$2']}]),
        Res
    end) of
         {atomic, SessList} when is_list(SessList) -> 
            mnesia:transaction(fun () -> 
                lists:map(fun (S) -> 
                    mnesia:delete(session, S, write), ok end, SessList) 
            end),
            ok;
         {atomic, _Else} -> ok
    end,
    ok.

