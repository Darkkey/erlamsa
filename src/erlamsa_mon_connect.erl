-module(erlamsa_mon_connect).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

-include("erlamsa.hrl").

-export([start/1]).

parse_params(["after_params=" ++ AfterParam|T], Acc) ->
    parse_params(T, maps:put(do_after_params, AfterParam, Acc));
parse_params(["after=" ++ AfterType|T], Acc) ->
    parse_params(T, maps:put(do_after_type, list_to_atom(AfterType), Acc));
parse_params(["port=" ++ PortNum|T], Acc) ->
    parse_params(T, maps:put(port, list_to_integer(PortNum), Acc));
parse_params(["timeout=" ++ PortNum|T], Acc) ->
    parse_params(T, maps:put(timeout , list_to_integer(PortNum), Acc));
parse_params([_H|T], Acc) ->
    %%TODO: do it in more elegant way
    io:format("Invalid monitor parameter: ~p, skipping...", [T]),
    parse_params(T, Acc);
parse_params([], Acc) ->
    Acc.

start(Params) ->
    {GenericMonOpts, LeftParams} = erlamsa_monitor:parse_after(string:split(Params, ",", all)),
    MonOpts = parse_params(LeftParams, GenericMonOpts),
    Port = maps:get(port, MonOpts, 51234),
    Pid = spawn_link(fun() ->
        {ok, Listen} = gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}, {packet, 0}]),
        erlamsa_logger:log(info, "connection monitor listening on port ~p", [Port]),
        spawn(fun() -> server_tcp(Listen, MonOpts) end),
        timer:sleep(infinity)
        end),
    {ok, Pid}.
 
server_tcp(ListenSocket, MonOpts) ->
    Timeout = maps:get(timeout, MonOpts, 5000),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> server_tcp(ListenSocket, MonOpts) end),
    {ok, {Host, Port}} = inet:peername(Socket),
    erlamsa_logger:log(info, "connection monitor got new connection from ~s:~p, socket ~p", 
                             [inet:ntoa(Host), Port, Socket]),
    loop_tcp(Socket, Timeout, MonOpts).
 
loop_tcp(Socket, Timeout, MonOpts) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            erlamsa_logger:log_data(info, "connection monitor got data on ~p:", [Socket], Data),
            loop_tcp(Socket, Timeout, MonOpts);
        {tcp_closed, Socket} ->
            erlamsa_logger:log(info, "connection monitor socket ~p closed by client", [Socket]),
            erlamsa_monitor:do_after(MonOpts),
            gen_tcp:close(Socket)
    after 
        Timeout ->
            erlamsa_logger:log(info, "connection monitor socket ~p closed on timeout", [Socket]),
            erlamsa_monitor:do_after(MonOpts),
            gen_tcp:close(Socket)
    end.