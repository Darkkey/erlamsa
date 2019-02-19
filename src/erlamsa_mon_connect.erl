-module(erlamsa_mon_connect).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

-include("erlamsa.hrl").

-export([start/1]).

parse_params(["host=" ++ Host|T], Acc) ->
    parse_params(T, maps:put(host, Host, Acc));
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
    {GenericMonOpts, LeftParams} = erlamsa_monitor:parse_after(string:tokens(Params, ",")),
    MonOpts = parse_params(LeftParams, GenericMonOpts),
    Port = maps:get(port, MonOpts, 51234),
    ets:insert(global_config, [{cm_port, Port}]),
    ets:insert(global_config, [{cm_host_user, maps:get(host, MonOpts, {})}]),
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
    erlamsa_logger:log(finding, "connection monitor got new connection from ~s:~p, socket ~p", 
                             [inet:ntoa(Host), Port, Socket]),
    loop_tcp(Socket, Timeout, MonOpts).

check_event(_Socket, <<${,$e,$v,$e,$n,$t,$},Rest/binary>>) ->
    process_event(binary_to_list(Rest));
check_event(Socket, Data) -> 
    erlamsa_logger:log_data(finding, "connection monitor got data on ~p:", [Socket], Data).

%% TODO: Add more events in future
process_event(Event) ->
     erlamsa_logger:log(finding, "event monitor new message: ~s", [string:trim(Event)]).
 
loop_tcp(Socket, Timeout, MonOpts) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            check_event(Socket, Data),
            loop_tcp(Socket, Timeout, MonOpts);
        {tcp_closed, Socket} ->
            erlamsa_logger:log(finding, "connection monitor socket ~p closed by client", [Socket]),
            erlamsa_monitor:do_after(MonOpts),
            gen_tcp:close(Socket)
    after 
        Timeout ->
            erlamsa_logger:log(finding, "connection monitor socket ~p closed on timeout", [Socket]),
            erlamsa_monitor:do_after(MonOpts),
            gen_tcp:close(Socket)
    end.