-module(erlamsa_mon_network).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

-include("erlamsa.hrl").

-export([start/1]).

parse_params(["url=" ++ URL|T], Acc) ->
    case http_uri:parse(URL) of
        {ok, {Proto, Auth, Host, Port, Path, Query}} -> 
            parse_params(T, maps:put(url, {Proto, Auth, Host, Port, Path ++ Query}, Acc));
        %%TODO: fail in more elegant way
        _ ->
            io:format("Invalid network monitor URL format: ~p, skipping...", [URL]), 
            fail
    end;
parse_params(["report=" ++ Report|T], Acc) ->
    parse_params(T, maps:put(report, list_to_atom(Report), Acc));
parse_params(["hello=" ++ Packet|T], Acc) ->
    parse_params(T, maps:put(hello, Packet, Acc));
parse_params(["delay=" ++ Delay|T], Acc) ->
    parse_params(T, maps:put(delay , list_to_integer(Delay), Acc));
parse_params(["timeout=" ++ Timeout|T], Acc) ->
    parse_params(T, maps:put(timeout , list_to_integer(Timeout), Acc));
parse_params([_H|T], Acc) ->
    %%TODO: do it in more elegant way
    io:format("Invalid monitor parameter: ~p, skipping...", [T]),
    parse_params(T, Acc);
parse_params([], Acc) ->
    Acc.

start(Params) ->
    {GenericMonOpts, LeftParams} = erlamsa_monitor:parse_after(string:tokens(Params, ",")),
    case parse_params(LeftParams, GenericMonOpts) of
        fail -> fail;
        MonOpts ->
            Pid = spawn_link(fun() ->
                erlamsa_logger:log(info, "network monitor started", []),
                spawn(fun() -> network_monitor(MonOpts, maps:get(delay, MonOpts, 1000)) end),
                timer:sleep(infinity)
            end),
            {ok, Pid}
    end.
 
network_monitor(MonOpts, Delay) ->
    case send_probe(maps:get(url, MonOpts, 1000), 
                    maps:get(hello, MonOpts, []),
                    maps:get(timeout, MonOpts, 5000)
                    ) of
        {ok, Data} -> erlamsa_logger:log_data(info, "network monitor: probe ok, target still alive :(, returned ", [], Data);
        {error, Reason} -> try_report(maps:get(report, MonOpts, all), Reason, MonOpts)
    end,
    timer:sleep(Delay),
    network_monitor(MonOpts, Delay).

%%TODO: FIXME: add http and icmp?
send_probe({udp, _Auth, Addr, Port, _Query}, Data, Timeout) ->
    erlamsa_logger:log(info, "network monitor: sending probe...", []),
    {_Res, Sock} = gen_udp:open(0, [binary, {active, false}, {reuseaddr, true}]),
    gen_udp:connect(Sock, Addr, Port),
    gen_udp:send(Sock, erlamsa_utils:hexstr_to_bin(Data)),
    Res = gen_udp:recv(Sock, 0, Timeout),
    gen_udp:close(Sock),
    Res;
send_probe({tcp, _Auth, Addr, Port, _Query}, Data, Timeout) ->
    erlamsa_logger:log(info, "network monitor: sending probe...", []),
    case gen_tcp:connect(Addr, Port, [binary, {active, false}], Timeout) of 
        {ok, Sock} ->
            gen_tcp:send(Sock, erlamsa_utils:hexstr_to_bin(Data)),
            Res = gen_tcp:recv(Sock, 0, Timeout),
            gen_tcp:close(Sock), Res;
        Err -> 
            Err
    end.

try_report(econnrefused, timeout, _) ->
    erlamsa_logger:log(info, "network monitor: probe timeout, but that's ok", []);
try_report(timeout, econnrefused, _) ->
    erlamsa_logger:log(info, "network monitor: connection refused, but that's ok", []);
try_report(Filter, Reason, MonOpts) ->
    erlamsa_logger:log(finding, "network monitor: probe failed, target seems down: ~p (filter = ~p) ", [Reason, Filter]),
    erlamsa_monitor:do_after(MonOpts).
        