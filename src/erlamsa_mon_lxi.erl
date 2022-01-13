-module(erlamsa_mon_lxi).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

-include("erlamsa.hrl").

-define(LXI_TIMEOUT, 1000).

-export([start/1]).

parse_params(["url=" ++ URL|T], Acc) ->
    case http_uri:parse(URL) of
        {ok, {_Proto, _Auth, Host, Port, _Path, _Query}} -> 
            parse_params(T, maps:put(url, {Host, Port}, Acc));
        %%TODO: fail in more elegant way
        _ ->
            io:format("Invalid network monitor URL format: ~p, skipping...", [URL]), 
            fail
    end;
parse_params(["var=" ++ Report|T], Acc) ->
    parse_params(T, maps:put(var, list_to_atom(Report), Acc));
parse_params(["channel=" ++ Channel|T], Acc) ->
    parse_params(T, maps:put(channel, list_to_integer(Channel), Acc));
parse_params(["delay=" ++ Delay|T], Acc) ->
    parse_params(T, maps:put(delay , list_to_integer(Delay), Acc));
parse_params(["uvalue=" ++ Timeout|T], Acc) ->
    parse_params(T, maps:put(uvalue , list_to_float(Timeout), Acc));
parse_params(["lvalue=" ++ Timeout|T], Acc) ->
    parse_params(T, maps:put(lvalue , list_to_float(Timeout), Acc));
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
                erlamsa_logger:log(info, "LXI monitor started", []),
                spawn(fun() -> lxi_monitor(MonOpts) end),
                timer:sleep(infinity)
            end),
            {ok, Pid}
    end.
 
%%% maps:get(delay, MonOpts, 1000)
%%% gen_tcp:send(Sock, erlamsa_utils:hexstr_to_bin(Data)),
%%% Res = gen_tcp:recv(Sock, 0, Timeout),

lxi_monitor(MonOpts) ->
    erlamsa_logger:log(info, "network monitor: sending TCP connect probe...", []),
    {Addr, Port} = maps:get(url, MonOpts),
    case gen_tcp:connect(Addr, Port, [binary, {active, false}], ?LXI_TIMEOUT) of 
        {ok, Sock} ->
            Channel = maps:get(channel, MonOpts, 1),
            gen_tcp:send(Sock, io_lib:format("INST ~p~n", [Channel])),
            lxi_monitor_loop(MonOpts, Sock, 
                maps:get(delay, MonOpts, 1000),
                {   maps:get(var, MonOpts, current),
                    maps:get(lvalue, MonOpts, 0.5),
                    maps:get(uvalue, MonOpts, 1.0)
                });
        Err -> 
            erlamsa_logger:log(error, "Could not connect to LXI endpoint ~s:~p: ~p~n", [Addr, Port, Err])
    end.

lxi_monitor_loop(MonOpts, Sock, Delay, Params = {Var, LValue, UValue}) ->
    gen_tcp:send(Sock, io_lib:format("MEAS:CURR?~n", [])),
    case gen_tcp:recv(Sock, 0, ?LXI_TIMEOUT) of
        {ok, BinVal} -> 
            StrVal = binary_to_list(BinVal),
            Val = list_to_float(string:trim(StrVal)),
            if
                Val > UValue; Val < LValue ->
                    erlamsa_logger:log(finding, "LXI monitor: variable (~p) value ~p is outside of range: [~p, ~p]", [Var, Val, LValue, UValue]),
                    erlamsa_monitor:do_after(MonOpts);
                true -> 
                    erlamsa_logger:log(debug, "LXI monitor: variable (~p) value ~p is within range: [~p, ~p]", [Var, Val, LValue, UValue]),
                    ok
            end,
            timer:sleep(Delay),
            lxi_monitor_loop(MonOpts, Sock, Delay, Params);
        Err -> 
            erlamsa_logger:log(error, "LXI communication error: ~p~n", [Err])
    end.

