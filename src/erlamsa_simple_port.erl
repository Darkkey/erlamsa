-module(erlamsa_simple_port).
-export([start/2, stop/0, init/2, call_port/1]).

start(ExtPrg, ExtraParams) ->
    spawn(?MODULE, init, [ExtPrg, ExtraParams]).
stop() ->
    erlamsa_simple_port ! stop.

call_port(Msg) ->
    erlamsa_simple_port ! {call, self(), Msg},
    receive
    {erlamsa_simple_port, Result} ->
        Result
    end.

init(ExtPrg, _ExtraParams) ->
    register(erlamsa_simple_port, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [use_stdio, stderr_to_stdout, stream, hide]),
          %^ | ExtraParams
    loop(Port).

loop(Port) ->
    receive
    {Port, {data, Data}} ->
        io:format("Data from port ~p~n", [Data]),
        loop(Port);
    {call, Caller, Msg} ->
        Port ! {self(), {command, Msg}},
        io:format("Sending command: ~p", [Msg]),
        receive
        {Port, {data, Data}} ->
            io:format("Reply Data from port ~p~n", [Data]),
            Caller ! {erlamsa_simple_port, Data}
        end,
        loop(Port);
    stop ->
        Port ! {self(), close},
        receive
        {Port, closed} ->
            exit(normal)
        end;
    {'EXIT', Port, _Reason} ->
        io:format("Exitting!"),
        exit(port_terminated)
    end.