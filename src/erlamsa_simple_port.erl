-module(erlamsa_simple_port).
-export([start/2, stop/1, init/2, call/2, subscribe/1]).

start(ExtPrg, ExtraParams) ->
    spawn(?MODULE, init, [ExtPrg, ExtraParams]).

stop(Pid) ->
    Pid ! stop.

init(ExtPrg, _ExtraParams) ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [use_stdio, stderr_to_stdout, stream, hide]),
          %%FIXME:? ^ | ExtraParams
    loop(Port, nil).

call(Pid, Msg) ->
    Pid ! {call, self(), Msg},
    receive
        {data, Pid, Result} ->
            Result
    end.

subscribe(Pid) ->
    Pid ! {subscribe, self()}.

inform_subscriber(nil, _Data) -> ok;
inform_subscriber(Pid, Data) ->
    Pid ! {stream_data, self(), Data}.

loop(Port, Subscriber) -> 
    receive
        {Port, {data, Data}} ->
            inform_subscriber(Subscriber, Data),
            loop(Port, Subscriber);
        {call, Caller, Msg} ->
            Port ! {self(), {command, Msg}},
            receive
            {Port, {data, Data}} ->
                Caller ! {data, self(), Data}
            end,
            loop(Port, Subscriber);
        {subscribe, NewSubscriber} ->
            loop(Port, NewSubscriber);
        stop ->
            Port ! {self(), close},
            receive
            {Port, closed} ->
                exit(normal)
            end;
        {'EXIT', Port, _Reason} ->
            exit(port_terminated)
    end.