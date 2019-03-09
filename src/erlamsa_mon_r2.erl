-module(erlamsa_mon_r2).           

-include("erlamsa.hrl").

-export([start/1, init/1]).

parse_params(["app={EXEC}"|T], Acc) ->
    receive
        {executed, Pid} ->
            parse_params(T, maps:put(app, io_lib:format("~p", [Pid]), Acc))
    end;
parse_params(["app=" ++ App|T], Acc) ->
    parse_params(T, maps:put(app, App, Acc));
parse_params(["r2path=" ++ R2Path|T], Acc) ->
    parse_params(T, maps:put(r2path, R2Path, Acc));
parse_params([_H|T], Acc) ->
    %%TODO: do it in more elegant way
    io:format("Invalid monitor parameter: ~p, skipping...", [T]),
    parse_params(T, Acc);
parse_params([], Acc) ->
    Acc.

handle_r2_session(start, nil, R2Cmd) ->
    erlamsa_logger:log(info, "r2monitor attempting to run: '~s'", [R2Cmd]),
    {R2Pid, StartRes} = r2pipe:start(R2Cmd, []), %%TODO: windows platform handling? Do we need it or need to fix r2?
    handle_r2_session(started, R2Pid, StartRes);
handle_r2_session(started, exit, StartRes) ->
    erlamsa_logger:log(error, "R2 unexpectedly exit while starting application: ~s~n", [lists:flatten(StartRes)]),
    error;
handle_r2_session(started, R2Pid, StartRes = ["= attach" ++ _Pids| _T]) ->
    erlamsa_logger:log(info, "r2monitor r2 start messages: '~s'", [lists:flatten(StartRes)]),
    _AAARes = r2pipe:call(R2Pid, "aaa\n"), %%autoanalysis
    RunRes = r2pipe:call(R2Pid, "dc\n"), 
    handle_r2_session(continue, R2Pid, RunRes);
handle_r2_session(started, R2Pid, StartRes) ->
    erlamsa_logger:log(error, "Error while starting application: ~s ~n", [lists:flatten(StartRes)]),
    r2pipe:stop(R2Pid),
    error;
handle_r2_session(continue, R2Pid, [10] ++ "==> Process finished" ++ [10,10]) ->
    r2pipe:stop(R2Pid),
    erlamsa_logger:log(info, "r2monitor ==> Process finished without error code, is it ok or crash?", []),
    ok;
handle_r2_session(continue, R2Pid, CrashMsg) ->
    erlamsa_logger:log(finding, "r2monitor [-->!!!<--] detected event (CRASH?!): ~s", [CrashMsg]),
    RegInfoMsg = r2pipe:call(R2Pid, "drj\n"),
    erlamsa_logger:log(finding, "r2monitor registers info ~s", [RegInfoMsg]),
    Info = r2pipe:call(R2Pid, "ij\n"),
    erlamsa_logger:log(finding, "r2monitor executable info ~s", [Info]),
    %%MemMaps = r2pipe:call(R2Pid, "dmj\n"),
    %% FIXME: should we record memory maps now or later? because they seems to be empty after crash...
    %erlamsa_logger:log_data(info, "r2monitor memory maps recorded", [], list_to_binary(MemMaps)),
    %BackTraceJson = r2pipe:call("dbtj\n"),
    %erlamsa_logger:log(info, "r2monitor json backtrace ~s", [BackTraceJson]),
    BackTrace = r2pipe:call(R2Pid, "dbt\n"),
    erlamsa_logger:log(finding, "r2monitor backtrace ~s", [BackTrace]),
    r2pipe:stop(R2Pid),
    erlamsa_logger:log(info, "r2monitor debugging finished", []),
    ok.


start(Params) -> 
   Pid = spawn(?MODULE, init, [Params]),
   global:register_name(?MODULE, Pid),
   {ok, Pid}.

init(Params) ->
    {GenericMonOpts, LeftParams} = erlamsa_monitor:parse_after(string:tokens(Params, ",")),
    MonOpts = parse_params(LeftParams, GenericMonOpts),
    r2_start(MonOpts, 0).

r2_start(_MonOpts, N = ?START_MONITOR_ATTEMPTS) ->
    erlamsa_logger:log(info, "r2_monitor: too many failures (~p), giving up", [N]);
r2_start(MonOpts, N) ->
    %%TODO:handle wrong params
    R2Cmd = io_lib:format("~s -q0 -d ~s", [maps:get(r2path, MonOpts, "r2"), maps:get(app, MonOpts, "")]),
    case handle_r2_session(start, nil, R2Cmd) of %% TODO: do after only if res is ok
        ok ->
            erlamsa_monitor:do_after(MonOpts),
            r2_start(MonOpts, 0);
        error ->
            r2_start(MonOpts, N+1)
    end. %% TODO:ugly, rewrite

