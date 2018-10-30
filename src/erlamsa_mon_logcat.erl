-module(erlamsa_mon_logcat).           

-include("erlamsa.hrl").

-export([start/1, init/1]).

parse_params(["app=" ++ App|T], Acc) ->
    parse_params(T, maps:put(app, App, Acc));
parse_params(["activity=" ++ App|T], Acc) ->
    parse_params(T, maps:put(activity, App, Acc));
parse_params(["adbpath=" ++ R2Path|T], Acc) ->
    parse_params(T, maps:put(adbpath, R2Path, Acc));
parse_params([_H|T], Acc) ->
    %%TODO: do it in more elegant way
    io:format("Invalid monitor parameter: ~p, skipping...", [T]),
    parse_params(T, Acc);
parse_params([], Acc) ->
    Acc.



start(Params) -> 
   Pid = spawn(?MODULE, init, [Params]),
   {ok, Pid}.

init(Params) ->
    {GenericMonOpts, LeftParams} = erlamsa_monitor:parse_after(string:split(Params, ",", all)),
    MonOpts = parse_params(LeftParams, GenericMonOpts),
    logcat_start(MonOpts, 0).

parse_crash_data_line(State, Data, AppPid, Acc) ->
    Tokens = string:split(Data, " ", all),
    case {Tokens, State} of
        {[_, _, _, _, "F", "DEBUG", [], [], ":", "***" | _T], listen} ->
            {crash_start, Acc};
        {[_, _, _, _, "F", "DEBUG", [], [], ":", "pid:", CrashPid | _T], crash_start} ->
            io:format("~p ? ~p~n", [AppPid, list_to_integer([X || X <- CrashPid, X =/= $,])]),
            case list_to_integer([X || X <- CrashPid, X =/= $,]) of
                AppPid ->
                    io:format("Got it!~n"),
                    {crash_ongoing, [Data | Acc]};
                _ElsePid ->
                    {listen, []}
            end;
        {[_, _, _, _, "F", "DEBUG" | _T], St} when St =/= listen ->
            {St, [Data | Acc]};
        {_NonCrashLine, crash} -> 
            {crash_complete, lists:reverse(Acc)};
        _ElseData -> 
            {listen, Acc}
    end.

get_crash_data(State, [H|T], AppPid, Acc) ->
    {NewState, NewAcc} = parse_crash_data_line(State, H, AppPid, Acc),
    get_crash_data(NewState, T, AppPid, NewAcc);
get_crash_data(State, [], AppPid, Acc) ->
    {State, Acc}.

wait_for_crash(LCPid, AppPid, State, Acc) ->
    receive 
        {stream_data, LCPid, Data} ->
            case get_crash_data(State, string:split(Data, [10], all), AppPid, Acc) of
                {crash_complete, Res} ->
                    io:format("Crash catched: ~p~n~n~n~n~n~n~n", [Res]),
                    Res;
                {NewState, NewAcc} ->            
                    wait_for_crash(LCPid, AppPid, NewState, NewAcc)
            end
    end.

parse_ps_output([]) -> 0;
parse_ps_output([_, _, _, Pid | _T]) ->
    list_to_integer(Pid);
parse_ps_output(_) -> 0.

handle_lc_session(start, nil, App, Activity, _Param) ->
    Str = os:cmd(io_lib:format("adb shell 'ps | grep ~s'", [App])),
    CheckRunning = parse_ps_output(string:split(Str, " ", all)),
    erlamsa_logger:log(info, "logcat_monitor checking whether ~s app is running", [App]),
    handle_lc_session(exec, CheckRunning, App, Activity, _Param);
handle_lc_session(exec, 0, _App, _Activity, ?START_MONITOR_ATTEMPTS) ->
    error;
handle_lc_session(exec, 0, App, Activity, N) ->
    Cmd = io_lib:format("adb shell am start -n ~s/.~s", [App, Activity]),
    _RunAttempt = os:cmd(Cmd),
    erlamsa_logger:log(info, "logcat_monitor attempting to run: '~s'", [Cmd]),
    handle_lc_session(start, nil, App, Activity, N + 1);
handle_lc_session(exec, AppPid, _App, _Activity, _N) ->
    erlamsa_logger:log(info, "logcat_monitor app pid = '~p'", [AppPid]),
    LCPid = erlamsa_simple_port:start("adb logcat *:E", []),
    erlamsa_simple_port:subscribe(LCPid),
    Crash = wait_for_crash(LCPid, AppPid, listen, []),
    ok.                    

logcat_start(_MonOpts, N = ?START_MONITOR_ATTEMPTS) ->
    erlamsa_logger:log(info, "logcat_monitor: too many failures (~p), giving up", [N]);
logcat_start(MonOpts, N) ->
    %% TODO: handle exceptions
    case handle_lc_session(start, nil, maps:get(app, MonOpts), maps:get(activity, MonOpts), 0) of 
        ok ->
            erlamsa_monitor:do_after(MonOpts),
            logcat_start(MonOpts, 0);
        error ->
            logcat_start(MonOpts, N+1)
    end. %% TODO:ugly, rewrite

