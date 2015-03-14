-module(erlamsa_app).

-behaviour(application).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    erlamsa_sup:start_link().

stop(_State) ->
    ok.
