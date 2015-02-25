-module(reload).
-author("dark_k3y").

%% API
-export([rl/0]).


rl() ->
    Modules = [M || {M, P} <- code:all_loaded(), is_list(P)
        andalso M =/= reload
        andalso string:str(P, "Research") > 0],
    [c:l(M) || M <- Modules].