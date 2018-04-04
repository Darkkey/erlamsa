-module(erlamsa_esi).
-export([fuzz/3]).

fuzz(Sid, Env, In) -> 
    Dict = maps:put(paths, [direct],
               maps:put(output, return,
                maps:put(input, list_to_binary(In),
                  maps:from_list(Env)))),    
    Output = erlamsa_fsupervisor:get_fuzzing_output(Dict),
    mod_esi:deliver(Sid, [Output]).
