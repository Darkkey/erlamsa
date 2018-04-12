-module(erlamsa_esi).
-export([fuzz/3]).

fuzz(Sid, Env, In) -> 
    InBin = list_to_binary(In),
    Dict = maps:put(paths, [direct],
               maps:put(output, return,
                maps:put(input, InBin,
                  maps:from_list(Env)))),    
    erlamsa_logger:log(info, "Request from IP ~p, session ~p", [maps:get(remote_adress, Dict, nil), Sid]),   
    erlamsa_logger:log_data(info, "Input data <session = ~p>", [Sid], InBin),
    Output = erlamsa_fsupervisor:get_fuzzing_output(Dict),
    erlamsa_logger:log_data(info, "Output data <session = ~p>", [Sid], Output),
    mod_esi:deliver(Sid, [Output]).
