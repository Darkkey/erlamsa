# erlamsa
Erlang port of famous radamsa fuzzzer.

## Build: 
```
make 
```
or 
```
escript rebar co
```

## Standalone usage: 
```
escript erlamsa_cmd.erl --help
```
## Example usage from erlang code
```
-spec fuzz(binary()) -> binary().
fuzz(Data) -> 
    Opts = maps:put(paths, [direct],
            maps:put(output, return,
             maps:put(input, Data, maps:new()))),
    MutatedData = erlamsa_utils:extract_function(erlamsa_main:fuzzer(Opts)),
    MutatedData.
```