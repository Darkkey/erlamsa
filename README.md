# erlamsa
Erlang port of famous radamsa fuzzzer.

## Build: 
Requires erlang OTP 17.5+. 

On Linux/OS X:
```
make 
```
or 
```
escript rebar co
```

On Windows (escript.exe should in %PATH%):
```
make_windows.bat
```

## Standalone usage: 
```
erlamsa --help
```

or

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

## Code Status

[![Build Status](https://travis-ci.org/Darkkey/erlamsa.svg?branch=master)](https://travis-ci.org/Darkkey/erlamsa)
