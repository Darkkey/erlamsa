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

On Windows (`escript.exe` should in `%PATH%`):
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

## Using as service ##

Launch as service:
```
erlamsa -H 127.0.0.1:17771
```

HTTP POST your data to `http://<Host:Port>/erlamsa/erlamsa_esi:fuzz` as `application/octet-stream`. See examples in clients/ folder (provided for C#, node.js and python). 
E.g. for Python 2.7:
```
import httplib

erlamsa_url = '127.0.0.1:17771'

original_string = "Hello erlamsa!"

httpconn = httplib.HTTPConnection(erlamsa_url)
headers = {"content-type": "application/octet-stream"}
httpconn.request('POST', '/erlamsa/erlamsa_esi:fuzz', original_string, headers)
response = httpconn.getresponse()

fuzzed_string = response.read()

print(original_string + " erlamsed to " + fuzzed_string)
```

Result:
```
$ python clients/erlamsa_python_client.py
Hello erlamsa! erlamsed to rlamsa!rlallo eHello e
```

## Code Status

[![Build Status](https://travis-ci.org/Darkkey/erlamsa.svg?branch=master)](https://travis-ci.org/Darkkey/erlamsa)
