# erlamsa
Erlamsa -- "`<<smart>>` dumb fuzzer" aka erlang port of famous radamsa fuzzer.

## TL;DR

```
$ sudo apt-get install git gcc make erlang erlang-dev erlang-tools erlang-ssl erlang-eunit erlang-mnesia erlang-inets
$ git clone https://github.com/Darkkey/erlamsa && cd erlamsa && make
$ echo 'Hello erlamsa!' | ./erlamsa
```

## Features

- mutational-based fuzzing engine based on radamsa;
- support for fuzzing protocols over HTTP, TCP, UDP, and raw IP/network;
- built-in fuzzing proxy for MitMing and fuzzing connection(s) between target's client and server;
- FaaS (fuzzing-as-a-service) mode, plain HTTP and HTTP/json queries are supported;
- `<<smart>>` patterns and mutations, e.g. CRC and sizer fields detection, fuzzing of data inside archives, base64-encoded data decoding, e.t.c.;
- fuzzing for SSRFs, XXEs, ZIP path traversals;
- various monitors to detect crashes and loggers to store fuzzing data;
- support for external modules.

## Installation:

### Prerequisites

Git, Erlang. If you want to use raw IP output, gcc and make are required to build procket.

### Installing erlang

Erlamsa requires erlang/OTP 17.5+ to run.

Download erlang from official website http://www.erlang.org, install and put in path to `erlc`, `erl` and `escript` binaries into `PATH` variable. Or use your system's packet manager to install it.

On OS X (using homebrew):
```
brew install erlang
```

On CentOS/RHEL:
```
yum install erlang
```

On Debian/Ubuntu/Kali:
```
apt-get install erlang erlang-dev erlang-tools erlang-ssl erlang-eunit erlang-mnesia erlang-inets
```

### Building erlamsa    

Get the latest version of erlamsa:
``` 
git clone https://github.com/Darkkey/erlamsa
cd erlamsa
```

On Linux/OS X:
```
make 
```
or 
```
escript rebar co
```

On Windows (`escript.exe` and `erlc.exe` should be in `%PATH%`):
```
make_windows.bat
```

### Docker

Alternatively, you could use erlamsa Docker container: https://github.com/Darkkey/erlamsa-docker-image

## Standalone usage: 
Under Linux/OS X use `./erlamsa`, under Windows -- `erlamsa.bat`.

Few examples/templates for a quick start:
```
$ echo 'Hello erlamsa' | ./erlamsa
Hello erlmo erlamsa
$ erlamsa <<FILE TO FUZZ>>
$ erlamsa <<FILE TO FUZZ>> -o erlamsa <<FILE TO FUZZ>>.fuzzed
$ erlamsa <<FILE TO FUZZ>> -o tcp://:<<LISTEN_ON_PORT>>
$ cat <<BINARY WITH PACKET>> | ./erlamsa -o udp://<<HOST>>:<<PORT>>
```

For help try:
```
./erlamsa --help
```
or
```
escript erlamsa --help
```

## Using as a service ##

Launch as a service:
```
./erlamsa -H 127.0.0.1:17771 -L -
```

Or, in detached mode (erlamsa will go background after launch):
```
./erlamsa -H 127.0.0.1:17771 -D
```

HTTP POST your data to `http://<Host:Port>/erlamsa/erlamsa_esi:fuzz` as `application/octet-stream`. See examples in `clients/` folder (provided for C#, node.js and python). 
E.g. for Python 2.7:
```python
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

### JSON service endpoint

Erlamsa also provides JSON service extension, which allows to send request in JSON documents. Fuzzing data should be encoded in base64 format and provided inside `data` field of the document, e.g.:

```
$ curl -H "Content-Type: application/json" -X POST -d '{"data":"aGVsbG8="}' http://localhost:17771/erlamsa/erlamsa_esi:json
{"data": "bGxvbGxvaA=="}
```

### Fuzzing options

For standalone Web or JSON fuzzing, along with fuzzing data, you could also provide fuzzing options, including `mutations`, `patterns`, `seed`, `blockscale`, e.t.c. The format of these options should be the same as in according command line keys. Pass them as HTTP headers (for standalone Web service) or as JSON members (for JSON endpoint). E.g.:

```
$ curl -H "Content-Type: application/json" -X POST -d '{"data":"aGVsbG8=","seed": "1,2,3"}' http://localhost:17771/erlamsa/erlamsa_esi:json
{"data": "aGVsW2xv"}
```

## Using as fuzzing proxy
Erlamsa could be used a fuzzing "MiTM" proxy for TCP and UDP protocols. This allow to fuzz the communication between server and client, modifying packets coming from server to client, from client to server, or in both cases.

To use it as fuzzing proxy, run as:
```
./erlamsa -i proto://lport:[udpclientport:]rhost:rport -P probsc,probcs
```
where `probsc` and `probcs` are floats in range of 0.0 to 1.0, that represents probabilities of fuzzing packets from server to client and client to server.

E.g. erlamsa, that is started as
```
./erlamsa -i tcp://7777:192.168.0.1:7777 -P 0.1,0.7 -L -
```
will accept packets on port `7777` (on all network interfaces, basically on 0.0.0.0 interface) and send them to host's `192.168.0.1` port `7777`. All packets coming from server to client will be fuzzed with probability of `0.1`(10%), from client to server -- with probability of `0.7`(70%). In this case, to start fuzzing just point your client application to erlamsa's host and port `7777`. `-L -` options means that all logging will be output to stdout.

## Example usage from erlang code

There are two possible ways to call erlamsa from your code: static (same host, library call) and dynamic (query the application on another node)

1) Static direct usage: see `erlamsa_app:fuzz/1` and `erlamsa_app:fuzz/2`, e.g.:
```
$ erl -pa ebin -pa deps/*/ebin
...
Eshell V10.0.3  (abort with ^G)

1> erlamsa_app:fuzz(<<"123">>).
<<243,160,128,169,49,50,51>>
2>
```

2) Remotely, on another node:

    a) Start and test fuzzing node:
    ```
    $ erl -pa ebin -pa deps/*/ebin -sname erlamsa
    ...

    (erlamsa@server)1> erlamsa_app:start(direct, maps:new()).
    ok
    (erlamsa@server)2> erlamsa_app:call(erlamsa, <<"123">>).
    <<"12311223123">>
    (erlamsa@server)3>
    ```
    b) Run on client node: 
    ```
    $ erl -pa ebin -pa deps/*/ebin -sname client
    ...

    (client@user)1> erlamsa_app:start(remote, {erlamsa, 'erlamsa@server'}).
    {test,<0.66.0>}
    (client@user)2> erlamsa_app:call(erlamsa, <<"321">>).
    <<"3321">>
    ```

## External (extension) scripts

Erlamsa could use external scripts for generation-based fuzzing, custom fuzzing, custom post-fuzzing procedures or custom mutation types. Module should be compiled with `erlc` before using and passed using `-e` option to erlamsa. Due to internal mechanism of external modules inclusion, you should pass external module ALWAYS as a FIRST argument to erlamsa.

In the following example, erlamsa is run with custom mutation module (see `external_muta.erl` for details):
```
$ echo -n 123 | ./erlamsa -e external_muta -m pan=1 -p od
<pancaked>123</pancaked>
```

### Custom fuzzing extension

Could be using in fuzzing proxy mode, to replace standard fuzzing procedure with a custom one. See `external_test.erl` for example template.

### Custom mutation extension

Adds custom mutation to the list of erlamsa mutators. See `external_muta.erl` for example. 

### Custom post-processing extension

Adds post-processing procedure that will be applied to fuzzing result just before output. Useful for fixing checksum or form of data to avoid unnessesary noise. See `external_nhrp.erl` for example.

## Monitors

Erlamsa is using monitors, special modules that are intended to detect various events, like application crashes, freezes, SSRFs, backconnects, e.t.c. Monitor is waiting for some event,
and, upon receiving it, reports to the logs and execute specified after actions. Each monitor is a separate process, that could be initiated via passing `-O name:params` in the command line. By default, `cm` (connect monitor) is run. To disable monitors that are run by default, one could pass `-O -monitor_name:` in the command line. E.g., to disable connection monitor, pass `-O -cm:` as an command line option to erlamsa.

### Monitor parameters

Parameters are passed to monitors in a form of comma-delimeted list, e.g.: `-O monitor_name:param1=value1,...`. Parameters are split to the generic ones and monitor-specific ones. Generic ones include:

* `after=` -- specifies the type of after-action that will be performed after monitor event is triggered. After-action options are specified in the `after_params=` monitor parameter. Below, there is a list of currently supported after actions:
    - `exec` -- executes specific process or shell script right after the monitor event is triggered. E.g., the following command line forces to restart run script `hello.sh` after connection event was triggered: `-O cm:after=exec,after_params=hello.sh`.

### Monitors list

Below, it could be found all currently supported by erlamsa monitors and corresponding parameters:

* `cm` -- connection event monitor. Run by default when eramsa is started. Listen on the endpoint specificed by `host=` option and `port=` option (default is `51234`). Upon recieving the connection, reports incoming data to the logs, and wait for connection is being closed by client or a certaing timeout (specified by `timeout=` option, default is 5000 (5 seconds)). This event monitor is intended to catch SSRFs, XXEs or backconnect events that could be generated during the fuzzing.

* `cdb` -- monitor for CDB Windows debugger. This monitor is attaching to / running fuzzed process and catches potential crashes. Upon catching a crash, it reports it to the logs and writes minidump file. Only for Windows platform. Supported options includes:
    - `app=` -- runs executable file to be monitored; application will be automatically restarted after crash;
    - `pid=` or `attach=` -- attaches to the process by pid or its name.

* `lc` -- monitor for adb/logcat crash detection for Android applications. This monitor is runs and monitors fuzzed process and catches potential crashes. Upon catching a crash, it reports data that appeared in the Android log. Supported options includes:
    - `app=` -- application name to be run and monitored;
    - `activity=` -- application activity to be invoked upon application run;
    - `adbpath=` -- path to adb executable.

* `r2` -- monitor for Radare2 debugger. This monitor is running fuzzed process and catches potential crashes. Upon catching a crash, it reports backtrace and registers to the logs. Only for Linux/OS X platforms. Supported options includes:
    - `app=` -- runs executable file to be monitored; application will be automatically restarted after crash;
    - `r2path=` -- path to r2 executable.

## Platform limitations

* on unix platforms (Linux, OS X, ...) erlamsa could not be launched in background mode using standard `&` shell symbol due to erlang VM limitations; `-D` option is intended for it. E.g., instead of `./erlamsa -H 127.0.0.1:17771 &` use `./erlamsa -H 127.0.0.1:17771 -D`
* `ip://` and `raw://` outputs are not working on Windows, `raw://` output is not working on OS X
* minimum recommended RAM to run on Windows OS is 4Gb
* erlamsa spawns separate process upon startup, to avoid this behaviour (and save some performance) you could use `eerlamsa`/`eerlamsa.bat` scripts.

## Code Status

[![Build Status](https://travis-ci.org/Darkkey/erlamsa.svg?branch=master)](https://travis-ci.org/Darkkey/erlamsa)
