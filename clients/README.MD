# Examples on how to interact with erlamsa

For advanced usage see https://github.com/Darkkey/erlamsa#using-as-a-service 

## TL;DR

```
$ cd erlamsa
$ ./erlamsa -H 127.0.0.1:17771 -D
$ cd clients/
$ go run erlamsa_go_client_json.go
Hello erlamsa from Go! erlamsed to safr G%0Dcurl localhost 51234%0Do!samsa rlamsa rlamHe
$ node erlamsa_node_client_json.js
Hello erlamsa! erlamsed to Hel rloelamsa!
$ node erlamsa_node_client.js
Hello erlamsa! erlamsed to a!Hello erlams+!Hello erlamsa!Hello erlamsa!Hello erlamsa!
$ python2 erlamsa_python_client.py
Hello erlamsa! erlamsed to lllle
$ pip3 install pyerlamsa
$ python3
...
>>> import pyerlamsa
>>> e = pyerlamsa.Erlamsa('http://127.0.0.1:17771')
>>> e.call('Hello erlamsa!');
(True, 'Hell4amsa15i685\x81\x91')
```
