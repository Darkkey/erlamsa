# erlamsa from frida example

This is a simple example that demonstrates how to send data from frida to erlamsa and push back result of fuzzing. 

## TL;DR

```
$ cd erlamsa
$ ./erlamsa -H 127.0.0.1:17771 &
$ cd clients/frida
$ make
$ python3 erlamsa_from_frida.py ./example
software main executable baseAddr: 0x10a570000
[+] New addr=0x10a570df0
[!] Ctrl+D on UNIX, Ctrl+Z on Windows/cmd.exe to detach from instrumented program.


[+] Called strncpy @0x10a570df0
[+] Dest: 0x7fff5568f8de
[+] Src: 0x7fff5568fa50
[+] Len: 0xa
Data dump Input :
           0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F  0123456789ABCDEF
00000000  2e 2f 65 78 61 6d 70 6c 65 00                    ./example.
b'./example\x00' erlamsed to b'./%yempme\x00'
Data dump Input after fuzzing :
           0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F  0123456789ABCDEF
00000000  2e 2f 25 79 65 6d 70 6d 65 00                    ./%yempme.
[+] Returned from strncpy: 0x7fff5568f8de

...
```

## Note
You may need to update the address of `my_strncpy` in `var strncpy = resolveAddress('0xdf0');` as it may change depending on your platform/compiler version. This address could be extracted using e.g. radare2:
```
$ r2 example
[0x100000e30]> aa
[x] Analyze all flags starting with sym. and entry0 (aa)
[0x100000e30]> afl | grep my_strncpy
0x100000df0    1 49           sym._my_strncpy
[0x100000e30]>
```