#!/usr/bin/python3
import frida
import sys
import base64
import http.client

erlamsa_url = '127.0.0.1:17771'
strncpy_address = '0xdf0'

def call_erlamsa(data):

    original_string = data

    httpconn = http.client.HTTPConnection(erlamsa_url)
    headers = {"content-type": "application/octet-stream"}
    httpconn.request('POST', '/erlamsa/erlamsa_esi:fuzz', original_string, headers)
    response = httpconn.getresponse()

    fuzzed_string = response.read()

    return fuzzed_string

def main(target_process, strncpy_address):
    pid = frida.spawn([target_process])
    session = frida.attach(pid)

    script_file = open("example.js", "r") 

    script = session.create_script(script_file.read() % int(strncpy_address, 16))

    def on_message(message, data):
        new_data = call_erlamsa(data)[:11]
	
        print(str(data) + ' erlamsed to ' + str(new_data))

        script.post(message = {'type': 'input'}, data = new_data)

    script.on('message', on_message)
    script.load()

    frida.resume(pid)

    print("[!] Ctrl+D on UNIX, Ctrl+Z on Windows/cmd.exe to detach from instrumented program.\n\n")
    sys.stdin.read()
    session.detach()

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: %s <executable file> [strncpy function address]" % __file__)
        sys.exit(1)

    target_process = sys.argv[1]
    if len(sys.argv) == 3:
        strncpy_address = sys.argv[2]

    main(target_process, strncpy_address)
