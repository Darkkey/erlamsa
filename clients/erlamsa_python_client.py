#!/usr/bin/python2.7
import httplib

erlamsa_url = '127.0.0.1:17771' 

original_string = "Hello erlamsa!"

httpconn = httplib.HTTPConnection(erlamsa_url)
headers = {"content-type": "application/octet-stream"}
httpconn.request('POST', '/erlamsa/erlamsa_esi:fuzz', original_string, headers)
response = httpconn.getresponse()

fuzzed_string = response.read()

print(original_string + " erlamsed to " + fuzzed_string)