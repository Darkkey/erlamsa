var http = require('http');

var original_string = "Hello erlamsa!";

var options = {
  hostname: '127.0.0.1',
  port: 17771,
  path: '/erlamsa/erlamsa_esi:fuzz',
  method: 'POST',
  headers: {
       'Content-Length': original_string.length,
       'Content-Type': 'application/octet-stream'
     }
};

var http_req = http.request(options, function (http_res) {
  http_res.on('data', function (fuzzed_string) { 
    console.log(original_string + ' erlamsed to ' + fuzzed_string);
  });
});

http_req.on('error', function (e) {
  console.error(e);
});

http_req.write(original_string);
http_req.end();