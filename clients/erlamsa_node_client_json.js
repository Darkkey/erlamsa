var http = require('http');

var original_string = "Hello erlamsa!";

var json_object = 
    JSON.stringify(
        {"data": Buffer.from(original_string).toString("base64"),
	 "seed": "123,123,123",
         "pattern": "od"}
        );

var options = {
  hostname: '127.0.0.1',
  port: 17771,
  path: '/erlamsa/erlamsa_esi:json',
  method: 'POST',
  headers: {
       'Content-Length': json_object.length,
       'Content-Type': 'application/json"'
     }
};

var http_req = http.request(options, function (http_res) {
  http_res.on('data', function (fuzzed_string) { 
    json_reply = JSON.parse(fuzzed_string);
    result_string = Buffer.from(json_reply.data, 'base64').toString('ascii');
    console.log(original_string + ' erlamsed to ' + result_string);
  });
});

http_req.on('error', function (e) {
  console.error(e);
});

http_req.write(json_object);
http_req.end();
