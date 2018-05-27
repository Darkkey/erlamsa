'use strict';

var baseAddr = Module.findBaseAddress('example');
console.log('software main executable baseAddr: ' + baseAddr);

var strncpy = resolveAddress('0xdf0'); 

var fuzzed_data;
var dst_addr;
var dst_len = 0;


Interceptor.attach(strncpy, {

    onEnter: function (args) {
        console.log('[+] Called strncpy @' + strncpy);
        console.log('[+] Dest: ' + args[0]);
        dst_addr = args[0]
        console.log('[+] Src: ' + args[1]); 
        console.log('[+] Len: ' + args[2]); 
        dst_len = args[2].toInt32()
        if(args[2].toInt32() > 0){
            dumpAddr('Input', args[1], args[2].toInt32());
            var buf = Memory.readByteArray(args[1], args[2].toInt32());
            send('data', buf);
            var op = recv('input', function(value, data) {
                fuzzed_data = data
            });
            op.wait();
        }
    },

    onLeave: function (retval) {
        if(dst_len > 0){
            Memory.writeByteArray(dst_addr, fuzzed_data);	
            dumpAddr('Input after fuzzing', dst_addr, dst_len);
        }

        console.log('[+] Returned from strncpy: ' + retval);
        console.log('');
    }
});

function dumpAddr(info, addr, size) {
    if (addr.isNull())
        return;

    console.log('Data dump ' + info + ' :');
    var buf = Memory.readByteArray(addr, size);

    console.log(hexdump(buf, { offset: 0, length: size, header: true, ansi: false }));
}

function resolveAddress(addr) {
    var offset = ptr(addr); 
    var result = baseAddr.add(offset); 
    console.log('[+] New addr=' + result); 
    return result;
}
