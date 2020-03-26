```c
#include <SoftwareSerial.h>;
SoftwareSerial device(10, 11);

void setup(){
  Serial.begin(115200);
  device.begin(115200);
}

void loop(){
  int i = Serial.available();
  while(i--){
    device.write(Serial.read());
  }
  int j = device.available();
  while(j--){
    Serial.write(device.read());
  }
}
```

```json
{
  "name": "node-roomba",
  "version": "1.0.0",
  "description": "",
  "main": "index.js",
  "scripts": {
    "test": "echo \"Error: no test specified\" && exit 1"
  },
  "author": "",
  "license": "ISC",
  "dependencies": {
    "serialport": "^7.1.3"
  }
}
```

```js
const SerialPort = require("serialport");

async function main(){
    const ports = await SerialPort.list();
    const arduinoPorts = ports.filter((o)=> o.manufacturer === "Arduino (www.arduino.cc)");
    if (arduinoPorts.length < 1){
        throw new Error("arduino not found");
    }
    console.log(arduinoPorts[0]);
    const serial = new SerialPort(arduinoPorts[0].comName, {
        baudRate: 115200,
    });
    serial.on('open', console.info.bind(console, "open"));
    serial.on('error', console.error.bind(console, "error"));
    serial.on('close', console.info.bind(console, "close"));
    serial.on('data', console.info.bind(console, "data"));
    serial.on('drain', console.info.bind(console, "drain"));
    await new Promise((resolve, reject)=> serial.on('open', resolve));
    console.log(serial.isOpen);
    await new Promise((resolve)=> setTimeout(resolve, 1000));
    
    let i = 31;
    while(true){
        if(i > 107) { i = 31; }
        serial.write([
            128, // Start
            131, // Safe
            140, // Song
            0, // Song Number
            1, // Song Length
            i++, // Note Number
            8, // Duration
        ]);
        serial.write([
            128, // Start
            131, // Safe
            141, // Song
            0, // Song Number
        ]);
        serial.once("data", (buf)=>{
            if (buf.length > 0){
                const byte = buf[0];
                const bumpRight = (byte & 0b00000001) > 0;
                const bumpLeft = (byte & 0b00000010) > 0;
                const wheelDropRight = (byte & 0b00000100) > 0;
                const wheelDropLeft = (byte & 0b00001000) > 0;
                console.log({bumpLeft, bumpRight, wheelDropLeft, wheelDropRight});
            }
        });
        serial.write([
            128, // Start
            131, // Safe
            142, // Sensor
            7, // Bumps and Wheel Drops
        ]);
        await new Promise((resolve)=> setTimeout(resolve, 1000));
    }

    while(true){
        serial.write([
            173, // Stop
        ]);
        await new Promise((resolve)=> setTimeout(resolve, 1000));
    }
}

main().catch(console.error);
```
