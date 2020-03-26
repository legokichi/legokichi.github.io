https://www.irobot.com/~/media/mainsite/pdfs/about/stem/create/create_2_open_interface_spec.pdf

roomba 871

```c
#include <SoftwareSerial.h>;

SoftwareSerial swserial(10, 11);

void setup(){
  swserial.begin(115200);
  delay(1000);
  byte song[] = {
    128, // Start
    131, // Safe
    140, // Song
    0, // Song Number
    1, // Song Length
    64, // Note Number
    64, // Duratuin
  };
  swserial.write(song, 7);
  
}
void loop(){
  byte song[] = {
    128, // Start
    131, // Safe
    141, // Song
    0, // Song Number
  };
  swserial.write(song, 4);
  delay(1000);
}

```
