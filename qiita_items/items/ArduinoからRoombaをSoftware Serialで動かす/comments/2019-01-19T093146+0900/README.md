```c
#include <SoftwareSerial.h>;

SoftwareSerial swserial(10, 11);

void set_song(byte n, byte buf[], byte len){
  byte song_header[] = {
    128, // Start
    131, // Safe
    140, // Song
    n, // Song Number
    len/2, // Song Length
  };
  swserial.write(song_header, 5);
  swserial.write(buf, len);
}

void play_song(byte n){
  byte play[] = {
    128, // Start
    131, // Safe
    141, // Play
    n, // Song Number
  };
  swserial.write(play, 4);
}

void intro1(byte n){
  byte song_body[]= {
    74, 16,
    76, 16,
    0, 16*2,
    
    77, 16,
    76, 16,
    0, 16*2,
    
    74, 16,
    81, 16*2,
    81, 16,
    
    81, 16,
    80, 16,
    0, 16*2,
  };
  set_song(n, song_body, 12*2);
}

void intro2(byte n){
  byte song_body[]= {
    84, 16*2,
    84, 16*2,
    
    81, 16*2,
    81, 16*2,
    
    82, 16*2,
    82, 16*2,
    
    79, 16*2,
    79, 16*2,
    
    81, 16*2,
    81, 16*2,
    
    77, 16*2,
    77, 16*2,

    79, 16*2,
    79, 16*2,

    76, 16*2,
    76, 16*2,
  };
  set_song(n, song_body, 16*2);
}

void intro3(byte n){
  byte song_body[]= {
    77, 16,
    76, 16,
    74, 16,
    0, 16,
    
    0, 16*2,
    77, 16*2,
    
    74, 16,
    0, 16*3,

    0, 16*4,
  };
  set_song(n, song_body, 9*2);
}

void main_theme1(byte n){
  byte song_body[]= {
    74, 16*2,
    81, 16*2,
    
    81, 16*2,
    79, 16*2,
    
    77, 16*2,
    76, 16*2,
    
    74, 16,
    72, 16*2,
    72, 16,
    
    72, 16*3,
    74, 16,

    74, 16*5,
    0, 16*3,

    0, 16*4,
  };
  set_song(n, song_body, 14*2);
}

void main_theme2(byte n){
  byte song_body[]= {
    74, 16*2,
    81, 16*2,
    
    81, 16*2,
    81, 16*2,
    
    84, 16*2,
    82, 16*2,
    
    81, 16*2,
    79, 16,
    79, 16,

    77, 16*2,
    79, 16*2,
    
    81, 16,
    82, 16*2,
    81, 16*5,

    0, 16*4,
  };
  set_song(n, song_body, 15*2);
}

void chorus1(byte n){
  byte song_body[]= {
    86, 16*2,
    86, 16*2,
    
    81, 16*2,
    81, 16*2,

    77, 16*4,
    
    81, 16*3,
    0, 16,

    86, 16*2,
    86, 16*2,
    
    82, 16*2,
    82, 16*2,

    79, 16*4,
    
    82, 16*3,
    0, 16,
  };
  set_song(n, song_body, 14*2);
}

void end1(byte n){
  byte song_body[]= {
    86, 16,
    0, 16,
    81, 16,
    0, 16,

    0, 16*2,
    86, 16,
    0, 16,
    
    84, 16,
    0, 16,
    0, 16*2,

    84, 16,
    86, 16,
    87, 16,
    87, 16,

    86, 16,
    0, 16,
  };
  set_song(n, song_body, 16*2);
}

void end2(byte n){
  byte song_body[]= {
    84, 16,
    86, 16,
    87, 16,
    87, 16,

    86, 16,
    0, 16,
  };
  set_song(n, song_body, 6*2);
}

void tril1(byte n){
  byte song_body[]= {
    84, 16,//1 mo ku shi
    86, 16,
    87, 16,
    84, 16,//2 ku shi mo
    
    86, 16,
    87, 16,
    84, 16,//3 shi mo ku
    86, 16,
    
    87, 16,
    84, 16,//4 ku mo shi
    86, 16,
    87, 16,
    
    84, 16,//5 mo shi ku
    86, 16,
    87, 16,
    84, 16,//6 shi ku mo
  };
  set_song(n, song_body, 16*2);
}

void tril2(byte n){
  byte song_body[]= {
    86, 16,
    87, 16,
    0, 16*2,
    
    0, 16*4
  };
  set_song(n, song_body, 3*2);
}

void setup(){
  swserial.begin(115200);

  intro1(0);
  intro1(1);
  intro2(2);
  intro3(3);
  delay(1000);
  // intro
  play_song(0);
  delay(4100);
  play_song(1);
  delay(4100);
  play_song(2);
  delay(8100);
  play_song(3);
  delay(4100);
  // intro
  play_song(0);
  delay(4100);
  play_song(1);
  delay(4100); 
  play_song(2);
  delay(8100); main_theme1(0);
  play_song(3);
  delay(4100);
  // main
  play_song(0);
  delay(8100); main_theme2(1);
  play_song(1);
  delay(8100); chorus1(2);
  // main
  play_song(0);
  delay(8100); end1(3);
  play_song(1);
  delay(8100); end2(0);
  // chorusl
  play_song(2); 
  delay(8100); tril1(1);
  play_song(2);
  delay(8100); tril2(2);
  // end
  play_song(3);
  delay(5000+50);
  play_song(3);
  delay(4500+50);
  play_song(0);
  delay(1500+50);
  // tril
  for(int i=0; i<9; i++){
    play_song(1);
    delay(4050);
    play_song(2);
    delay(2100);
  }
}


void loop(){
}

```
