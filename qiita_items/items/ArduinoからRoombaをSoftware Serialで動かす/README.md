# 今回すること

Roomba620をSoftware serial経由でArduino UNO R3につなぎ、Serial経由でPCから操作します。
ROI用のMini-DINケーブルを自作する必要がないのでとりあえず動かしてみるのには便利です。
ROIとは何ぞやという方は[この辺](http://www.teamknox.com/Roomba/Roombaj.html)とか読んどきましょう。

# RoombaとArduinoをつなぐ
[Roomba Open Interface](http://www.irobot.lv/uploaded_files/File/iRobot_Roomba_500_Open_Interface_Spec.pdf)の3ページ目にピンの説明載ってます。

![Roomba's External Serial Port Mini-DIN Connector Pinout](http://i.gyazo.com/58fba8586263286646ba1c4a5527866d.png)

今回使うのは3,4,6ピンです。
3番ピンはRoomba側の受信(RXD)ポートです。Arduino側の送信(TXD)ポートとつなぎます。
4番ピンはRoomba側の送信(TXD)ポートです。Arduino側の受信(RXD)ポートとつなぎます。
6番ピンはグラウンド(GND)です。Arduino側のGNDとつなぎます。

ArduinoとRoombaだけで通信する場合はArduinoのSerialポートである0番(RX)、1番(TX)をRoombaとつなげばよいです。
しかし、今回はPCからArduinoを操作し、ArduinoからRoombaを操作したいので、この手は使えません。
なぜなら、ArduinoとPCをUSB経由でSerial通信するときも、これらのポートが使われてしまうからです。
そこで、ArduinoのSoftwareSerialという機能を使います。
これはSerial用ではない0番と1番以外のポートを使って擬似Serial通信をするものです。

![2014-07-02 22.43.54.jpg](http://i.gyazo.com/8e2b8ece2e63524fd48b2575c34d814c.png)

今回はSoftwareSerial用ポートとしてRXをArduinoの10番ポートに、TXを11番ポートに設定します。
ROI資料の4ページによるとRoombaは初期状態で115200ボーで通信を受け付けていますのでそのように設定します。

```arduino
#include <SoftwareSerial.h>;
SoftwareSerial device(10, 11);
void setup(){
  device.begin(115200);
}
void loop(){}
```

これで準備完了です。ROIコマンドを送ってみましょう。

```
#include <SoftwareSerial.h>;
SoftwareSerial device(10, 11);
void setup(){
  device.begin(115200);
  byte buffer[] = {
    byte(128), // Start
    byte(135)  // Clean
  };
  device.write(buffer, 2);
}
void loop(){}
```

これでRoombaはCleanモードに入ります。

ではRoombaを自在に動かしてみましょう。

```arduino
#include <SoftwareSerial.h>;
SoftwareSerial device(10, 11);
void setup(){
  device.begin(115200);
}

void loop(){
  motor(64, -64);
  delay(250);
  motor(-64, 64);
  delay(250);
}

void motor(int l, int r){
  byte buffer[] = {
    byte(128), // Start
    byte(132), // FULL
    byte(146), // Drive PWM
    byte(r>>8),
    byte(r),
    byte(l>>8),
    byte(l)
  };
  device.write(buffer, 7);
}
```

このプログラムはRoombaが左右に身を振り「イヤイヤ」をする例です。

# ArduinoとPC(Processing)をつなぐ

ここまででRoombaをArduinoからSoftware Serialを使って操作することができました。
次はArduinoをPCのProcessingからUSBを通じたSerial通信で操作しましょう。

```processing
#include <SoftwareSerial.h>;
SoftwareSerial device(10, 11);

void setup(){
  Serial.begin(9600);
  device.begin(115200);
}

void loop(){
  if(Serial.available() > 0){
    int cmd = Serial.read();
    Serial.write(cmd);
    switch(cmd){
      case 48: motor( 64,  64); break; // forward
      case 49: motor( 64, -64); break; // turnRight
      case 50: motor(-64,  64); break; // turnLeft
      case 51: motor(-64, -64); break; // back
      default: motor(  0,   0); break; // stop
    }
  }else{
    motor(0, 0);
  }
  delay(100);
}

void motor(int l, int r){
  byte buffer[] = {
    byte(128), // Start
    byte(132), // FULL
    byte(146), // Drive PWM
    byte(r>>8),
    byte(r),
    byte(l>>8),
    byte(l)
  };
  device.write(buffer, 7);
}
```
ArduinoへSerialで48というバイト値を送ると前進、49で右旋回、50で左旋回、51で後退、それ以外だと停止させるプログラムです。

PCのProcessingのプログラムについての説明は割愛します。
CなりProcessingなりPythonなりnodeなりで各自作ってください。
