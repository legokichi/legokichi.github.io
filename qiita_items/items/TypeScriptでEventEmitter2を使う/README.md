

```ts
/// <reference path="../typings/eventemitter2/eventemitter2.d.ts"/>

class Hoge extends EventEmitter2{
  constructor() {
    super();
    EventEmitter2.call(this);
  }
}

var hoge = new Hoge();
hoge.on("hoge", function(ev: string){
  console.log(ev);
});

hoge.emit("hoge", "hello");
```
