
## tokio-core

今後使うことはもうないであろう tokio-core のタイムアウトの例です
メインスレッドでイベントループを回しています。


```rust
extern crate futures;
#[macro_use]
extern crate mdo;
extern crate mdo_future;
extern crate tokio_core;

use std::time::Duration;
use futures::prelude::*;
use mdo_future::future::*;
use tokio_core::reactor::Core;
use tokio_core::reactor::Timeout;

fn main() {
    println!("tokio_core");
    let mut core = Core::new().unwrap();
    let handle = core.handle();
    core.run(mdo!{
        _ =<< Timeout::new(Duration::from_secs(1), &handle).unwrap();
        let _ = println!("1");
        _ =<< Timeout::new(Duration::from_secs(1), &handle).unwrap();
        let _ = println!("2");
        _ =<< Timeout::new(Duration::from_secs(1), &handle).unwrap();
        let _ = println!("3");
        ret ret(())
    }.map_err(|err| println!("{:?}", err) ) ).unwrap();
    println!("end");
}
```

## tokio runtime

開発中の tokio runtime の tokio-timer を使った実装です。

```rust
extern crate futures;
#[macro_use]
extern crate mdo;
extern crate mdo_future;
extern crate tokio;
extern crate tokio_timer;

use std::time::Duration;
use futures::prelude::*;
use mdo_future::future::*;

fn main(){
    println!("tokio runtime");
    tokio::run(mdo!{
        _ =<< ::tokio_timer::sleep(Duration::from_secs(1));
        let _ = println!("1");
        _ =<< ::tokio_timer::sleep(Duration::from_secs(1));
        let _ = println!("2");
        _ =<< ::tokio_timer::sleep(Duration::from_secs(1));
        let _ = println!("3");
        ret ret(())
    }.map_err(|err| println!("{:?}", err) ));
    println!("end");
}
```

## actix 0.5

もう使うことはないであろう actix 0.5 の例。
actix 0.5 は内部では tokio-core を使っています。
tokio runtime が出る以前に開発がスタートしたので内部でアクター実行のための独自 runtime を持っています（詳しくないです）
開発中の actix 0.6 は tokio runtime を使っているので API も変わっています。

システムアクターのイベントループで timeout を使う例です。
Arbiter を介して tokio-core のイベントループが動いてるスレッドのリモートハンドラを取得して使います

```rust
extern crate futures;
#[macro_use]
extern crate mdo;
extern crate mdo_future;
extern crate tokio_core;
extern crate actix;

use std::time::Duration;
use futures::prelude::*;
use mdo_future::future::*;
use actix::prelude::*;
use tokio_core::reactor::Timeout;

fn main(){
    println!("actix 0.5");
    let sys = System::new("system");
    Arbiter::handle().spawn(mdo!{
        _ =<< Timeout::new(Duration::from_secs(1), Arbiter::handle()).unwrap();
        let _ = println!("1");
        _ =<< Timeout::new(Duration::from_secs(1), Arbiter::handle()).unwrap();
        let _ = println!("2");
        _ =<< Timeout::new(Duration::from_secs(1), Arbiter::handle()).unwrap();
        let _ = println!("3");
        ret ret(Arbiter::system().do_send(actix::msgs::SystemExit(0)))
    }.map_err(|err| println!("{:?}", err) ) );
    sys.run();
    println!("end");
}
```


## actix 0.6

actix 0.6 は内部で tokio runtime を使ってるので tokio runtime の API がそのまま使えます。

```rust
extern crate futures;
#[macro_use]
extern crate mdo;
extern crate mdo_future;
extern crate tokio;
extern crate tokio_timer;
extern crate actix;

use std::time::Duration;
use futures::prelude::*;
use mdo_future::future::*;
use actix::prelude::*;

fn main(){
    println!("actix 0.6");
    System::run(|| {
        tokio::spawn(mdo!{
            _ =<< ::tokio_timer::sleep(Duration::from_secs(1));
            let _ = println!("1");
            _ =<< ::tokio_timer::sleep(Duration::from_secs(1));
            let _ = println!("2");
            _ =<< ::tokio_timer::sleep(Duration::from_secs(1));
            let _ = println!("3");
            ret ret(System::current().stop())
        }.map_err(|err| println!("{:?}", err) ));
    });
    println!("end");
}
```


## 非同期 IO のフレンズ紹介と感想

ここに出てくるライブラリの知見は年末までには様変わりしていそうで諸行無常

* futures 0.1 : tokio-core と合わせて開発された mio を利用したゼロコストフューチャーライブラリ
    * Rust 2018 で標準ライブラリに future が入るので死が宣告されている
* mdo-future : モナドの do 構文を実現するためのシンプルなマクロライブラリ mdo を使ってfutures の and_then ネストを軽減するライブラリ
    * Rust 2018 で言語に async-await 構文が入るの死が宣告されている
* tokio-core : 2016-2017 年に開発されていた非同期IOのためのシングルスレッドイベントループライブラリ
    * tokio runtime の登場で死が宣告されている
* tokio runtime : 2018 年に開発がスタートした非同期IOのためのマルチスレッドイベントループライブラリ
    * Rust 2018 で future の型が変わるので futures 0.1 ベースの API に死が宣告されている
* actix 0.5 : 2017-2018 年に tokio-core をベースに開発されていたマルチスレッドアクターシステム
    * actix 0.6 の登場で死が宣告されている
* actix 0.6 : tokio-core をやめて tokio runtime をベースにしたマルチスレッドアクターシステム
    * Rust 2018 で future の型が変わるので futures 0.1 ベースの API に死が宣告されている

砂上の楼閣……


## バージョン情報

* futures 0.1.21
* mdo 0.3.0
* mdo-future 0.2.0
* tokio-core 0.1.17
* tokio 0.1.7
* tokio-timer 0.2.4
* actix 0.5.8
* actix 0.6.1
* rustc 1.27.0 (3eda71b00 2018-06-19)
