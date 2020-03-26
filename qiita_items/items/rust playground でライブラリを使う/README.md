https://play.rust-lang.org

では下記のいずれかの条件を満たした crate をインポートして使うことができます。

* crates.io で最もよくダウンロードされている上位100位 - https://crates.io/crates?sort=downloads
* rust-cookbook で使われいるクレート - https://rust-lang-nursery.github.io/rust-cookbook/
* それらの依存関係のクレート


futures + tokio_core を使った例です - https://play.rust-lang.org/?gist=9d639631527653dee35c1f4174497c62&version=stable&mode=debug&edition=2015

```rust
extern crate futures;
extern crate tokio_core;

use std::time::Duration;
use futures::prelude::*;
use tokio_core::reactor::Core;
use tokio_core::reactor::Timeout;

fn main() {
    println!("tokio_core");
    let mut core = Core::new().unwrap();
    let handle = core.handle();
    core.run(
        Timeout::new(Duration::from_secs(1), &handle).unwrap()
            .and_then(move |_|{
                println!("1");
                Timeout::new(Duration::from_secs(1), &handle).unwrap()
                    .and_then(move |_|{
                        println!("2");
                        Timeout::new(Duration::from_secs(1), &handle).unwrap()
                            .and_then(move |_|{
                                println!("3");
                                futures::future::ok(())
                            })
                    })
            })
            .map_err(|err| println!("{:?}", err) )
    ).unwrap();
    println!("end");
}
```

残念ながら [rust の非同期 IO ライブラリ tokio-core と tokio runtime と actix 0.5 と actix 0.6 でタイマーの比較](https://qiita.com/DUxCA/items/8ab8693e3ae3078fab24) で多様した mdo は含まれていないので↑のように手動でマクロ展開する必要があります。

 ~~すごいライブラリ作って上位100位に入って mdo を依存関係に入れれば使えるようになる~~ 

ace エディタがスマホから入力するのが辛い問題はありますが、ちょっとした挙動の検証に何かと便利なのではないでしょうか。

## 参考

* その他の情報はこの help ページで見れます - https://play.rust-lang.org/help
