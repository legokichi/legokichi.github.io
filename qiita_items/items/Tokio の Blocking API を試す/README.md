

# Tokio の Blocking API を試す

tokio の master branch に blocking api が入りました。
これは IO ブロッキング が発生する場所を `tokio_threadpool::blocking` で囲むことで、現在のイベントループを処理するワーカスレッドを IO ブロッキングするためのスレッドプールへ退避させ、自身のイベントループの処理を新しいワーカスレッドと交代します。
これにより `tokio::runtime` のもつ CPU 個のイベントループ処理用のスレッドプールを IO ブロッキングさせることなく処理を継続することができます。

blocking 関数の型は以下の通りです。

```rust
fn blocking<F, T>(f: F) -> Poll<T, BlockingError>
where
  F: FnOnce() -> T
```

blocking 関数は呼ばれると IO 待ちスレッドプールの上限を確認し、待てそうならこのスレッドをIO を待つためのスレッドプールへ移動し、引数の　`FnOnce` を実行してIO ブロッキングを発生させます。
もし IO 待ちスレッドプールがいっぱいであれば、　`BlockingError` を返して 「IO ブロッキング」を待ちます。

この blocking api を使うことで 非同期 IO に対応していないライブラリをかんたんに非同期処理にできることが期待できます。
例えば現在の diesel はクエリの呼び出しが同期処理になっていますが、この同期処理が発生したスレッドを IO 待ちスレッドプールに入れることでイベントループを止めることなく非同期処理を継続できます。

## テストコード

`blocking` の挙動を理解するためにコードを動かしてみます。

```rust
#[macro_use]
extern crate mdo;
extern crate mdo_future;
extern crate futures;
extern crate tokio;
extern crate tokio_timer;
extern crate tokio_reactor;
extern crate tokio_threadpool;
use mdo_future::future::{bind};
use futures::prelude::*;
use tokio::prelude::*;
use tokio_threadpool::{blocking};


fn main() {
    let args: Vec<String> = std::env::args().collect();
    let n = args[1].parse::<u64>().unwrap_or(4) as usize;
    let m = args[2].parse::<u64>().unwrap_or(200) as usize;

    println!("pool_size: {}, max_blocking:{}", n, m);
    let mut builder = tokio::executor::thread_pool::Builder::new();

    // pool_size は 非同期タスクを処理するスレッドの数。デフォルトは CPU 個。
    // max_blocking は IO 待ちをする スレッドの数。 デフォルトは 100。
    builder.pool_size(n).max_blocking(m);

    let mut core = tokio::runtime::Builder::new().threadpool_builder(builder).build().unwrap();
    let now = tokio::clock::now();

    // 同期 sleep と非同期 sleep を 3 つ同時に行う
    for i in 0..3 {
        let fut = Box::new(future::ok(()));

        // 1 秒の同期 sleep を 3 回行う
        let fut = (0..3).fold(fut as Box<Future<Item=(), Error=()> + Send>, move |o, j|{
            let fut = mdo!{
                () =<< o;
                // tokio が Send を要求するため Arc を、
                // poll_fn が FnMut を要求するため futures::sync::oneshot::channel ではなく
                // Clone がある futures::sync::mpsc::channel を使っている
                let (sender, receiver) = futures::sync::mpsc::channel::<()>(10);
                let sender = std::sync::Arc::new(std::sync::Mutex::new(sender));
                () =<< futures::lazy(move ||{
                    future::poll_fn(move || {
                        let sender = sender.clone();
                        // いまからこのスレッドはイベントループを処理するのをやめて IO 待ちを始める
                        blocking(move ||{
                            println!("blocking {}:{}, thread:{:?}, {}s", i, j, std::thread::current().id(), now.elapsed().as_secs());
                            std::thread::sleep(std::time::Duration::from_secs(1));
                            {
                                let mut sender = (*sender).lock().unwrap();
                                let _ = sender.try_send(());
                            }
                        }).map_err(|_|())
                    })
                });
                (_,_) =<< receiver.into_future().map_err(|_|());
                ret future::ok(())
            };
            Box::new(fut)
        });
        let fut = fut.map(|_|());
        core.spawn(fut);

        // 1 秒の非同期 sleep を 3 回行う
        let fut = Box::new(future::ok(()));
        let fut = (0..3).fold(fut as Box<Future<Item=(), Error=()> + Send>, move |o, j|{
            Box::new(o.and_then(move |_|{
                println!("non-blocking {}:{}, thread:{:?}, {}s", i, j, std::thread::current().id(), now.elapsed().as_secs());
                // JavaScript なら new Promise((resolve)=> setTimeout(resolve, 1000)) に相当
                tokio_timer::sleep(std::time::Duration::from_secs(1)).map_err(|_|())
            }))
        });
        core.spawn(fut);
    }
    core.shutdown_on_idle().wait().unwrap();
    println!("finish");
}

```

`std::thread::sleep` による IO Blocking が同時に3つ発生する状況を作りました。
これで `max_blocking` が 3 以下だと `blocking` は `BlockingError` を返して IO ブロッキング待ちが発生することが予想できます。

イベントループスレッドの動作の確認のために `tokio_timer::sleep` による非同期タイマーも並列に実行します。

max_blocking の数を変えて試してみましょう。

## pool_size = 1, max_blocking = 4

非同期は `tokio_timer::sleep` だけなので pool_size は 1 にしてあります（Y2K 問題のような状況では pool_size を CPU 個にすることでスループットを上げることができます）。

まずは max_blocking = 4 です。 

```console
$ cargo run -- 1 4
    Finished dev [unoptimized + debuginfo] target(s) in 0.0 secs
     Running `target/debug/tokio-sandbox 1 4`
pool_size: 1, max_blocking:4
blocking 0:0, thread:ThreadId(2), 0s
non-blocking 0:0, thread:ThreadId(3), 0s
blocking 1:0, thread:ThreadId(3), 0s
non-blocking 1:0, thread:ThreadId(4), 0s
blocking 2:0, thread:ThreadId(4), 0s
non-blocking 2:0, thread:ThreadId(5), 0s
blocking 0:1, thread:ThreadId(2), 1s
blocking 1:1, thread:ThreadId(3), 1s
blocking 2:1, thread:ThreadId(4), 1s
non-blocking 0:1, thread:ThreadId(5), 1s
non-blocking 1:1, thread:ThreadId(5), 1s
non-blocking 2:1, thread:ThreadId(5), 1s
blocking 0:2, thread:ThreadId(2), 2s
blocking 1:2, thread:ThreadId(3), 2s
blocking 2:2, thread:ThreadId(4), 2s
non-blocking 0:2, thread:ThreadId(5), 2s
non-blocking 2:2, thread:ThreadId(5), 2s
non-blocking 1:2, thread:ThreadId(5), 2s
finish
```

初期状態では
`thread:ThreadId(1)` = メインスレッド
`thread:ThreadId(2)` = イベントループスレッド

だったのが

`blocking 0:0, thread:ThreadId(2)` の発生で次の　`non-blocking 0:0, thread:ThreadId(3)`　からイベントループスレッドが `ThreadId(3)` になっているのがわかります。
その後、ブロッキングの発生ごとに順次繰り上がって、最終的にはイベントループスレッドは `thread:ThreadId(5)` になりました。


## pool_size = 1, max_blocking = 3

max_blocking = 3 です。

```console
$ cargo run -- 1 3
    Finished dev [unoptimized + debuginfo] target(s) in 0.0 secs
     Running `target/debug/tokio-sandbox 1 3`
pool_size: 1, max_blocking:3
blocking 0:0, thread:ThreadId(2), 0s
non-blocking 0:0, thread:ThreadId(3), 0s
blocking 1:0, thread:ThreadId(3), 0s
non-blocking 1:0, thread:ThreadId(4), 0s
blocking 2:0, thread:ThreadId(4), 0s
non-blocking 2:0, thread:ThreadId(5), 0s
blocking 0:1, thread:ThreadId(2), 1s
blocking 1:1, thread:ThreadId(3), 1s
blocking 2:1, thread:ThreadId(4), 1s
non-blocking 0:1, thread:ThreadId(5), 1s
non-blocking 1:1, thread:ThreadId(5), 1s
non-blocking 2:1, thread:ThreadId(5), 1s
blocking 0:2, thread:ThreadId(2), 2s
blocking 1:2, thread:ThreadId(3), 2s
blocking 2:2, thread:ThreadId(4), 2s
non-blocking 0:2, thread:ThreadId(5), 2s
non-blocking 2:2, thread:ThreadId(5), 2s
non-blocking 1:2, thread:ThreadId(5), 2s
finish

```
今度も1回目の処理でスレッド ID が順次繰り上がっている様子が確認できます


## pool_size = 1, max_blocking = 2

max_blocking = 2 です。
ここから IO 待ち処理が追いつかなくなってきます。

```console
$ cargo run -- 1 2
    Finished dev [unoptimized + debuginfo] target(s) in 0.0 secs
     Running `target/debug/tokio-sandbox 1 2`
pool_size: 1, max_blocking:2
blocking 0:0, thread:ThreadId(2), 0s
non-blocking 0:0, thread:ThreadId(3), 0s
blocking 1:0, thread:ThreadId(3), 0s
non-blocking 1:0, thread:ThreadId(4), 0s
                                        <--- ここに blocking 2:0, thread:ThreadId(4) がくるはずだが
                                             max_blocking の制限により BlockingError となるので発生が遅れる
non-blocking 2:0, thread:ThreadId(4), 0s
blocking 0:1, thread:ThreadId(2), 1s
blocking 1:1, thread:ThreadId(3), 1s
non-blocking 0:1, thread:ThreadId(4), 1s
non-blocking 1:1, thread:ThreadId(4), 1s
non-blocking 2:1, thread:ThreadId(4), 1s
blocking 0:2, thread:ThreadId(2), 2s
blocking 1:2, thread:ThreadId(3), 2s
non-blocking 0:2, thread:ThreadId(4), 2s
non-blocking 2:2, thread:ThreadId(4), 2s
non-blocking 1:2, thread:ThreadId(4), 2s
blocking 2:0, thread:ThreadId(4), 3s
blocking 2:1, thread:ThreadId(4), 4s     <--- !!!???
blocking 2:2, thread:ThreadId(4), 5s
finish

```

`blocking 2:*` が溢れているのが確認できます。
`blocking 2:1, thread:ThreadId(4)` の時点では他に空いているスレッドがあるのに使っていません。これはバグです。ソースを見たら現時点では未実装でした - https://github.com/tokio-rs/tokio/blob/8d8c895a1c97198e9461c4e01098f9c73ce626fe/tokio-threadpool/src/worker/mod.rs#L199-L201
まだ開発中ブランチなので仕方がないですね。

## pool_size = 1, max_blocking = 1

max_blocking = 1 です。
IO 待ちスレッドは一つしか許可されないため、 `blocking 0:*` の処理だけで 3秒かかっています。

```console
$ cargo run -- 1 1
    Finished dev [unoptimized + debuginfo] target(s) in 0.0 secs
     Running `target/debug/tokio-sandbox 1 1`
pool_size: 1, max_blocking:1
blocking 0:0, thread:ThreadId(2), 0s
non-blocking 0:0, thread:ThreadId(3), 0s
                                          <--- ここで BlockingError
non-blocking 1:0, thread:ThreadId(3), 0s
non-blocking 2:0, thread:ThreadId(3), 0s
blocking 0:1, thread:ThreadId(2), 1s
non-blocking 0:1, thread:ThreadId(3), 1s
non-blocking 2:1, thread:ThreadId(3), 1s
non-blocking 1:1, thread:ThreadId(3), 1s
blocking 0:2, thread:ThreadId(2), 2s
non-blocking 0:2, thread:ThreadId(3), 2s
non-blocking 1:2, thread:ThreadId(3), 2s
non-blocking 2:2, thread:ThreadId(3), 2s
blocking 1:0, thread:ThreadId(3), 3s
blocking 1:1, thread:ThreadId(3), 4s
blocking 1:2, thread:ThreadId(3), 5s
blocking 2:0, thread:ThreadId(2), 6s
blocking 2:1, thread:ThreadId(2), 7s
blocking 2:2, thread:ThreadId(2), 8s
finish
```

## 感想

* `future::poll_fn` が `FnMut` 要求してて `futures::sync::oneshot::channel` がつかえなくてうざい
* `Arc<Mutex<futures::sync::mpsc::Sender>>` なるおぞましい型を使っってしまったがもっとうまい方法もある？
* 非同期 IO の未来が広がって嬉しい


## 参考
* goroutine の work stealing について - https://qiita.com/takc923/items/de68671ea889d8df6904#_reference-05eea9307a3e6e7252a9
* tokio runtime - https://tokio.rs/blog/2018-03-tokio-runtime/
* tokio blocking - https://tokio.rs/blog/2018-05-tokio-fs/#blocking
* blocking api PR - https://github.com/tokio-rs/tokio/pull/317
* blocking api doc - https://tokio-rs.github.io/tokio/tokio_threadpool/fn.blocking.html
* diesel Async I/O - https://github.com/diesel-rs/diesel/issues/399

## 付録

### Cargo.toml

```toml
[package]
name = "tokio-sandbox"
version = "0.1.0"

[dependencies]
getopts = "0.2"
mdo = "0.3"
mdo-future = "0.2"
futures = "0.1"
tokio = { git = "https://github.com/tokio-rs/tokio.git", commit = "8d8c895a1c97198e9461c4e01098f9c73ce626fe" }
tokio-timer = { git = "https://github.com/tokio-rs/tokio.git", commit = "8d8c895a1c97198e9461c4e01098f9c73ce626fe" }
tokio-reactor = { git = "https://github.com/tokio-rs/tokio.git", commit = "8d8c895a1c97198e9461c4e01098f9c73ce626fe" }
tokio-threadpool = { git = "https://github.com/tokio-rs/tokio.git", commit = "8d8c895a1c97198e9461c4e01098f9c73ce626fe" }
```

tokio は version = "0.1.7" の master branch

### rustc version

rustc 1.26.1 (827013a31 2018-05-25)
