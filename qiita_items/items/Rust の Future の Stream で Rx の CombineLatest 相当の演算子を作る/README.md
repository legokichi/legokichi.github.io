# Rust の Future の Stream で Rx の CombineLatest 相当の演算子を作る
2 つのイベントストリームを合成する、 [こういう](http://reactivex.io/documentation/operators/combinelatest.html)[演算子](https://github.com/staltz/xstream#combine)が欲しい

```
--1----2-----3--------4---
----a-----b-----c--d------
         combine
----1a-2a-2b-3b-3c-3d-4d--
```

## 動機


このようなステートマシンを持つアクターが 2 個あったとする。

```
        +------------------------------+
        v                              |
Init -> Idle -(Run)-> Running -> Done -+
        |
      (Kill)
        |
        v
        Finish
```

```rust
enum State {
  Init,
  Idle,
  Running,
  Done,
  Finish
}
enum Command {
  Run,
  Kill
}
```

各アクターへはコマンド列を `futures::stream::Stream<Item = State>` のように送り、
各アクターは状態遷移したとき `futures::stream::Stream<Item = State>` で現在の実行状態を送るとする。

このとき、 2 つのアクターの実行状態の直積をとったストリーム `impl Stream<Item=(State, State)>` を得たい。
これを実現するのが Rx の CombineLatest 演算子であるが、 Rust の futures 0.3 の StreamExt にはそのようなメソッドは存在しないので作りたい。

## アクターの実装

`State::Init` は省略し、 `Command::Kill` は Stream の close で代用する。

```rust
use std::time::Duration;
use futures::prelude::*; // 0.3.4
use futures::channel::mpsc::{UnboundedSender, UnboundedReceiver, unbounded};
use tokio::time; // 0.2.11
use tokio::task::JoinHandle;

#[derive(Debug)]
pub struct Command;

#[derive(Debug)]
pub enum State{
    Idle,
    Running,
    Done,
    Finish
}

pub fn start() -> (
    UnboundedSender<Command>,
    UnboundedReceiver<State>,
    JoinHandle<()>,
) {
    let (com_tx, mut com_rx) = unbounded::<Command>();
    let (mut st_tx, st_rx) = unbounded::<State>();
    let fut = async move{
        loop{
            st_tx.start_send(State::Idle).unwrap();
            if let Some(_com) = com_rx.next().await {
                st_tx.start_send(State::Running).unwrap();
                time::delay_for(Duration::from_millis(10)).await;
                st_tx.start_send(State::Done).unwrap();
            }else{
                break;
            }
        }
        st_tx.start_send(State::Finish).unwrap();
        st_tx.close().await.unwrap();
        ()
    };
    let handle = tokio::spawn(fut);
    (com_tx, st_rx, handle)
}
```

## アクターへのコマンド送信の実装

2 つのアクターそれぞれに時間差でコマンドを送り、 Sink close することでアクターを kill する。

```rust
let (mut com_tx, st_rx, tsk) = start();
let (mut com_tx2, st_rx2, tsk2) = start();

tokio::spawn(async move{
    time::delay_for(Duration::from_millis(100)).await;
    com_tx.start_send(Command).unwrap();
    time::delay_for(Duration::from_millis(100)).await;
    com_tx2.start_send(Command).unwrap();
    time::delay_for(Duration::from_millis(100)).await;
    com_tx.close().await.unwrap();
    time::delay_for(Duration::from_millis(100)).await;
    com_tx2.close().await.unwrap();
});
```

## CombineLatest の実装
ようやく本題に入る。
`StreamExt::fold` と `Either` と `Option` を使うことで CombineLatest を再現する。

```rust
use either::Either; // 1.5.3

tokio::spawn(async move{
    stream::select(
        st_rx.map(Either::Left),
        st_rx2.map(Either::Right),
    ).fold((None, None), |(l, r), either|async move{
        let ret = match either {
            Either::Left(l) => (Some(l), r),
            Either::Right(r) => (l, Some(r)),
        };
        println!("{:?}", ret);
        ret
    }).await;
});
```

これを実行すると

```rust
(Some(Idle), None)
(Some(Idle), Some(Idle))
(Some(Running), Some(Idle))
(Some(Done), Some(Idle))
(Some(Idle), Some(Idle))
(Some(Idle), Some(Running))
(Some(Idle), Some(Done))
(Some(Idle), Some(Idle))
(Some(Finish), Some(Idle))
(Some(Finish), Some(Finish))
```

のようになり、それぞれのアクターの状態の直積が得られた。
Option でラップされているのが気になると思うが、 `enum State` の `Default` を `State::Init` ないし `State::Idle` として実装すれば `Option` もなくせると思う。


playground のコード - https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=38827437b5227cfbf753062f727f2d0b

## 参考

* https://stackoverflow.com/questions/42934501/how-to-combine-latest-values-of-two-streams-using-futures-rs
* https://github.com/staltz/xstream#combine
* http://reactivex.io/documentation/operators/combinelatest.html

