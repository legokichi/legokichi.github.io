## いちばんシンプルな非同期 Rust のランタイムを自作する

基本的な概念の確認のために自作してみました。
簡便化のために select, epoll は使わず、メインスレッドでイベントループ動かし、wakerはmpsc経由で別スレッドから投げます。

## タスクを定義する

タスクとは一連の Future のチェーンです。
`Future` を `spawn` するときに発生します。
今回はタスクを `Future<Output = ()>` のような値を返さないものとして定義しました。

```rs
use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll, Waker};

/// タスクはアウトプットのない BoxFuture
struct Task {
    future: Pin<Box<dyn Future<Output = ()> + 'static>>,
}
impl Task {
    fn new(f: impl Future<Output = ()> + 'static) -> Self {
        Self {
            future: Box::pin(f),
        }
    }
    /// このタスクを poll して Ready を返すと、このタスクは待機キューから削除される
    fn poll(&mut self, waker: Waker) -> Poll<()> {
        let mut ctx = Context::from_waker(&waker);
        match Future::poll(self.future.as_mut(), &mut ctx) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(()) => Poll::Ready(()),
        }
    }
}
```

## Waker と Runtime (イベントループ) を作る

イベントループを作ります。
イベントループの実装に必要なのは 実行可能タスクのキュー と 待機タスクの置き場 、そして 待機タスクの再起動通知を送る Waker です。

```rs
use std::cell::{Cell, RefCell};
use std::collections::BTreeMap;
use std::future::Future;
use std::rc::Rc;
use std::sync::{Arc, Mutex};
use std::task::{Context, Poll, Waker};
use std::sync::mpsc::{channel, Sender, Receiver};

// ユニークなタスク ID
pub type TaskId = usize;

#[derive(Clone)]
pub struct Runtime {
    tx: Sender<TaskId>,
    /// ユニークなタスク ID を発行するために使う
    task_id_counter: Rc<Cell<TaskId>>,
    /// 前回 poll したら Pending だったタスクを置いとく場所
    /// タスクがここに入ることを park, ここから出ることを unpark という
    wait_tasks: Rc<RefCell<BTreeMap<TaskId, Task>>>,
    /// これから poll するとタスク内の Future が Ready を返し、タスクが１ステップ進むであろうタスクの ID キュー
    /// タスクに対応する Waker が呼ばれると実行可能なタスクがこのキューに突っ込まれる
    /// std::task::Waker が Send + Sync の都合で Arc<Mutex<T>> に包んでいる
    run_queue: Arc<Mutex<Receiver<TaskId>>>,
}
impl Runtime {
    pub fn new() -> Self {
        let (tx, rx) = channel();
        Self {
            tx,
            task_id_counter: Rc::new(Cell::new(0)),
            wait_tasks: Rc::new(RefCell::new(BTreeMap::new())),
            run_queue: Arc::new(Mutex::new(rx)),
        }
    }
    /// タスクを待機タスク置き場に突っ込む
    pub fn spawn(&self, f: impl Future<Output = ()> + 'static) {
        let task_id = self.task_id_counter.get();
        self.task_id_counter.set(task_id + 1);
        let waker = MpscWaker::waker(task_id, self.tx.clone());
        let mut task = Task::new(f);
        // タスクを poll して即時 Ready なら待機タスク置き場には入れない
        match task.poll(waker) {
            Poll::Ready(()) => {}
            Poll::Pending => {
                // このタスクは待機
                self.wait_tasks.borrow_mut().insert(task_id, task);
            }
        }
    }
    /// イベントループを起動して Pending なタスクがなくなるで待つ
    pub fn run(&self, f: impl Future<Output = ()> + 'static) {
        self.spawn(f);
        loop {
            // いわゆる Reactor 
            // 本来はここで epoll して実行準備の整ったタスクに対応する waker を呼ぶ (= mpsc に task id を突っ込む)が、今回は実装しない
            ();

            // いわゆる Executer - 実行可能タスクを実行する
            // 実行可能タスクIDキューから待機タスクIDを取り出す
            let task_id = { self.run_queue.lock().unwrap() }.recv().unwrap();
            let mut task = {
              // borrow_mut のスコープを制限
              self.wait_tasks.borrow_mut().remove(&task_id).unwrap()
            };
            // タスクに waker = context を渡して poll する
            let waker = MpscWaker::waker(task_id, self.tx.clone());
            match task.poll(waker) {
                // タスク内の Future チェーンがすこし進んだが、タスク全体はまだ終了してない、なので待機タスクに追加
                Poll::Pending => {
                    self.wait_tasks.borrow_mut().insert(task_id, task);
                }
                // タスクが完了したので Drop
                Poll::Ready(()) => {}
            }

            // 待機タスクが空になったらイベントループ終了
            if self.wait_tasks.borrow_mut().is_empty() {
                break;
            }
        }
    }
}
```

このイベントループは epoll のかわりに Mpsc の受信でスレッドを止めることでビジーループに入るのを防いでいます。

Waker は待機しているタスクを起こす (wake) 性質をもつ構造体です。
この Waker の wake を呼ぶと reactor が反応(react) してイベントループがまわります。
`std::task::Waker` はなぜかトレイトではなく vtable をもつ構造体として定義されています。
~~ランタイム作者の最適化の余地を残している？ようです~~
どうやら future は core ライブラリに生え手いて Box、Rc や Arc などのalloc の使えない組み込み向けにも使えるように低レベルの API が露出しているようです

```rs
use futures::task::ArcWake;
use std::sync::{Arc, Mutex};
use std::task::Waker;
use std::sync::mpsc::{channel, Sender, Receiver};

#[derive(Clone)]
struct MpscWaker(TaskId, Arc<Mutex<Sender<TaskId>>>);
impl MpscWaker {
    fn waker(task_id: TaskId, tx: Sender<TaskId>) -> Waker {
        futures::task::waker(Arc::new(MpscWaker(task_id, Arc::new(Mutex::new(tx)))))
    }
}
impl ArcWake for MpscWaker {
    fn wake_by_ref(arc_self: &Arc<Self>) {
        let MpscWaker(task_id, ref tx) = **arc_self;
        tx.lock().unwrap().send(task_id).unwrap();
    }
}
```

始めは futures ライブラリの `futures::task::ArcWake` を使わずに std の `std::task::{RawWaker, RawWakerVTable, Waker}` だけで Waker を作ってみようと思ったのですが、 `std::task::Waker` は `Send + Sync` を要求しており、 Rc だけは無理そうだったので、この性質を満たすために素直に `futures::task::ArcWake` を使うことにしました。
詳細については [Keenさんの記事](https://keens.github.io/blog/2019/07/07/rustnofuturetosonorunnerwotsukuttemita/) をどうぞ


## タイマーを作る

今回はごくシンプルにタイマーごとにスレッドを立てて sleep する実装にしました。
スレッドが目覚めたら waker を呼びます。

```rs
use std::future::Future;
use std::pin::Pin;
use std::sync::{Arc, Mutex};
use std::task::{Context, Poll, Waker};
use std::thread::JoinHandle;
use std::time::Duration;

pub struct Timeout {
    // タイマースレッドのハンドル
    th: Option<JoinHandle<()>>,
    // タイマーの状態
    state: Arc<Mutex<Poll<()>>>,
    // タイマーが発動したときに呼ぶ waker 
    waker: Arc<Mutex<Option<Waker>>>,
}
impl Timeout {
    pub fn set(duration: Duration) -> Self {
        let waker = Arc::new(Mutex::new(None::<Waker>));
        let state = Arc::new(Mutex::new(Poll::Pending));
        let w = waker.clone();
        let s = state.clone();
        let th = std::thread::spawn(move || {
            // スリープ
            std::thread::sleep(duration);
            // state を Ready にする
            let mut state = s.lock().unwrap();
            *state = Poll::Ready(());
            // waker を呼んでこの Future が所属するタスクを実行可能タスクIDキューに積む
            if let Some(waker) = &*w.lock().unwrap() {
                waker.wake_by_ref()
            }
        });
        Self {
            th: Some(th),
            state,
            waker,
        }
    }
}
impl Future for Timeout {
    type Output = ();
    /// 初回 poll 時の waker を借りパクして流用します
    fn poll(mut self: Pin<&mut Self>, ctx: &mut Context) -> Poll<Self::Output> {
        *self.waker.lock().unwrap() = Some(ctx.waker().clone());
        match *self.state.lock().unwrap() {
            Poll::Pending => Poll::Pending,
            Poll::Ready(()) => Poll::Ready(()),
        }
    }
}
impl Drop for Timeout {
    fn drop(&mut self) {
        self.th.take().unwrap().join().unwrap();
    }
}
```

スレッドを使わないタイマーを実装するには reactor の部分で 現在待っているタイマーの一覧から最小の待ち時間のものを調べてその時間だけ sleep する必要があります。
async-std の [futures-timer](https://github.com/async-rs/futures-timer) や初期の tokio-core のタイマーは最小ヒープ木によって次のsleep時間 (実際にはsleep ではなく復帰可能な [std::thread::park_timeout](https://doc.rust-lang.org/std/thread/fn.park_timeout.html)を使っているよう) を算出しているようです。
また [tokio-timer](https://github.com/tokio-rs/tokio-timer#hashed-timing-wheel) は Hashed Timing Wheels というより[高精度で効率的なタイマーアルゴリズムを使っているよう](https://tokio.rs/blog/2018-03-timers/)です。
他にも epoll で使える [timerfd](https://github.com/polachok/tokio-timerfd) を使った実装などもあるようです。

async-std の futures-timer は内部にシングルトンのタイマースレッドをもっており、ランタイムに依存しないタイマー(tokioでも動く)です。
一方で timerfd や 

## タイマーを使ってみる

async や spawn や join を駆使した複雑なフローを試してみます。
果たしてうまく動くでしょうか？

```rs
fn main() {
    use futures::future::join;

    let runtime = Runtime::new();
    let r = runtime.clone();

    runtime.run(async move {
        let start_at = Instant::now();
        r.spawn(async move {
            Timeout::set(Duration::from_millis(100)).await;
            println!("100ms: {}ms", start_at.elasped().as_millis());
        });
        join(
            async {
                Timeout::set(Duration::from_millis(1000)).await;
                println!("1000ms: {}ms", start_at.elasped().as_millis());
                Timeout::set(Duration::from_millis(500)).await;
                println!("1500ms: {}ms", start_at.elasped().as_millis());
            },
            async {
                Timeout::set(Duration::from_millis(2000)).await;
                println!("2000ms: {}ms", start_at.elasped().as_millis());
            },
        )
        .await;
        println!("joined: {}ms", start_at.elasped().as_millis());
    });
}
```

```
100ms: 102ms
1000ms: 1002ms
1500ms: 1502ms
2000ms: 2002ms
joined: 2002ms
```

うまく動きました！

* コード全体 - https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=3bfa55aef722898021afe7183ba8cf23

## 感想

* 自作ランタイムが動いて嬉しい！
* Context と Waker が妙に冗長なのは何故
* ArcWake はあるけど RcWake がないのは何故（あるいは Waker に Send + Sync がついているのは何故）
* そのうち wheel timer も作りたい

## 参考

* https://rust-lang.github.io/async-book/02_execution/01_chapter.html
* https://github.com/polachok/fahrenheit
* https://keens.github.io/blog/2019/07/07/rustnofuturetosonorunnerwotsukuttemita/ 
* https://raskr.hatenablog.com/entry/2018/07/16/214420

## おまけ Heap Timer のアルゴリズム

futures-timer で使われている min-heap timer のアルゴリズムの最小構成です

```rs
use std::time::{Duration, Instant};
use std::cmp::Ordering;
use std::collections::BinaryHeap;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
struct Timeout(Instant);
impl Timeout {
    fn timeout(duration: Duration) -> Self {
        Self(Instant::now() + duration)
    }
    fn wait_for(&self) -> Duration {
        self.0 - Instant::now()
    }
}

impl Ord for Timeout {
    fn cmp(&self, other: &Timeout) -> Ordering {
        // Rust の BinaryHeap は max-heap だが
        // 使いたいのは min-heap なので
        // 順序を逆にしてる
        other.0.cmp(&self.0)
    }
}
impl PartialOrd for Timeout {
    fn partial_cmp(&self, other: &Timeout) -> Option<Ordering> {
        Some(Ord::cmp(self, other))
    }
}

fn main(){
    let now = Instant::now();

    let mut heap = BinaryHeap::new();
    heap.push(Timeout::timeout(Duration::from_millis(10)));
    heap.push(Timeout::timeout(Duration::from_millis(100)));
    heap.push(Timeout::timeout(Duration::from_millis(1000)));
    
    let th = std::thread::spawn(move ||{
        loop{
            if let Some(wait) = heap.pop().map(|o| o.wait_for()) {
                println!("{}ms + {}ms", now.elapsed().as_millis(), wait.as_millis());
                std::thread::park_timeout(wait);
                println!("{}ms", now.elapsed().as_millis());
            }else{
                break;
            }
        }
    });
    th.join().unwrap();
}
```

出力

```
0ms + 9ms
10ms
10ms + 89ms
100ms
100ms + 899ms
1000ms
```

* https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=ac9e7a53017c08d0ef7592e94db66186
