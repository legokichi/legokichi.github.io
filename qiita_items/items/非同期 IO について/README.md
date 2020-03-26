# 非同期 IO について

------

## おしながき

1. C10K 問題 について
2. 非同期 IO とは何か
3. async-await
4. 非同期 IO のこれから

------

## 1. C10K 問題 について

------

### 時は 2002 年

------

### Web 勃興期

------

### 牧歌的時代の終わり

------

### HTTP サーバ

------

### 10000 クライアントからの同時接続

------

### どう実装するか

------

### 当時の Web サーバ

* __Apache__
* クライアント毎に 1 プロセス
* クライアント毎に 1 スレッド

-----

### クライアント毎に 1 プロセス

* 10000 プロセス も起動したら OS が死ぬ
* CPU|メモリ が足りない
* PID の上限
* etc...

-----

### クライアント毎に 1 スレッド

* 10000 thread も起動したら OS が死ぬ
* CPU|memoryが足りない
* thread数の上限
* virtual memory の上限
* etc...

------

### これからの時代(2002年当時)

* __nginx__
* 複数の thread で複数の client を受け持つ
* __非同期__ __並列__ イベント駆動
  * 非同期: 非同期イベント駆動
  * 並列: 軽量スレッド

------

### ![](https://upload.wikimedia.org/wikipedia/commons/thumb/a/af/Gen_bijection.svg/200px-Gen_bijection.svg.png) → ![](https://upload.wikimedia.org/wikipedia/commons/thumb/6/6c/Surjection.svg/200px-Surjection.svg.png)

------

### C10K 前後史

------

* 1973: Actor(Scala, Erlang の起源)
* 1977: future-promise
* 1978: CSP(Go の起源)
* 1983: C++
* 1986: Erlang
* 1990: Haskell 1.0
* 1991: Concurrent ML

------

* 1994: PHP, Java, python1, Perl5, Common Lisp
* 1995: Apache, JavaScript, Ruby, Haskell 1.3(monad)
* 1996: Shockwave Flash, Concurrent Haskell
* 1997: IE4

-----

* 2000: libevent(poll|select)
* 2001: POSIX AIO(POSIX 1003.1b), IE6
* 2002: __C10K 問題提議__
* 2003: libaio(Linux AIO, Linux2.6), Scala
* 2004: __nginx(AIO)__, Rails, Firefox1

------

* 2005: gevent(eventlet), GoogleMap, F#
* 2006: Firefox 2.0, jQuery1.0
* 2007: libev, Clojure(STM), LINQ(C#3.0)
* __2008__: __epoll (Linux 2.6.28)__, boost(1.35) asio , Task(C#4.0), node.js(libuv), Google Chrome
* 2009: go, CoffeeScript, iPhone3GS, ReactiveX(C#5.0)

------

* 2010: C#5.0(async)
* 2011: java nio(jdk1.7), Kotlin, React, Deferred(jQuery1.5)
* 2012: Elixir, TypeScript
* 2013: Promise
* 2014: Swift, Flux, asyncio(Python3.4)

------

### C10K まとめ

* 10000 コネクションさばきたい
* 非同期や並列アーキテクチャが模索された時代があった

------

## 2. 非同期 IO とは何か

------

### IO とは

* Disk IO
* Network IO
* IO は CPU の時間に比べてとても遅い

------

### IO の種類

* 同期ブロッキング
* 同期ノンブロッキング
* 非同期ブロッキング
* 非同期ノンブロッキング

------

### 同期ブロッキング

* プロセスがカーネルにシステムコールする
* プロセスはカーネルの返事を __待つ__
* プロセスのスレッドのプログラムカウンタは止まる
* 例: read, write システムコール

------

### 同期ノンブロッキング

* プロセスがカーネルにシステムコールする
* プロセスはカーネルの返事を __待たない__
* プロセスは任意のタイミングで IO の状態をカーネルに問い合わせる(polling)
* 例: O_NONBLOCK を指定した read, write, poll, select システムコール

------

### 非同期ブロッキング

* プロセスはカーネルにシステムコールでIO処理をたくさん登録して返事を __待つ__
* カーネルは __どれかが終わったら__ 返事する
* 同期ブロッキングに比べてたくさんの IO 処理を同時に待てる (多重 IO)
* ex. epoll システムコール

------

### 非同期ノンブロッキング

* プロセスはカーネルにシステムコールをたくさん投げる
* プロセスはカーネルからのシグナルが発生するまで他のことをする
* シグナル処理は高コストという問題も
* ex. POSIX AIO, Linux AIO

------

### 非同期 IO の実現方法

* 1スレッドで複数の IO 呼び出しを処理を __しない__
* 1スレッドで複数の IO 呼び出しを処理を __する__

----

### 単一スレッドで複数の IO 呼び出しを処理を __しない__

* 同期ブロッキングを使い、複数スレッドで並列化
* Python や Ruby のスレッドに GIL (Global Interpreter Lock) がついているのは IO 待ちのためのスレッドだから

------

#### GIL のない言語では

* go は GIL がない代わりに channel と mutex がある
* Clojure は GIL の代わりに STM がある

------

### 単一スレッドで複数の IO 呼び出しを処理を __する__

* 同期ノンブロッキング(O_NONBLOCK)して、カーネルに次回のイベントを poll(libevent)
* POSIX AIO で非同期ノンブロッキングして、シグナルでカーネルからの通知を受け取る(nginx aio)
* epoll で複数のIOをまとめて取り扱う(nodejs)

------

### 非同期 ファイル IO について

* epoll はファイル IO を非同期で扱えない
* linux でのファイル IO はスレッドプールで実現されている
* Linux AIO はパフォーマンスの問題や開発が止まっているから使われていない

------

### 非同期 IO まとめ

* 同期ブロッキング - 普通のIO
* 同期ノンブロッキング - 古い非同期(O_NONBLOCK)
* 非同期ブロッキング - モダンな非同期(epoll)
* 非同期ノンブロッキング - 開発停滞(Linux AIO)

------

## 3. イベント駆動と async-await

------

### イベント駆動とは

* イベントループで次に発火するイベントを決める
* コールバックスタイルのイベントリスナにイベントを通知する
* シングルスレッドで多重 IO が扱える
* イベントループのスレッドでの処理時間を抑えないとパフォーマンスが出ない

-----

#### イベントループ擬似コード

```js
const waitingEvents = [];
const callbacks = {
  "main": ()=>{
    waitingEvents.push("foo");
    callbacks["foo"] = (event)=>{
      console.log("foo", event);
    };
  }
};

// event loop
while(true){
  const currentEvent = epollLike(waitingEvents);
  currentEvent.forEach(([eventName, event])=>{
    callbacks[eventName](event);
  });
}
```

------

### コールバックスタイル

------


![plhe5cqmsz7ss4mrwqix.jpg](https://qiita-image-store.s3.amazonaws.com/0/13484/2fd00f51-a975-168f-0d57-36faf579e8e7.jpeg)

------

＿人人人人人人人人人人人＿
＞　突然のCallback Hell　＜
￣Y^Y^Y^Y^Y^Y^Y^Y^Y^Y￣

------

### async-await の必要性

![js-callbacks-promises-asyncawait.gif](https://qiita-image-store.s3.amazonaws.com/0/13484/96f6e60b-2383-e495-70b5-a38e5634fe9e.gif)

------

### callback hell からの脱出

* 継続渡し形式(コールバック)から
* 直接形式(yield|async-await)へ

------

### yield | async-await の実現方法

* 軽量スレッド
* 継続
* ステートマシン
* マクロ

------

### 軽量スレッド

* スレッド生成のコストはでかい
* そもそもスタックフレームはでかい
* IO イベントを待つために省メモリな軽量スレッドを使う
* goroutine, C# Task, Java Green thread, GHC Haskell, Elixir, Boost Fiber

------

```c++
int read_chunk( NonblockingAPI & api, std::string & data, std::size_t desired) {
    int error;
    while ( EWOULDBLOCK == ( error = api.read( data, desired) ) ) {
        // ここで他のファイバーに処理を譲る
        boost::this_fiber::yield();
    }
    return error;
}
```

------

### 継続

* Generator, Coroutine
* __スタックポインタとプログラム・カウンタを実行時に書き換える__ ことでシングルスレッドで擬似マルチスレッドできる
* JavaScript, Python, Rust, Ruby, Boost Context, scheme call/cc

------

```cpp
context ctx;
asio::io_service io_service;

void f(){
  socket_.async_connect(
    ip::tcp::endpoint(ip::address::from_string("127.0.0.1"), 31400),
    [](auto ec){ ctx.resume(); });
  ctx.suspend();
  // next...
}

int main(){
  auto ctx = context{f};
  ctx.start();
  io_service.run();
}
```

------

### 状態マシン

* Coroutine エミュレータ
* Boost Coroutine, C# の async-await, Babel|TS の generator の ES5 向けコンパイル

------

### コンパイル前

```ts
async function a() {
    const a = await new Promise((resolve, reject)=>
      setTimeout(()=>{ resolve(0); }, 1000) );
}
```

------

### コンパイル後

```js
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __generator = (this && this.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g;
    return g = { next: verb(0), "throw": verb(1), "return": verb(2) }, typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (_) try {
            if (f = 1, y && (t = y[op[0] & 2 ? "return" : op[0] ? "throw" : "next"]) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [0, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
};
function a() {
    return __awaiter(this, void 0, void 0, function () {
        var a;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0: return [4 /*yield*/, new Promise(function (resolve, reject) {
                        return setTimeout(function () { resolve(0); }, 1000);
                    })];
                case 1:
                    a = _a.sent();
                    return [2 /*return*/];
            }
        });
    });
}
```

------

### マクロ

* コンパイル前にコールバックネストに変換される
* haskell, purescript, clojure

------

### 閑話休題

------

### Fiber, Coroutine, Generator, Goroutine の違い

------

#### Fiber とか

* User Land Thread
* Light Weight Thread
* Green Thread
* OS thread の代替物。スケジューラ がある。
* スレッドプールで処理されることもある

------

#### Generator とか

* Generator
  * Iterator を生成するもの
* Iterator
  * next() で次の値が取り出せるもの
* Coroutine
  * Generator で実現できるもの。スケジューラは含まれていない

------

#### Goroutine

* go のランタイムにある非同期並列 Fiber の scheduler
* 他の言語の軽量スレッドと違い IO イベントループに依存しないことで IO bound と CPU bound な処理の使い分けを scheduler でがんばる

------

### イベント駆動と async-await まとめ

* イベント駆動はコールバックスタイルになる
* コールバックスタイルはコールバック地獄を生み出す
* コールバック地獄は async-await で解決できる

------

## 4. 非同期 IO のこれから

------

### 非同期 IO のこれから

* 非同期例外とタスクキャンセル
* promise-future, async-await はモナド
* Stream は promise-future の一般化
* Stream はモナド
* 関数型リアクティブプログラミング

------

時間切れ

------

## 参考資料

------

* http://cml.cs.uchicago.edu/pages/cml.html
* https://www.oreilly.co.jp/community/blog/2010/03/pthread-epoll-inet-server-part1.html
* https://www.oreilly.co.jp/community/blog/2010/03/pthread-epoll-inet-server-part2.html
* http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2014/n4024.pdf
* https://stackoverflow.com/questions/42983095/coroutine-vs-fiber-difference-clarification
* https://softwareengineering.stackexchange.com/questions/254140/is-there-a-difference-between-fibers-coroutines-and-green-threads-and-if-that-i
* http://d.hatena.ne.jp/fjnl/touch/20111005/1317802868
* https://lists.boost.org/Archives/boost/2011/03/178613.php
* https://cpplover.blogspot.jp/2012/10/blog-post_28.html
* https://postd.cc/performance-without-the-event-loop/
* https://github.com/crossbeam-rs/crossbeam/issues/64
* https://docs.rs/rayon/0.9.0/rayon/fn.spawn.html
* https://github.com/crossbeam-rs/rfcs/blob/master/text/2017-04-30-roadmap.md
* https://github.com/crossbeam-rs/rfcs/blob/master/text/2017-11-09-channel.md
* https://rust-lang-nursery.github.io/rust-cookbook/basics.html#ex-run-piped-external-commands
* https://docs.rs/tokio-process/0.2.0/tokio_process/
* https://rust-lang-nursery.github.io/rust-cookbook/basics.html#access-a-file-randomly-using-a-memory-map
* https://manishearth.github.io/blog/2018/01/10/whats-tokio-and-async-io-all-about/
* https://qiita.com/YukiMiyatake/items/91d90d53d8e1e135da62
* https://qiita.com/takc923/items/de68671ea889d8df6904
* https://qiita.com/CountryMan/items/fedc6a677721e077af25  
* http://www.hyuki.com/yukiwiki/wiki.cgi?TheC10kProblem
* http://ufcpp.net/study/csharp/sp5_async.html
* https://www.infoq.com/jp/news/2017/05/async-streams
* https://qiita.com/mishima/items/f242808c76fd5b4cf3b1
* https://cycle.js.org
* https://cycle.js.org/dialogue.html
* https://github.com/staltz/xstream
* https://pursuit.purescript.org/packages/purescript-xstream/1.0.0/docs/Control.XStream
* https://pursuit.purescript.org/packages/purescript-signal/9.0.0/docs/Signal
* https://qiita.com/jooex/items/7292a706cdd99987fa28
* http://package.elm-lang.org/packages/Apanatshka/elm-signal-extra/5.7.0/Signal-Stream
* https://giisyu.gitbooks.io/elm_usui_book/content/src/elmArchitecture/cmdSub.html
* https://hackage.haskell.org/package/Stream-0.4.7.2/docs/Data-Stream.html
* https://github.com/slamdata/purescript-halogen
* https://docs.rs/futures/0.1.18/futures/stream/index.html
* https://github.com/ghc/ghc/blob/12ae1fa51b2f59e37d6100359b494bee2192ef0a/rts/Schedule.c#L200
* https://github.com/ghc/ghc/blob/master/rts/posix/Select.c#L150
* https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/IOManager
* https://www.reddit.com/r/haskell/comments/4vz3gs/polling_on_sockets/d638n9q/
* https://www.reddit.com/r/haskell/comments/6b3dlt/techempower_benchmarks_14_released_with_servant/
* http://d.hatena.ne.jp/fjnl/touch/20111005/1317802868
* https://stackoverflow.com/questions/11423426/how-does-libuv-compare-to-boost-asio
* https://stackoverflow.com/questions/8776416/getting-to-know-the-basics-of-asynchronous-programming-on-nix
* https://lists.boost.org/Archives/boost/2011/03/178613.php
* https://cpplover.blogspot.jp/2012/10/blog-post_28.html
* https://coelhorjc.wordpress.com/2014/12/18/using-non-blocking-and-asynchronous-io-ck10-problem-in-linux-and-windows-with-epool-iocp-aiolibaio-libeventlibevlibuv-boost-asio/
* http://blog.matsumoto-r.jp/?p=2051
* http://kimitok.hateblo.jp/entry/2014/04/01/230114
* http://ascii.jp/elem/000/001/440/1440099/
* https://github.com/Rufflewind/tokio-file-unix
* https://github.com/tokio-rs/tokio-core/blob/master/examples/compress.rs
* https://jvns.ca/blog/2017/06/03/async-io-on-linux--select--poll--and-epoll/
* http://docs.libuv.org/en/v1.x/design.html
* http://docs.libuv.org/en/v1.x/threadpool.html#threadpool
* https://blog.libtorrent.org/2012/10/asynchronous-disk-io/
* http://www.kotha.net/ghcguide_ja/7.0.4/lang-parallel.html
* https://wiki.haskell.org/Concurrency
* https://social.msdn.microsoft.com/Forums/en-US/8247aa40-1e46-4a3f-b23a-a1b5501acf1e/f-vs-rx
* https://www.slideshare.net/zoetrope/reactive-systems-back-pressure
* https://gist.github.com/fand/b465ca6785c502478384
