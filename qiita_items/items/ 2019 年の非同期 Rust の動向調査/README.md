

この記事は [2018 年の非同期 Rust の動向調査](https://qiita.com/legokichi/items/64eae20155b6bc685ed3) の続報です。

## TL;DR

* Rust  1.39.0 以降で async/await 構文が使えるようになりました
* Future を実行するためのランタイムはいままで tokio だけでしたが、別に async-std というのができました
* async/await を使うライブラリを選ぶときはこの２つのランタイムのどちらで動くのかを確認しましょう。
特に理由がなければ tokio を使うのがいいでしょう。


## 2018 年からの差分

一年前から非同期 Rust を追いかけている人向けの情報です。

## 組織の再編

昨年は async/await を stabilize させるための [async-foundations](https://rustasync.github.io/team/async-foundations/) とツールチェーンを調査する [web-foundations](https://rustasync.github.io/team/web-foundations/) というグループが発足しました。 しかし諸事情により [Async Foundations WG](https://rust-lang.github.io/compiler-team/working-groups/async-await/) と [Async Ecosystem WG (rustasync)](https://github.com/rustasync/team) に再編されました。役割はほぼ同じです。 async/await stabilize されたのち [Async Ecosystem WG](https://github.com/rustasync/team)　は解散、 [Async Foundations WG](https://rust-lang.github.io/compiler-team/working-groups/async-await/) は [async-book](https://github.com/rust-lang/async-book) の編纂をしているようです。

## rustasync から async-rs へ

rustasync チームは Web フレームワーク [Tide](https://github.com/http-rs/tide) や非同期ランタイムの [romio](https://github.com/withoutboats/romio) と [julix](https://github.com/withoutboats/juliex) 、その上に構築した非同期ランタイムの抽象 [rustasync/runtime](https://github.com/rustasync/runtime) および [runtime-native](https://crates.io/crates/runtime-native) と [runtime-tokio](https://crates.io/crates/runtime-tokio) 、 [runtime-wasm](https://github.com/rustasync/runtime/issues/7) を作るなど、 std 相当の安定した非同期ツールチェーンの整備に向けて活動していました。 ところが 方針が変わり runtime クレートによる抽象化をやめて tokio とはまったく別の非同期ランタイムの [async-std](https://github.com/async-rs/async-std) を開発しました。 rustasync チーム は Async Ecosystem WG 亡き後も [async-rs](https://github.com/async-rs) と [http-rs](https://github.com/http-rs) に名前を変えて 非同期 rust 環境の整備に向けて活動しています。

## tokio と async-std の違い

tokio と async-std は TcpStream 、 UdpStream、 Timer などの実装方式が違うため、互換性がありません。
現在は tokio、async-std、および tokio ベースの actix-net ランタイムに基づく3つのエコシステムが並立しています。

runtime | [tokio](https://github.com/tokio-rs/tokio) | [async-std](https://github.com/async-rs/async-std) | [actix-rt](https://github.com/actix/actix-net/tree/master/actix-rt)<br />(tokio)
------|-----|-----|------
scheduler | work-stealing | work-stealing | actor & worker-pool
main | [tokio::main](https://docs.rs/tokio/0.2.10/tokio/attr.main.html),<br />[tokio::test](https://docs.rs/tokio/0.2.10/tokio/attr.test.html) | [async_std::main](https://docs.rs/async-std/1.4.0/async_std/attr.main.html),<br />[async_std::test](https://docs.rs/async-std/1.4.0/async_std/attr.test.html) | [actix_rt::main](https://docs.rs/actix-rt/1.0.0/actix_rt/attr.main.html),<br />[actix_rt::test](https://docs.rs/actix-rt/1.0.0/actix_rt/attr.test.html)
block_on | [tokio::runtime::Runtime::block_on](https://docs.rs/tokio/0.2.10/tokio/runtime/struct.Runtime.html#method.block_on) | [async_std::block_on](https://docs.rs/async-std/1.4.0/async_std/task/fn.block_on.html) | [actix_rt::SystemRunner::block_on](https://docs.rs/actix-rt/1.0.0/actix_rt/struct.SystemRunner.html#method.block_on),<br />[actix_rt::Runtime::block_on](https://docs.rs/actix-rt/1.0.0/actix_rt/struct.Runtime.html#method.block_on)
http trait | [tower](https://github.com/tower-rs/tower),<br />[tower-http](https://github.com/tower-rs/tower-http),<br />[tower-http-service](https://crates.io/crates/tower-http-service) | [http-service](https://crates.io/crates/http-service),<br />[http-client](https://crates.io/crates/http-client) | [actix-http](https://github.com/actix/actix-web/tree/maaster/actix-http)
http-client | [reqwest](https://crates.io/crates/reqwest) | [surf](https://crates.io/crates/surf) | [actix-http](https://github.com/actix/actix-web/tree/maaster/actix-http)
http-server | [hyper](https://crates.io/crates/hyper) | [http-service-hyper](https://crates.io/crates/http-service-hyper) | [actix-http](https://github.com/actix/actix-web/tree/maaster/actix-http)
web framework | [warp](https://crates.io/crates/warp) | [tide](https://crates.io/crates/tide) | [actix-web](https://github.com/actix/actix-web)
logger | [tracing](https://crates.io/crates/tracing) | [async-log](https://crates.io/crates/async-log) | [actix-tracing](https://github.com/actix/actix-net/tree/master/actix-tracing)
timer | [tokio::time](https://docs.rs/tokio/0.2.10/tokio/time/index.html) | [futures-timer](https://github.com/async-rs/futures-timer),<br />[async_std::future::timeout](https://docs.rs/async-std/1.4.0/async_std/future/fn.timeout.html)  | [actix_rt::time](https://docs.rs/actix-rt/1.0.0/actix_rt/time/index.html)<br />(tokio)
tls | [tokio-tls](https://crates.io/crates/tokio-tls),<br />[tokio-rustls](https://crates.io/crates/tokio-rustls) |  [async-tls](https://crates.io/crates/async-tls),<br />[async-native-tls](https://crates.io/crates/async-native-tls) | [actix-tls](https://github.com/actix/actix-net/tree/master/actix-tls)
tcp & udp | tokio | async-std | [actix-net](https://crates.io/crates/actix-net)<br />(tokio)

もともとこのような事態を避けるために runtime クレートが開発されてたのですが、 [runtime は rustasync の解散とともに開発終了](https://github.com/rustasync/runtime/pull/121) しました。また rustasync チームの「std 相当の安定したAPIを持つ非同期ランタイムを提供したい」という思惑と tokio チームの開発方針が一致しなかったことが　rustasync チームが tokio とは別に async-std を開発した原因のようです。

### ※ `#[main] async main` について

Tcp, Udp, tokio-timer などのランタイム固有の機能を使っている場合、それぞれのランタイム固有の `#[main]` を使う必要があります。

async-std の中で requwest (tokio::net) を使うと……

```rust
#[async_std::main]
async fn main() -> Result<(), reqwest::Error> {
    reqwest::get("http://example.com").await?; // thread 'main' panicked at 'not currently running on the Tokio runtime.'
}
```

tokio の中で surf (async-std) を使うと……

```rust
#[tokio::main]
async fn main() -> Result<(), surf::Exception> {
    surf::get("http://example.com/").await?.body_string().await?;
}
```

……パニックしません。実は surf 1.0.3 はデフォルトで [ブロッキング IO の libcurl](https://github.com/http-rs/surf/blob/1.0.3/src/http_client/isahc.rs) を[スレッドで待つことで動かし](https://github.com/sagebind/isahc/blob/eb2d1c04b60b2d1783e3e6d978c14a788459fd48/src/agent.rs#L79)、 [`features= ["hyper-client", "hyper-tls", "native-tls"]` をつけると内部で runtime-tokio を呼ぶ](https://github.com/http-rs/surf/blob/c046120e0aa7ff551993c245425af613c3c109bd/src/http_client/hyper.rs#L154-L161) ので動くのです。(おそらく hyper 相当の http ライブラリを tokio とは別に作るのが困難なため)


### ※ `block_on` について

Tcp, Udp, tokio-timer などのランタイム固有の機能を使っている場合、 [futures::executor::block_on](https://docs.rs/futures/0.3.1/futures/executor/fn.block_on.html)  は使うことができません。
それぞれのランタイム固有の `block_on` を使う必要があります。

futures::executor::block_on で tcp (tokio::net) を使うと……

```rust
futures::executor::block_on(async {
    reqwest::get("http://example.com/").await.ok(); // thread 'main' panicked at 'not currently running on the Tokio runtime.'
});
```
パニックします。

futures::executor::block_on で timer (tokio::time) を使うと……

```rust
use std::time::Duration;

futures::executor::block_on(async {
    tokio::time::delay_for(Duration::from_millis(10)).await; // thread 'main' panicked at 'no current timer'
});
```

パニックします。

このように現在の rust ではどのライブラリが tokio | async-std で動くのかを見分けるのは大変困難です。


## actix-web について

actix-web はもともと tokio-core を使ったアクターライブラリ actix を元に作られた Web フレームワークで、これはシングルスレッドの tokio-core を使ったワーカープールの上にアクターのランタイムを載せるものでした。ところが tokio のマルチスレッド化とともに、マルチスレッド版 tokio とは少し挙動の違う独自ランタイム([actix-rt](https://github.com/actix/actix-net/tree/master/actix-rt))となってしまいました。そのため、 [actix-rt](https://github.com/actix/actix-net/tree/master/actix-rt) の上で [tokio::blocking](https://github.com/actix/actix/pull/182) などのマルチスレッド tokio 固有の機能を使うことはできません。さらに、 actix-web は [actix-http](https://github.com/actix/actix-web/tree/maaster/actix-http) という独自の http ライブラリを使用しており、これも hyper を利用したほかのエコシステムとは一線を画してします。

## wasm-bindgen と Promise について

wasm-bindgen をつかうことで JS の Promise を Rust で await できるようになりました。これは wasm ホスト環境の JavaScript インタプリタの setTimeout や Promise などを使った非同期ランタイムとみなせます。例えば [reqwest は wasm にコンパイル可能で、 http client の実装は fetch API を使っています](https://github.com/seanmonstar/reqwest/pull/630)。詳細は [wasm-pack で JS の Promise を await できる非同期 Rust を書いて node.js で動かす](https://qiita.com/legokichi/items/dcd7d1ecd6c9b5c877d7) を参照してください。

## その他のランタイム

rust の async ランタイムは誰でも作ることができます。　tokio や async-std の他にも組み込み向けのランタイム [embrio](https://github.com/Nemo157/embrio-rs) や Google が開発中の [fuchsia OS](https://fuchsia.dev/) で使われている [ランタイム](https://fuchsia.googlesource.com/fuchsia/+/master/src/lib/fuchsia-async/) もあるようです。fuchsia については [Async Interview #2: cramertj](https://smallcultfollowing.com/babysteps/blog/2019/12/09/async-interview-2-cramertj/) が詳しいです。

## 何を使えばいいの？

async-std はエコシステムは登場してまだ半年も経っておらず 2020 年現在まだまだ開発中です。 __tokio の上で reqwest と warp (または [rweb](https://github.com/kdy1/rweb)) を使うのがいいと思います。__ AWS では rusoto および aws-lambda-rust-runtime を使うことになります。 どちらも futures-0.3 や tokio 0.2 に未対応(2020-01-26現在) です。 特に rusoto は [tokio-compat](https://github.com/tokio-rs/tokio-compat) や [futures-compat](https://docs.rs/futures-preview/0.3.0-alpha.19/futures/compat/index.html) などを使うことになるでしょう。mysql や postgres を [diesel](https://github.com/diesel-rs/diesel) で使いたい人は、[diesel はまだ非同期 IO に対応していない](https://github.com/diesel-rs/diesel/issues/2084) ので [r2d2 でコネクションプーリングをした上で](https://github.com/sfackler/r2d2) actix-rt の worker を使うか tokio::blocking を使うなどの工夫が必要です。redis クライアントは [redis-rs](https://github.com/mitsuhiko/redis-rs) でいいでしょう。

ただ現時点では actix-web のほうがサンプルコードは充実しています。参考になりそうなリポジトリへのリンクはこちらです。


* https://github.com/actix/examples
* https://github.com/actix/book
* https://github.com/seanmonstar/warp/tree/master/examples
* https://github.com/kaj/warp-diesel-ructe-sample
* https://github.com/kud1ing/Warp-Book

とはいえ Rust の非同期環境はようやく core::future と async/await が入った段階で、今後どうなっていくのかはまだまだ不透明です。

# 2019-01 から 2020-02 までの時系列

太字記事は Rust の async/await についての特にオススメの記事、および影響の大きな記事です。

* 2020-03-10 - Async Interview #7: withoutboats - https://smallcultfollowing.com/babysteps/blog/2020/03/10/async-interview-7-withoutboats/
* 2020-03-10 - Practical examples of the Rust async ecosystem - https://github.com/benkay86/async-applied

## 2020-02

* 2020-02-01 - Implementing a Copyless Redis Protocol in Rust with Parsing Combinators - https://dpbriggs.ca/blog/Implementing-A-Copyless-Redis-Protocol-in-Rust-With-Parsing-Combinators
* 2020-02-01 - Writing AWS Lambda Functions with Bastion - https://blog.bastion.rs/2020/02/01/writing-aws-lambda-functions-with-bastion.html
* 2020-02-01 - How we migrate our framework into async/await - https://medium.com/@pailee.wai/how-we-migrate-our-framework-into-async-await-c67b160e16be
* 2020-02-05 - Why Discord is switching from Go to Rust - https://blog.discordapp.com/why-discord-is-switching-from-go-to-rust-a190bbca2b1f
* 2020-02-07 - The std::future::Future of Rusoto - https://linuxwit.ch/blog/2020/02/the-future-of-rusoto/
* 2020-02-08 - __Bringing async/await to embedded Rust - https://ferrous-systems.com/blog/embedded-async-await/
* 2020-02-06 - __Futures Explained in 200 lines of Rust__ - https://cfsamson.github.io/books-futures-explained/
* 2020-02-11 - __Async Interview #6: Eliza Weisman__ - https://smallcultfollowing.com/babysteps/blog/2020/02/11/async-interview-6-eliza-weisman/
* 2020-02-11 - A primer to Rust Async - https://omarabid.com/async-rust
* 2020-02-15 - rusoto が async/.await 対応したのをきっかけにさわってみた雑感 - https://rohki.hatenablog.com/entry/2020/02/15/215157

## 2020-01

* 2020-01-01 - Actix Web: Optimization Amongst Optimizations - https://brandur.org/nanoglyphs/008-actix
* 2020-01-04 - rust on heroku with async/await and tokio - https://www.ultrasaurus.com/2020/01/rust-on-heroku-with-async-await-and-tokio/
* 2020-01-07 - Farewell to Rusoto - https://matthewkmayer.github.io/blag/public/post/farewell-rusoto/
    * rusoto の元の開発者が引退
* 2020-01-09 - Towards a Rust foundation - https://smallcultfollowing.com/babysteps/blog/2020/01/09/towards-a-rust-foundation/
* 2020-01-09 - Is there a good read on different async runtimes? - https://users.rust-lang.org/t/is-there-a-good-read-on-different-async-runtimes/36678
* 2020-01-09 - A simple Telegram bot in Rust with Actix - https://qiita.com/kimagure/items/830924fd5e8e2950b6e4
* 2020-01-11 - いちばんシンプルな非同期 Rust のランタイムを自作する + Heap Timer - https://qiita.com/legokichi/items/1beb3dce317ef45a927b
* 2020-01-13 - __Async Interview #4: Florian Gilcher__ - https://smallcultfollowing.com/babysteps/blog/2020/01/13/async-interview-4-florian-gilcher/
* 2020-01-13 - Building a Microservice with Rust - https://diego-pacheco.blogspot.com/2020/01/building-microservice-with-rust.html
* 2020-01-13 - The Why, What, and How of Pinning in Rust - https://dev.to/iam_aprogrammer/pinning-asynchronous-programming-in-rust-why-what-and-how-3eal - https://www.youtube.com/watch?v=DwY2KkCX59w
* 2020-01-14 - hyper-async-std - Attempt at using hyper with the async-std runtime - https://github.com/leo-lb/hyper-async-std
* 2020-01-14 - Tonic: 0.1 has arrived! - https://luciofran.co/tonic-0-1-release/
* 2019-01-14 - Understanding Tokio, pt. 2 - https://blog.troutwine.us/2020/01/14/understanding-tokio-pt-2/
* 2020-01-16 - Can hyper work with async-std instead of Tokio?  - https://github.com/hyperium/hyper/issues/2111
* 2020-01-16 - __warp v0.2__ - https://seanmonstar.com/post/190293882502/warp-v02
* 2020-01-16 - __Epoll, Kqueue and IOCP Explained with Rust - Cross Platform Event Queues Explained With __ust - https://cfsamsonbooks.gitbook.io/epoll-kqueue-iocp-explained/
* 2020-01-17 - __Impact of Mozilla layoffs (and future profitability) on Rust’s development and future?__ - https://users.rust-lang.org/t/impact-of-mozilla-layoffs-and-future-profitability-on-rusts-development-and-future/37011
    * mozilla のレイオフで Rust 開発者が減った話
* 2020-01-17 - Smoke-testing Rust HTTP clients - https://medium.com/@shnatsel/smoke-testing-rust-http-clients-b8f2ee5db4e6
* 2020-01-17 - __Actix project postmortem__ - https://github.com/actix/actix-web-postmortem
* 2020-01-17 - What is going on? #1 https://github.com/actix/actix-web-postmortem/issues/1
* 2020-01-17 - A sad day for Rust actix-web is dead. - https://words.steveklabnik.com/a-sad-day-for-rust
* 2020-01-17 - Actix-net unsoundness patch "is boring" - https://www.reddit.com/r/rust/comments/epszt7/actixnet_unsoundness_patch_is_boring/
* 2020-01-17 - I am done with open source. - https://mobile.twitter.com/fafhrd91/status/1218135374339301378
* 2020-01-18 - actix/actix-net#83, actix/actix-net#87 - https://gist.github.com/bb010g/705c8ffe4b9db9550a7782d25e5a53be
* 2020-01-18 - My experience porting old Rust Futures to async/await - https://medium.com/dwelo-r-d/my-experience-porting-old-rust-futures-to-async-await-744430e9c576
* 2020-01-19 - My FOSS Story - https://blog.burntsushi.net/foss/
* 2020-01-19 - Error Handling Examples - https://github.com/seanmonstar/warp/issues/388
* 2019-01-19 - Synchronized Asynchronous Job Runner in Rust - https://www.zupzup.org/rust-job-runner/
* 2020-01-20 - __Async Interview #5: Steven Fackler__ - https://smallcultfollowing.com/babysteps/blog/2020/01/20/async-interview-5-steven-fackler/
* 2020-01-20 - __Project future #1289__ - https://github.com/actix/actix-web/issues/1289
* 2020-01-20 - Actix-web is back in the main repo with a note from the Nikolay Kim - https://www.reddit.com/r/rust/comments/erdklb/actixweb_is_back_in_the_main_repo_with_a_note/
* 2020-01-20 - macros to help using warp - https://github.com/seanmonstar/warp/issues/395
* 2020-01-21 - Steps forward - https://github.com/actix/actix-web/issues/1291
* 2020-01-21 - https://twitter.com/JustM0nik4/status/1219302277690748928?s=20
* 2020-01-21 - The Thank You Thread - https://www.reddit.com/r/rust/comments/ereh5h/the_thank_you_thread/
* 2020-01-21 - Is this a rewrite of rumqtt? - https://github.com/tekjar/rumq/issues/19
* 2020-01-22 - Lessons learnt updating a library to std::future - https://cetra3.github.io/blog/mpart-async-0-3-0/
* 2020-01-22 - Rust 2020 roadmap #2857 - https://github.com/rust-lang/rfcs/pull/2857
* 2020-01-22 - Returning Trait Objects - http://bryce.fisher-fleig.org/blog/returning-trait-objects/index.html
* 2020-01-25 - __rweb: generate openapi spec file automatically from rust source code__ - https://www.reddit.com/r/rust/comments/etcev1/rweb_generate_openapi_spec_file_automatically/
* 2020-01-25 - __Build your own block_on()__ - https://stjepang.github.io/2020/01/25/build-your-own-block-on.html
* 2020-01-25 - support OpenApi #89 Please check kdy1/rweb - https://github.com/seanmonstar/warp/issues/89#issuecomment-578340768
* 2020-01-25 - A stack-less Rust coroutine library under 100 LoC - https://blog.aloni.org/posts/a-stack-less-rust-coroutine-100-loc/
* 2020-01-25 - Actix(Rust) it's blazing fast - https://diego-pacheco.blogspot.com/2020/01/actixrust-its-blazing-fast.html
* 2020-01-25 - How to use Rust Warp - https://www.steadylearner.com/blog/read/How-to-use-Rust-Warp
* 2020-01-25 - Generator Resume Arguments - https://github.com/rust-lang/rust/pull/68524
* 2020-01-27 - https://mobile.twitter.com/ubnt_intrepid/status/1221750942099337216
* 2020-01-27 - 500K pps with tokio - https://dwarfhack.com/posts/tech/tokio_pps/
* 2020-01-29 - TIDE CHANNELS - https://blog.yoshuawuyts.com/tide-channels/
* 2020-01-30 - New age of Bastion - https://blog.bastion.rs/2020/01/30/new-age-of-bastion.html
* 2020-01-31 - __Build your own executor__ - https://stjepang.github.io/2020/01/31/build-your-own-executor.html

## 2019-12

* 2019-12-01 - Implementing pid1 with Rust and async/await - https://www.reddit.com/r/rust/comments/e42mio/implementing_pid1_with_rust_and_asyncawait/ - https://tech.fpcomplete.com/rust/pid1
* 2019-12-01 - Application State - https://github.com/trezm/Thruster/issues/130
* 2019-12-01 - なぜasyncの中でformat!を使うとエラーが出るのか - https://qiita.com/__pandaman64__/items/2c2864259e0d99be964c
* 2019-12-01 - Rust Advent Calendar 2019 1日目 Rust の非同期プログラミングモデルはランタイム観点だと Go のそれに似ている - https://keno-ss.hatenadiary.org/entry/2019/12/01/235828
* 2019-12-02 - Structured Concurrency Support - https://github.com/tokio-rs/tokio/issues/1879
* 2019-12-02 - http v0.2 - https://seanmonstar.com/post/189439210962/http-v02
* 2019-12-04 - rust-async-std-tokio-compat - Compatibility layer between async-std and tokio network streams - async-std to tokio streams - https://github.com/jedisct1/rust-async-std-tokio-compat
* 2019-12-06 - okapi v0.2.0 - https://github.com/GREsau/okapi/releases/tag/v0.2.0
* 2019-12-08 - Rust Gets Async-Await - https://www.i-programmer.info/news/98-languages/13233-rust-gets-async-await.html
* 2019-12-09 - __Async Interview #2: cramertj__ - https://smallcultfollowing.com/babysteps/blog/2019/12/09/async-interview-2-cramertj/
* 2019-12-10 - __Async Interview #2: cramertj, part 2__ - https://smallcultfollowing.com/babysteps/blog/2019/12/10/async-interview-2-cramertj-part-2/
* 2019-12-11 - __Async Interview #2: cramertj, part 3__ - https://smallcultfollowing.com/babysteps/blog/2019/12/11/async-interview-2-cramertj-part-3/
* 2019-12-10 - rumq v0.1.0-alpha.1 - https://github.com/tekjar/rumq/releases/tag/v0.1.0-alpha.1
* 2019-12-11 - __Actix runtime v1.0.0__ - https://github.com/actix/actix-net/releases/tag/rt-1.0.0
* 2019-12-11 - __hyper v0.13.0__ - https://github.com/hyperium/hyper/releases/tag/v0.13.0
* 2019-12-13 - async-std v1.3.0 - https://github.com/async-rs/async-std/releases/tag/v1.3.0
* 2019-12-14 - wasm-pack で JS の Promise を await できる非同期 Rust を書いて node.js で動かす - https://qiita.com/legokichi/items/dcd7d1ecd6c9b5c877d7
* 2019-12-16 - __Stop worrying about blocking: the new async-std runtime, inspired by Go__ - https://async.rs/blog/stop-worrying-about-blocking-the-new-async-std-runtime/
* 2019-12-16 - New scheduler resilient to blocking - https://github.com/async-rs/async-std/pull/631
* 2019-12-17 - Tokio Roadmap to 1.0 - https://github.com/tokio-rs/tokio/pull/1965
* 2019-12-17 - __Announcing Mio 0.7-alpha.1__ - https://tokio.rs/blog/2019-12-mio-v0.7-alpha.1/
* 2019-12-17 - Rust no-stdのasync完全理解を目指そう！ - https://tomo-wait-for-it-yuki.hatenablog.com/entry/2019/12/17/073126
* 2019-12-18 - http-client 1.1.1 - https://github.com/http-rs/http-client/releases/tag/1.1.1
* 2019-12-18 - tokio-compat 0.11 - https://github.com/tokio-rs/tokio-compat/releases/tag/0.1.1 
* 2019-12-18 - __Announcing Tokio-Compat__ - https://tokio.rs/blog/2019-12-compat/
* 2019-12-20 - Do you plan to migrate from tokio to async-std when it matures ? - https://github.com/actix/actix-net/issues/79
* 2019-12-20 - async fn and async block not support task::current().id(),how to get Coroutines id? - https://github.com/tokio-rs/tokio/issues/1996
* 2019-12-20 - tower 0.3.0 - https://github.com/tower-rs/tower/releases/tag/tower-0.3.0
* 2019-12-20 - wasm-bindgen - 0.2.56 - https://github.com/rustwasm/wasm-bindgen/releases/tag/0.2.56 - https://github.com/rustwasm/wasm-bindgen/blob/66e48bd1681fab6ce99a8e3d9230efebaa24d67f/CHANGELOG.md#0256
    * wasm-bindgen で JS の Promise を Future にして await できるようになった
* 2019-12-21 - tide 0.5.0 -  https://github.com/http-rs/tide/releases/tag/0.5.0
* 2019-12-22 - __How to Detect Accidental Blocking Code in Async Rust__ - https://rickyhan.com/jekyll/update/2019/12/22/convert-to-async-rust.html
* 2019-12-21 - async-std v1.4.0 - https://github.com/async-rs/async-std/releases/tag/v1.4.0
* 2019-12-21 - __STREAMS CONCURRENCY__ - https://blog.yoshuawuyts.com/streams-concurrency/
* 2019-12-22 - thruster 0.8.0 - https://crates.io/crates/thruster/0.8.0
* 2019-12-22 - tokioで外部コマンド実行 - https://qiita.com/Kumassy/items/3fb3e52729e375efd5ed
* 2019-12-23 - __Async Interview #3: Carl Lerche__ - https://smallcultfollowing.com/babysteps/blog/2019/12/23/async-interview-3-carl-lerche/
* 2019-12-24 - Async Exceptions in Haskell, and Rust - https://tech.fpcomplete.com/blog/async-exceptions-haskell-rust
* 2019-12-25 - __Actix web 2.0.0__ is released - https://mobile.twitter.com/fafhrd91/status/1209878253802004480
* 2019-12-25 - async/await 時代の新しい HTTP サーバーフレームワーク tide を試す - https://yuk1tyd.hatenablog.com/entry/2019/12/25/000100
* 2019-12-26 - You can try this branch async-await  - https://github.com/awslabs/aws-lambda-rust-runtime/issues/14#issuecomment-569046122
* 2019-12-27 - Preventing the Collapse of Civilization - https://brandur.org/nanoglyphs/007-civilization
* 2019-12-27 - Sessions - https://github.com/seanmonstar/warp/issues/342#issuecomment-569213444
* 2019-12-28 - Announcing SQLx, a fully asynchronous pure Rust client library for Postgres and MySQL/MariaDB with compile-time checked queries - https://www.reddit.com/r/rust/comments/egpw7g/announcing_sqlx_a_fully_asynchronous_pure_rust/ - https://github.com/launchbadge/sqlx
* 2019-12-28 - Rust + GraphQL + Juniper + Diesel + Postgres + Actix - https://github.com/lucperkins/rust-graphql-juniper-actix-diesel-postgres
* 2019-12-30 - __reqwest v0.10__ - https://seanmonstar.com/post/189960517042/reqwest-v010
* 2019-12-30 - Deno, first approach - https://dev.to/lsagetlethias/deno-first-approach-4d0
* 2019-12-31 - Understanding Tokio, pt. 1 - https://blog.troutwine.us/2019/12/31/understanding-tokio-pt1/

## 2019-11

* 2019-11-01 - tide 0.3.0 -  https://github.com/http-rs/tide/releases/tag/0.3.0
* 2019-11-03 - Experimental cooperative cancellation for async-std - https://github.com/async-rs/stop-token
* 2019-11-04 - __Blocking inside async code__ - https://stjepang.github.io/2019/12/04/blocking-inside-async-code.html
* 2019-11-04 - Announcing genawaiter – use generators (yield) on stable Rust - https://users.rust-lang.org/t/announcing-genawaiter-use-generators-yield-on-stable-rust/34283 - https://github.com/whatisaphone/genawaiter
* 2019-11-05 - Rust 2020 - https://www.ncameron.org/blog/rust-2020/
* 2019-11-07 - __Async-await on stable Rust!__ - https://blog.rust-lang.org/2019/11/7/Async-await-stable.html
* 2019-11-07 - __Announcing Rust 1.39.0__ - https://blog.rust-lang.org/2019/11/7/Rust-1.39.0.html
* 2019-11-07 - __Proposing new `AsyncRead` / `AsyncWrite` traits__ - https://github.com/tokio-rs/tokio/pull/1744
* 2019-11-08 - Rust 1.39を早めに深掘り - https://tech-blog.optim.co.jp/entry/2019/11/08/080000
* 2019-11-08 - __Rustの非同期プログラミングをマスターする__ - https://tech-blog.optim.co.jp/entry/2019/11/08/163000
* 2019-11-07 - async-std 0.99.12: async/.await is ready! - https://async.rs/blog/async-std-0-9-12-async-await-is-ready/
* 2019-11-07 - __futures-rs 0.3.0__ - https://github.com/rust-lang/futures-rs/releases/tag/0.3.0
* 2019-11-07 - Using Rust in Windows - https://msrc-blog.microsoft.com/2019/11/07/using-rust-in-windows/
* 2019-11-08 - surf 1.0.3 - https://github.com/http-rs/surf/releases/tag/1.0.3
* 2019-11-08 - https://mobile.twitter.com/yoshuawuyts/status/1192516304827228162
* 2019-11-10 - hyper v0.13 - https://seanmonstar.com/post/189594157852/hyper-v013
* 2019-11-11 - Announcing async-std 1.0 - https://async.rs/blog/announcing-async-std-1-0/ - https://www.reddit.com/r/rust/comments/duvdzz/announcing_asyncstd_10/
* 2019-11-11 - Demystifying Asynchronous Rust - https://github.com/teh-cmc/rust-async - https://teh-cmc.github.io/rust-async/html/
* 2019-11-12 - async-std v1.0.0 - https://github.com/async-rs/async-std/releases/tag/v1.0.0
* 2019-11-12 - Rust and 2020 - https://vorner.github.io/2019/11/12/rust-2020.html
* 2019-11-12 - __demo for rust asynchronous io: from mio to stackless coroutine__ - https://github.com/Hexilee/async-io-demo
* 2019-11-13 - ERROR HANDLING SURVEY - https://blog.yoshuawuyts.com/error-handling-survey/
* 2019-11-14 - Thoughts on Error Handling in Rust - https://lukaskalbertodt.github.io/2019/11/14/thoughts-on-error-handling-in-rust.html
* 2019-11-14 - __Global Executors__ - https://boats.gitlab.io/blog/post/global-executors/
* 2019-11-14 - https://mobile.twitter.com/qnighy/status/1194764795074887680
* 2019-11-15 - Global Executors - https://internals.rust-lang.org/t/global-executors/11295
* 2019-11-15 - Consider matching `std::process` behavior - https://github.com/tokio-rs/tokio/issues/1771
* 2019-11-16 - async-stream v0.2.0 - https://github.com/tokio-rs/async-stream/releases/tag/v0.2.0
* 2019-11-21 - Await Trust-DNS no longer - https://bluejekyll.github.io/blog/rust/2019/12/21/await-trust-dns.html
* 2019-11-22 - async-std v1.1.0 - https://github.com/async-rs/async-std/releases/tag/v1.1.0
* 2019-11-22 - https://mobile.twitter.com/ubnt_intrepid/status/1197773414271873024
* 2019-11-22 - __Announcing the Async Interviews__ - https://smallcultfollowing.com/babysteps/blog/2019/11/22/announcing-the-async-interviews/
* 2019-11-23 - Async, Awaited - https://brandur.org/nanoglyphs/004-async-awaited
* 2019-11-24 - Cleanup support in Signal Hook - https://vorner.github.io/2019/11/24/signal-hook-cleanup.html
* 2019-11-26 - Is this compatible with async-std? - https://github.com/seanmonstar/reqwest/issues/719
* 2019-11-26 - __Announcing Tokio 0.2 and a Roadmap to 1.0__ - https://tokio.rs/blog/2019-11-tokio-0-2/
* 2019-11-27 - tokio-rustls 0.12.0 - https://crates.io/crates/tokio-rustls/0.12.0
* 2019-11-27 - プロダクションのRustコードを async / await に移行した話 - https://blog.idein.jp/post/189326134515/rust-async-await
* 2019-11-27 - __TIDE__ - https://blog.yoshuawuyts.com/tide/
* 2019-11-27 - __tokio 0.2.0__ - https://github.com/tokio-rs/tokio/releases/tag/tokio-0.2.0
* 2019-11-28 - tide 0.3.0 -  https://github.com/http-rs/tide/releases/tag/0.3.0
* 2019-11-28 - __Async Interview #1: Alex and Nick talk about async I/O and WebAssembly__ - https://smallcultfollowing.com/babysteps/blog/2019/11/28/async-interview-1-alex-and-nick-talk-about-async-i-o-and-webassembly/
* 2019-11-28 - async-std v1.2.0 - https://github.com/async-rs/async-std/releases/tag/v1.2.0
* 2019-11-29 - `join!` - Rust macros with combinators to join everything - https://www.reddit.com/r/rust/comments/e3evz1/join_rust_macros_with_combinators_to_join/
* 2019-11-30 - __Async Interviews__ - https://users.rust-lang.org/t/async-interviews/35167

## 2019-10

* 2019-10-01 - tokio 0.2.0-alpha.6 - https://crates.io/crates/tokio/0.2.0-alpha.6
* 2019-10-02 - Tonic: gRPC has come to async/await! - https://luciofran.co/tonic-grpc-has-come-to-async-await/
* 2019-10-04 - Actix server v0.7.0 - https://github.com/actix/actix-net/releases/tag/server-v0.7.0
* 2019-10-07 - __Async Foundations Update: Time for polish!__ - https://blog.rust-lang.org/inside-rust/2019/10/7/AsyncAwait-WG-Focus-Issues.html
* 2019-10-07 - Will it be possible to use async-await in a tower service?- https://github.com/tower-rs/tower/issues/358
* 2019-10-08 - reqwest alpha.await - https://seanmonstar.com/post/188220739932/reqwest-alphaawait
* 2019-10-09 - __The Node Experiment - Exploring Async Basics with Rust__ - https://cfsamson.github.io/book-exploring-async-basics/
* 2019-10-10 - futures-timer 1.0.0 - https://github.com/async-rs/futures-timer/releases/tag/1.0.0
* 2019-10-11 - __Improving async-await's "Future is not Send" diagnostic__ - https://blog.rust-lang.org/inside-rust/2019/10/11/AsyncAwait-Not-Send-Error-Improvements.html
* 2019-10-13 - __Making the Tokio scheduler 10x faster__ - https://tokio.rs/blog/2019-10-scheduler/
* 2019-10-14 - __Opensource and ownership transfer__ - https://github.com/AtherEnergy/rumqtt/issues/179
* 2019-10-16 - __Asynchronous Destructors__ - https://boats.gitlab.io/blog/post/poll-drop/
* 2019-10-16 - Rust's Journey to Async/await - https://www.infoq.com/presentations/rust-2019/
* 2019-10-26 - __Making an opinionated Web framework__ - https://speakerdeck.com/qnighy/making-an-opinionated-web-framework
* 2019-10-26 - __why async fn in traits are hard__ - https://smallcultfollowing.com/babysteps/blog/2019/10/26/async-fn-in-traits-are-hard/
* 2019-10-27 - tokio vs async-std ? - https://www.reddit.com/r/rust/comments/dngig6/tokio_vs_asyncstd/
* 2019-10-27 - Trying to `futures::executor::block_on(run_server())` gives runtime error - https://github.com/hyperium/hyper/issues/1995#issuecomment-554997179
* 2019-10-30 - The Rustasync working group has sunset Runtime is no longer active - https://github.com/rustasync/team
* 2019-10-31 - Can anyone give me a high level summary of the difference between Tokio and Async-std, as well as how async works in rust? - https://www.reddit.com/r/rust/comments/dpjlkt/can_anyone_give_me_a_high_level_summary_of_the/f5wkmvn/
* 2019-10-30 - deprecate - https://github.com/rustasync/runtime/pull/121
* 2019-10-31 - https://twitter.com/Linda_pp/status/1189734163139555329

## 2019-09

* 2019-09-02 - __FUTURES CONCURRENCY__ - https://blog.yoshuawuyts.com/futures-concurrency/
* 2019-09-02 - __How Rust optimizes async/await II: Program analysis__ - https://tmandry.gitlab.io/blog/posts/optimizing-await-2/
* 2019-09-03 - RustでRuntimeを使用してmainをasyncにする - https://sioyaki.com/entry/2019/09/03/152447
* 2019-09-04 - Pinning and Service - https://github.com/tower-rs/tower/issues/319
* 2019-09-04 - hyper alpha supports async/await - https://seanmonstar.com/post/187493499882/hyper-alpha-supports-asyncawait
* 2019-09-12 - async-stream v0.1.1 - https://github.com/tokio-rs/async-stream/releases/tag/v0.1.1
* 2019-09-13 - tokio-0.2.0-alpha4` `AsyncRead` and `AsyncWrite` traits are not the same as `futures-0.3.0-alpha18` versions - https://github.com/tokio-rs/tokio/issues/1551
* 2019-09-14 - Tracking issue for std::future migration - https://github.com/actix/actix-net/issues/45
* 2019-09-14 - The Why, What, and How of Pinning in Rust - https://www.youtube.com/watch?v=DkMwYxfSYNQ&feature=youtu.be
* 2019-09-15 - __Playing with the new async__ - https://vorner.github.io/2019/09/15/play-with-new-async.html
* 2019-09-18 - mio-timerfd 0.1.0 - https://github.com/oefd/mio-timerfd - https://crates.io/crates/mio-timerfd/0.1.0
* 2019-09-19 - Tide's Future Direction - https://github.com/http-rs/tide/issues/325
* 2019-09-19 - Using async-std (was reqwest) - https://users.rust-lang.org/t/using-async-std-was-reqwest/32735
* 2019-09-20 - Will crates like tokio, mio and futures be still needed after async/await gets stabilized? - https://old.reddit.com/r/rust/comments/d6pw43/will_crates_like_tokio_mio_and_futures_be_still/f0v27a4/
* 2019-09-20 - tokio 0.2.0-alpha.5 - https://github.com/tokio-rs/tokio/releases/tag/tokio-0.2.0-alpha.5 - https://crates.io/crates/tokio/0.2.0-alpha.5
* 2019-09-20 - https://mobile.twitter.com/qnighy/status/1175057815313907712
* 2019-09-21 - __ASYNC BUILDERS__ - https://blog.yoshuawuyts.com/async-finalizers/
* 2019-09-22 - https://mobile.twitter.com/qnighy/status/1175699769865359362
* 2019-09-22 - https://mobile.twitter.com/qnighy/status/1175678940926464000
* 2019-09-22 - https://mobile.twitter.com/qnighy/status/1175709767685177345
* 2019-09-24 - rumqtt v0.31.0 - https://github.com/AtherEnergy/rumqtt/releases/tag/v0.31.0
* 2019-09-27 - WASM Support - https://github.com/seanmonstar/reqwest/pull/630
* 2019-09-26 - Async Ecosystem Working Group Meeting #30 - https://github.com/rustasync/team/issues/143
* 2019-09-27 - futures-preview 0.3.0-alpha.19 - https://crates.io/crates/futures-preview/0.3.0-alpha.19
* 2019-09-29 - __Fighting the Async fragmentation__ - https://vorner.github.io/2019/09/29/figthting-the-async-fragmentation.html
    * async ランタイムの断片化を心配する話
* 2019-09-30 - __Async-await hits beta!__ - https://blog.rust-lang.org/2019/09/30/Async-await-hits-beta.html
    * Async Ecosystem WG が廃止
    * Async Foundations WG は async-book の整備に

## 2019-08

* 2019-08-02 - Zero-Cost Asynchronous Programming in Rust - https://blog.knoldus.com/zero-cost-asynchronous-programming-in-rust/
* 2019-08-03 - __Rustのasync/awaitをスムーズに使うためのテクニック__ - https://qiita.com/qnighy/items/59133e69a0ba0c6a7fef
* 2019-08-06 - Async I/O TransactionGuard - https://github.com/diesel-rs/diesel/issues/399#issuecomment-518422793
* 2019-08-08 - Swagger/OpenAPI Spec generation - https://www.reddit.com/r/rust/comments/cn9wrc/swaggeropenapi_spec_generation/
* 2019-08-08 - __Tokio alpha release with async & await__ - https://tokio.rs/blog/2019-08-alphas/
* 2019-08-09 - tokio 0.2.0-alpha.1 - https://crates.io/crates/tokio/0.2.0-alpha.1
* 2019-08-09 - dtolnay::_01__await_a_minute - [https://docs.rs/dtolnay/0.0.3/dtolnay/macro_01__await_a_minute.html](https://docs.rs/dtolnay/0.0.3/dtolnay/macro._01__await_a_minute.html)
* 2019-08-11 - Runtime configuration reloading - https://vorner.github.io/2019/08/11/runtime-configuration-reloading.html
* 2019-08-16 - __Announcing async-std__ - https://async.rs/blog/announcing-async-std/ - https://news.ycombinator.com/item?id=20719095
* 2019-08-16 - https://mobile.twitter.com/asyncrs/status/1162393724745322496
* 2019-08-17 - Compatibility with tokio? - https://github.com/async-rs/async-std/issues/54
* 2019-08-17 - https://mobile.twitter.com/yoshuawuyts/status/1162492948430413824
* 2019-08-17 - https://mobile.twitter.com/Argorak/status/1162514356560699396
* 2019-08-18 - __How Rust optimizes async/await I__ - https://tmandry.gitlab.io/blog/posts/optimizing-await-1/
* 2019-08-20 - futures-preview - 0.3.0-alpha.18 - https://crates.io/crates/futures-preview/0.3.0-alpha.18
* 2019-08-21 - https://mobile.twitter.com/yaahc_/status/1164176227722121216
* 2019-08-11 - async-log - 2.0.0 - https://github.com/async-rs/async-log/releases/tag/2.0.0
* 2019-08-12 - actix-webの柔軟なリクエストハンドラの仕組み - http://blog.endflow.net/flex-handler/
* 2019-08-13 - Rust 1.38 is scheduled to branch off today,  but this PR didn't land yet and there are a bunch of blockers still open. - https://github.com/rust-lang/rust/pull/63209#issuecomment-520741844
* 2019-08-14 - __SURF__ - https://blog.yoshuawuyts.com/surf/
* 2019-08-14 - __Diagnostics with Tracing__ - https://tokio.rs/blog/2019-08-tracing/
* 2019-08-14 - AWS’ sponsorship of the Rust project - https://aws.amazon.com/jp/blogs/opensource/aws-sponsorship-of-the-rust-project/
* 2019-08-14 - Should the standard library have a basic Future runtime? - https://internals.rust-lang.org/t/should-the-standard-library-have-a-basic-future-runtime/10705/11
* 2019-08-15 - Announcing Rust 1.37.0 - https://blog.rust-lang.org/2019/8/15/Rust-1.37.0.html
* 2019-08-15 - async generator changes - https://github.com/Nemo157/embrio-rs/pull/23
* 2019-08-15 - Understanding Futures in Rust -- Part 2 - https://www.viget.com/articles/understanding-futures-is-rust-part-2/-
* 2019-08-17 - async-std v0.99.0 - https://github.com/async-rs/async-std/releases/tag/v0.99.0
* 2019-08-17 - Announcing async-std beta: an async port of the Rust standard library - https://www.reddit.com/r/rust/comments/cr85pp/announcing_asyncstd_beta_an_async_port_of_the/
* 2019-08-17 - fred.rs 1.1.1 - https://github.com/azuqua/fred.rs/releases/tag/1.1.1
* 2019-08-18 - tokio 0.2.0-alpha.2 - https://crates.io/crates/tokio/0.2.0-alpha.2
* 2019-08-21 - Remove async_await feature - https://github.com/rust-lang/async-book/pull/33
* 2019-08-23 - runtime-native 0.3.0-alpha.6 - https://crates.io/crates/runtime-native/0.3.0-alpha.6
* 2019-08-24 - replace `redis-async` with `redis` crate - https://github.com/actix/actix-redis/pull/30
* 2019-08-25 - __Poll: Async/Await, let’s talk about executors!__ - https://users.rust-lang.org/t/poll-async-await-lets-talk-about-executors/31753
* 2019-08-27 - __Async Stack Traces in Rust__ - https://fitzgeraldnick.com/2019/08/27/async-stacks-in-rust.html
* 2019-08-28 - Running `RusotoFuture` on `tokio:0.2.0-alpha.2` - https://github.com/rusoto/rusoto/issues/1493
* 2019-08-28 - rust から redis を非同期で使う 2019夏 - https://qiita.com/hadashiA/items/22f331c8b407011fa7fa
* 2019-08-29 - surf 1.0.1 - https://github.com/http-rs/surf/releases/tag/1.0.1
* 2019-08-29 - tokio 0.2.0-alpha.3 - https://crates.io/crates/tokio/0.2.0-alpha.3
* 2019-08-30 - tokio 0.2.0-alpha.4 - https://crates.io/crates/tokio/0.2.0-alpha.4
* 2019-08-30 - Async ecosystems newsletter/blog post #1 - https://github.com/rustasync/team/issues/142


## 2019-07

* 2019-07-04 - tokio - 0.1.22 - https://crates.io/crates/tokio/0.1.22
* 2019-07-04 - futures-preview 0.3.0-alpha.17 - https://crates.io/crates/futures-preview/0.3.0-alpha.17
* 2019-07-04 - __Announcing Rust 1.36.0__ - https://blog.rust-lang.org/2019/7/4/Rust-1.36.0.html
    * core::future が stable に入った
* 2019-07-05 - __Rustの未来いわゆるFuture__ - https://tech-blog.optim.co.jp/entry/2019/07/05/173000
* 2019-07-05 - Update to std::future::Future futures/0.3 - https://github.com/actix/actix-web/issues/955
* 2019-07-06 - __Rustのasync/awaitの特徴4つ__ - https://qiita.com/qnighy/items/05c38f73ef4b9e487ced
* 2019-07-07 - __RustのFutureとそのRunnerを作ってみた__ - https://keens.github.io/blog/2019/07/07/rustnofuturetosonorunnerwotsukuttemita/
* 2019-07-08 - __Async-await status report #2__ - https://smallcultfollowing.com/babysteps/blog/2019/07/08/async-await-status-report-2/
* 2019-07-09 - Migrating a crate from futures 0.1 to 0.3 - https://www.ncameron.org/blog/migrating-a-crate-from-futures-0-1-to-0-3/
* 2019-07-17 - Notes on a smaller Rust - https://boats.gitlab.io/blog/post/notes-on-a-smaller-rust/
* 2019-07-19 - Determine the future of tower-web -https://github.com/carllerche/tower-web/issues/218
* 2019-07-27 - openapi-generatorで生成されたrustのコードでエラーがでる。 - https://ja.stackoverflow.com/questions/56943/openapi-generator%E3%81%A7%E7%94%9F%E6%88%90%E3%81%95%E3%82%8C%E3%81%9Frust%E3%81%AE%E3%82%B3%E3%83%BC%E3%83%89%E3%81%A7%E3%82%A8%E3%83%A9%E3%83%BC%E3%81%8C%E3%81%A7%E3%82%8B
* 2019-07-31 - Paperclip 0.3.0 released! - https://www.reddit.com/r/rust/comments/cjw534/paperclip_030_released/

## 2019-06

* 2019-06-04 - __Removing libpq (to enable async)__ - https://github.com/diesel-rs/diesel/issues/2084
* 2019-06-05 - __actix-web 1.0.0__ - https://github.com/actix/actix-web/releases/tag/web-v1.0.0
* 2019-06-05 - Actix-web 1.0 released - https://www.reddit.com/r/rust/comments/bwy99w/actixweb_10_released/
* 2019-06-12 - 「Rustで始めるネットワークプログラミング」を出版しました。 - https://cha-shu00.hatenablog.com/entry/2019/06/12/231526
* 2019-06-13 - __Green Threads Explained in 200 Lines of Rust__ - https://cfsamson.gitbook.io/green-threads-explained-in-200-lines-of-rust/
* 2019-06-14 - AWS Kinesis ひたすら読みにいく k-iter の複数 shard 対応(と、Rust の Future/Stream/Tokio 話) - https://rohki.hatenablog.com/entry/2019/06/24/210938
* 2019-06-17 - runtime-native 0.3.0-alpha.4 - https://crates.io/crates/runtime-native/0.3.0-alpha.4
* 2019-06-18 - Rust Creator Graydon Hoare Talks About Security, History, and Rust - https://thenewstack.io/rust-creator-graydon-hoare-talks-about-security-history-and-rust/
* 2019-06-19 - How Rust Views Tradeoffs - https://www.infoq.com/presentations/rust-tradeoffs/
* 2019-06-20 - Distributed Systems Training in Go and Rust - https://pingcap.com/blog/distributed-systems-training-in-go-and-rust/
* 2019-06-20 - __RUST STREAMS__ - https://blog.yoshuawuyts.com/rust-streams/
* 2019-06-20 - tokio::spawn failed (is a tokio runtime running this future?) - https://github.com/http-rs/tide/issues/280
* 2019-06-24 - Survivor skills - https://aturon.github.io/personal/2019/06/24/survivor-skills/
* 2019-06-25 - Back in the saddle - https://aturon.github.io/tech/2019/06/25/back-in-the-saddle/
* 2019-06-27 - runtime-native 0.3.0-alpha.5 - https://crates.io/crates/runtime-native/0.3.0-alpha.5
* 2019-06-27 - futures-rs - 0.1.28 - https://github.com/rust-lang/futures-rs/releases/tag/0.1.28
* 2019-06-29 - __ASYNC LOG__ - https://blog.yoshuawuyts.com/async-log/ - https://mobile.twitter.com/yoshuawuyts/status/1144750481631301632
* 2019-06-29 - Lambda with Async/Await - https://github.com/awslabs/aws-lambda-rust-runtime/pull/111

## 2019-05

* 2019-05-06 - __A final proposal for await syntax__ - https://boats.gitlab.io/blog/post/await-decision/
* 2019-05-06 - redisconf19 - Writing Redis Modules In Rust - https://www.reddit.com/r/rust/comments/bl0vew/redisconf19_writing_redis_modules_in_rust/
* 2019-05-09 - tokio console - https://github.com/tokio-rs/console
* 2019-05-10 - async support - https://github.com/awslabs/aws-lambda-rust-runtime/pull/106
* 2019-05-11 - futures-preview 0.3.0-alpha.16 - https://crates.io/crates/futures-preview/0.3.0-alpha.16
* 2019-05-12 - Rewrite async/await in their entirety - https://github.com/Nemo157/embrio-rs/pull/13
* 2019-05-13 - runtime-native 0.3.0-alpha.3 - https://crates.io/crates/runtime-native/0.3.0-alpha.3
* 2019-05-15 - 4 years of Rust - https://blog.rust-lang.org/2019/5/15/4-Years-Of-Rust.html
* 2019-05-16 - __Zero Cost Abstractions__ - https://boats.gitlab.io/blog/post/zero-cost-abstractions/
* 2019-05-18 - https://mobile.twitter.com/yutakashino/status/1129529590697717760
* 2019-05-23 - Announcing Rust 1.35.0 - https://blog.rust-lang.org/2019/5/23/Rust-1.35.0.html
    * dbg! マクロ
* 2019-05-27 - Can we reduce the burden of cancel-correctness for async Futures? - https://internals.rust-lang.org/t/can-we-reduce-the-burden-of-cancel-correctness-for-async-futures/10278
* 2019-05-28 - __Async/Await - The challenges besides syntax - Cancellation__ - https://gist.github.com/Matthias247/ffc0f189742abf6aa41a226fe07398a8
* 2019-05-28 - __Update on await syntax__ - https://boats.gitlab.io/blog/post/await-decision-ii/
* 2019-05-31 - tokio 0.1.21 - https://crates.io/crates/tokio/0.1.21

## 2019-04

* 2019-04-02 - Explained: How does async work in Rust? - https://levelup.gitconnected.com/explained-how-does-async-work-in-rust-c406f411b2e2 -  https://dev.to/gruberb/explained-how-does-async-work-in-rust-46f8
* 2019-04-11 - thruster 0.7.0 - https://crates.io/crates/thruster/0.7.0
* 2019-04-15 - __for await loops (Part I)__ - https://boats.gitlab.io/blog/post/for-await-i/
* 2019-04-16 - __RUNTIME__ - https://blog.yoshuawuyts.com/runtime/
    * Async Ecosystem WG は std と同じ使い勝手の async 環境のために runtime の抽象化を試みていた
* 2019-04-16 - futures-preview 0.3.0-alpha.14 - https://crates.io/crates/futures-preview/0.3.0-alpha.14
* 2019-04-17 - runtime-native 0.3.0-alpha.1 - https://crates.io/crates/runtime-native/0.3.0-alpha.1
* 2019-04-18 - Futures 0.1 Compatibility Layer - https://rust-lang.github.io/futures-rs/blog/2019/04/18/compatibility-layer.html
* 2019-04-15 - tide 0.1.0 - https://github.com/http-rs/tide/releases/tag/0.1.0
* 2019-04-15 - Understanding Futures In Rust -- Part 1 - https://www.viget.com/articles/understanding-futures-in-rust-part-1/
* 2019-04-17 - Announcing Runtime - https://internals.rust-lang.org/t/announcing-runtime/9825
* 2019-04-21 - The Nuclear Reactor Design Pattern - https://vorner.github.io/2019/04/21/nuclear-reactor-design-pattern.html
* 2019-04-21 - async-timer 0.1.0 - https://github.com/DoumanAsh/async-timer - https://crates.io/crates/async-timer/0.1.0
* 2019-04-22 - RustLatam 2019 - Without Boats: Zero-Cost Async IO - https://www.youtube.com/watch?v=skos4B5x7qE
* 2019-04-23 - [tracking issue] timers - https://github.com/rustasync/runtime/issues/14
* 2019-04-24 - RustLatam 2019 - Without Boats: Zero-Cost Async IO - https://www.reddit.com/r/rust/comments/bgikha/rustlatam_2019_without_boats_zerocost_async_io/
* 2019-04-25 - Made timerfd implementation with romio - https://github.com/rustasync/team/issues/14#issuecomment-486279799
* 2019-04-27 - tower 0.1.0 - https://github.com/tower-rs/tower/releases/tag/tower-0.1.0
* 2019-04-27 - 0.3.0-alpha.15 - https://crates.io/crates/futures-preview/0.3.0-alpha.15
* 2019-04-28 - async is not zero-cost - https://www.reddit.com/r/rust/comments/bi9yzs/async_is_not_zerocost/
* 2019-04-29 - juliex 0.3.0-alpha.6 - https://github.com/withoutboats/juliex/releases/tag/0.3.0-alpha.6
* 2019-04-29 - runtime-native 0.3.0-alpha.2 - https://crates.io/crates/runtime-native/0.3.0-alpha.2
* 2019-04-29 - http-service 0.2.0 - https://github.com/http-rs/http-service/releases/tag/http-service-0.2.0

## 2019-03

* 2019-03-01 - __Async-await status report__ - https://smallcultfollowing.com/babysteps/blog/2019/03/01/async-await-status-report/
    * await 論争
* 2019-03-05 - Actix web 0.7.19 - https://github.com/actix/actix-web/releases/tag/v0.7.19
* 2019-03-14 - Question: Is it possible to use warp on zeit or aws lambda - https://github.com/seanmonstar/warp/issues/190
* 2019-03-15 - Switching to tower::Service - https://github.com/hyperium/hyper/issues/1782
* 2019-03-19 - Async IO in Rust and Haskell - https://slides.com/wraithm/async-io-in-rust-and-haskell/
* 2019-03-19 - Explained: Futures in Rust for Web Development - https://dev.to/gruberb/explained-rust-futures-for-web-development-a10
* 2019-03-25 - __std::pin の勘所__ - https://www.slideshare.net/HiroakiGoto/stdpin


## 2019-02

* 2019-02-04 - osaka.rs - https://aep.github.io/rust-async-without-the-noise/ - https://github.com/aep/osaka - https://news.ycombinator.com/item?id=19104065 
* 2019-02-04 - Rust Governance: Scaling Empathy - https://manishearth.github.io/blog/2019/02/04/rust-governance-scaling-empathy/
* 2019-02-09 - async/awaitと合成可能性 - https://keens.github.io/blog/2019/02/09/async_awaittogouseikanousei/
* 2019-02-10 - osaka.rs メモ - https://qiita.com/maueki/items/c0bb363120e0f01d27ec
* 2019-02-11 - __Generators I: Toward a minimum viable product__ - https://boats.gitlab.io/blog/post/generators-i/
* 2019-02-14 - futures-native-timers - https://github.com/tinaun/futures-native-timers
* 2019-02-18 - __Generators II: The Question Mark Problem__ - https://boats.gitlab.io/blog/post/generators-ii/
* 2019-02-21 - futures-preview 0.3.0-alpha.13 - https://crates.io/crates/futures-preview/0.3.0-alpha.13
* 2019-02-22 - Rust lang team working groups - https://smallcultfollowing.com/babysteps/blog/2019/02/22/rust-lang-team-working-groups/
* 2019-02-22 - Toy Future combinators with async/await syntax - https://www.reddit.com/r/rust/comments/at6gh7/toy_future_combinators_with_asyncawait_syntax/
* 2019-02-27 - __ASYNC ECOSYSTEM WG__ - https://blog.yoshuawuyts.com/async-ecosystem-wg/
    * Async Ecosystem WG (rustasync) は tide, romio, juliex を開発


## 2019-01

* 2019-01-07 - __The Waker API I: what does a waker do?__ - https://boats.gitlab.io/blog/post/wakers-i/
* 2019-01-10 - HTTP Client Trait - https://github.com/hyperium/http/issues/288
    * carllerche は tower が http server/cliernt 抽象化層になるはずとのこと
* 2019-01-11 - __The Waker API II: waking across threads__ - https://boats.gitlab.io/blog/post/wakers-ii/
    * LocalWaker がいらない理由
* 2019-01-16 - futures-preview 0.3.0-alpha.12 - https://crates.io/crates/futures-preview/0.3.0-alpha.12
* 2019-01-18 - Move to Rustasync organization ? - https://github.com/withoutboats/juliex/issues/21
* 2019-01-18 - Move to Rustasync organization ? - https://github.com/withoutboats/romio/issues/94
* 2019-01-25 - aws-lambda-rust-runtime v0.2.0 - https://github.com/awslabs/aws-lambda-rust-runtime/releases/tag/v0.2.0
* 2019-01-28 - tracing-core 0.1.0 - https://github.com/tokio-rs/tracing/releases/tag/tracing-core-0.1.0
* 2019-01-31 - Tracking issue: timer bindings #62 - https://github.com/withoutboats/romio/issues/62

