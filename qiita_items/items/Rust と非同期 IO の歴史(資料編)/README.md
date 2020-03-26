* この記事は [Rust と非同期 IO の歴史](https://qiita.com/DUxCA/items/882bcacd12870d087555) の草稿兼参考資料集です

## 最近のサーバサイド Rust の話題

--------

* iron, gotham が開発停止
* tokio-core が tokio になった
* RFCS に async-await が入った
* futures 0.2 が yank されそう

--------

## 疑問

--------

* 2018 年にもなって非同期 IO で疲弊している
* Rust は 2006 年に開発が始まった
* node.js も go も 2009 年には存在していた
* __今まで Rust は何をやっていたのか？__

--------

## 今まで Rust は何をやっていたのか？

--------

### 2018


* 2018-06-23 - __Rust 2018: an early preview__ - https://internals.rust-lang.org/t/rust-2018-an-early-preview/7776/14
* 2018-06-21 - Rust Version 1.27.0
* 2018-06-19 - Yank futures 0.2? - https://github.com/rust-lang-nursery/futures-rs/issues/1039
* 2018-06-19 - Rust Networking with Carl Lerche - https://softwareengineeringdaily.com/2018/06/19/rust-networking-with-carl-lerche/
* 2018-06-19 - Introducing Tsukuyomi - https://medium.com/@ubnt_intrepid/introducing-tsukuyomi-3123fd6b913d
* 2018-05-19 - __Tracking issue for Pin APIs (RFC 2349)__ - https://github.com/rust-lang/rust/issues/49150
* 2018-05-10 - Rust Version 1.26.0
* 2018-05-15 - A revised single-trait approach - https://github.com/rust-lang/rfcs/pull/2418#issuecomment-388939069
* 2018-05-07 - Design goals and constraints - https://github.com/rust-lang/rfcs/pull/2418#issuecomment-386964959
* 2018-05-03 - __The Rust Edition Guide__ - https://rust-lang-nursery.github.io/edition-guide/
* 2018-04-25 - RFC: add futures and task system to libcore - https://github.com/rust-lang/rfcs/pull/2418
* 2018-04-08 - __RFC: add futures to libcore__ - https://github.com/rust-lang/rfcs/pull/2395
* 2018-04-07 - __futures 0.2.0__ - https://github.com/rust-lang-nursery/futures-rs/releases/tag/0.2.0
* 2018-04-07 - async/await notation for ergonomic asynchronous IO - https://github.com/rust-lang/rfcs/pull/2394
* 2018-04-06 - Async & Await in Rust: a full proposal - https://boats.gitlab.io/blog/post/2018-04-06-async-await-final/
* 2018-03-30 - Feature Name: async_await - https://github.com/rust-lang/rfcs/blob/master/text/2394-async_await.md
* 2018-03-30 - ジェネレータと Pin/Unpin まとめ - https://qiita.com/ubnt_intrepid/items/df70da960b21b222d0ad
* 2018-03-20 - Async/Await VI: 6 weeks of great progress - https://boats.gitlab.io/blog/post/2018-03-20-async-vi/
* 2018-03-08 - __Announcing the Tokio runtime__ - https://tokio.rs/blog/2018-03-tokio-runtime/
* 2018-02-27 - Futures 0.2 is nearing release - https://aturon.github.io/2018/02/27/futures-0-2-RC/
* 2018-02-08 - __tokio 0.1.0__
* 2018-02-08 - Async/Await V: Getting back to the futures - https://boats.gitlab.io/blog/post/2018-02-08-async-v-getting-back-to-the-futures/
* 2018-02-07 - Async/Await IV: An Even Better Proposal - https://boats.gitlab.io/blog/post/2018-02-07-async-iv-an-even-better-proposal/
* 2018-01-31 - Rust creator Graydon Hoare is now at Apple working on Swift -  https://news.ycombinator.com/item?id=13533701
* 2018-01-30 - Async/Await III: Moving Forward with Something Shippable - https://boats.gitlab.io/blog/post/2018-01-30-async-iii-moving-forward/
* 2018-01-30 - Async/Await II: Narrowing the Scope of the Problem - https://boats.gitlab.io/blog/post/2018-01-30-async-ii-narrowing-the-scope/
* 2018-01-30 - Graydon Hoare 、手元に蟹本が届き、当時を振り返る - https://mobile.twitter.com/graydon_pub/status/958192076209897472
* 2018-01-27 - futures0.2 - https://github.com/rust-lang-nursery/futures-rfcs/blob/master/futures-02.md
* 2018-01-25 - Async/Await I: Self-Referential Structs - https://boats.gitlab.io/blog/post/2018-01-25-async-i-self-referential-structs/
* 2018-01-14 - I wonder, why Graydon Hoare, the author of Rust, stopped contributing into it and switched to Swift? - https://amp.reddit.com/r/rust/comments/7qels2/i_wonder_why_graydon_hoare_the_author_of_rust/
* 2018-01-10 - What Are Tokio and Async IO All About? - https://manishearth.github.io/blog/2018/01/10/whats-tokio-and-async-io-all-about/


--------

#### この頃の Rust

* tokio-core が tokio になった
* iron, gotham が開発停止
* async-await 構文が RFCS に入った

--------

### 2017


* 2017-12-18 - Tokio internals: Understanding Rust's asynchronous I/O framework from the bottom up - https://cafbit.com/post/tokio_internals/
* 2017-11-22 - Version 1.22.0
* 2017-08-18 - "What next?" - https://graydon2.dreamwidth.org/253769.html
* 2017-08-17 - Immovable generators - https://github.com/rust-lang/rust/pull/45337
* 2017-08-09 - __Announcing Gotham__ - https://gotham.rs/blog
* 2017-08-28 - __futures-await__ - https://github.com/alexcrichton/futures-await
* 2017-08-07 - https://www.reddit.com/r/rust/comments/74o8k9/what_is_the_basic_difference_between_mio_and_tokio/
* 2017-07-14 - __hyper 0.11.0__ - https://github.com/hyperium/hyper/releases/tag/v0.11.0
* 2017-07-08 - __eRFC: Experimentally add coroutines to Rust__ - https://github.com/rust-lang/rfcs/pull/2033
* 2017-02-09 - __mdo-future__ - https://crates.io/crates/mdo-future
* 2017-02-02 - Rust Version 1.15.0
* 2017-01-07 - mio で echo サーバメモ - http://agtn.hatenablog.com/entry/2017/01/07/151455


--------

#### この頃の Rust

* hyper が tokio_core + futures-rs に対応
* actix-web, Gotham の開発が始まる
* coroutines が nightly に入る
* mdo-future, futures-await がリリース

--------

#### この時代

* ES2017 に asnyc-await が入る

--------

### 2016

--------

* 2016-12-22 - Rust Version 1.14.0
* 2016-12-23 - Getting Started with Tokio - https://lukesteensen.com/2016/12/getting-started-with-tokio/
* 2016-12-07 - Rust futures at a glance - https://daiheitan.github.io/blog/2016/12/07/Rust-futures-at-a-glance/
* 2016-11-27 - Rustで非同期Thrift - http://keens.github.io/slide/RustdehidoukiThriftshitai/
* 2016-11-21 - Rustとコルーチンと非同期I/O - https://qiita.com/kubo39/items/cd9816e31adaed8e040f
* 2016-11-16 - How stable is the mio/futures/tokio ecosystem? - https://www.reddit.com/r/rust/comments/5d3b3n/how_stable_is_the_miofuturestokio_ecosystem/
* 2016-10-15 - Rustのネットワークラリーフレームワーク/フレームワーク - https://qiita.com/dai197x/items/abc207d602b0f3a5cff1
* 2016-09-16 - Rotor and Tokio - https://users.rust-lang.org/t/rotor-and-tokio/7322
* 2016-09-11 - The relationship between async libraries in Rust - https://www.jimmycuadra.com/posts/the-relationship-between-async-libraries-in-rust/
* 2016-09-10 - __tokio-core 0.1.0___
* 2016-09-07 - Designing futures for Rust - https://aturon.github.io/blog/2016/09/07/futures-design/
* 2016-08-14 - Futures in Rust - Writing an Async Web API Wrapper - An excercise in learning Rust - http://www.ishbir.com/post/2016-08-14-futures-in-rust/
* 2016-08-11 - Zero-cost futures in Rust in Hacker News - https://news.ycombinator.com/item?id=12268988
* 2016-08-10 - Getting started with futures in Redit - https://www.reddit.com/r/rust/comments/4x15mr/getting_started_with_futuresrs/
* 2016-08-06 - History of Rust - https://github.com/steveklabnik/history-of-rust/blob/gh-pages/index.md
* 2016-08-04 - __tokio_core__ - https://medium.com/@carllerche/announcing-tokio-df6bb4ddb34
* 2016-08-01 - __futures 0.1.0__ - https://crates.io/crates/futures/versions
* 2016-03-29 - __Coroutine VS Futures の議論始まる__ - https://github.com/rust-lang/rfcs/issues/1081#issuecomment-202668696
* 2016-02-29 - this week in Rust - https://this-week-in-rust.org/blog/2016/02/29/this-week-in-rust-120/
* 2016-02-24 - Proposal: Unify Sockets, Timers, and Channels - https://github.com/carllerche/mio/issues/360
* 2016-01-21 - Rust Version 1.6.0
* 2016-01-03 - Async IO in Rust (part III) - https://medium.com/@paulcolomiets/async-io-in-rust-part-iii-cbfd10f17203


#### この頃の Rust

* ライブラリレベルのコンテキストスイッチは危険だとの議論
* mio ベースのゼロコスト futures-rs と tokio-core の開発が始まる
* rotor, mioco などのそれ以前の非同期IOライブラリが開発終了

--------

#### この時代

* Raspberry Pi 3

--------

### 2015


* 2015-12-10 - Rust Version 1.5.0
* 2015-11-13 - Async IO for Rust (part II) - https://medium.com/@paulcolomiets/async-io-for-rust-part-ii-33b9a7274e67
* 2015-10-04 - __coio-rs__ - https://users.rust-lang.org/t/coroutines-for-rust/3135
* 2015-09-16 - Asynchronous IO in Rust in Redit - https://www.reddit.com/r/rust/comments/3l5rvd/asynchronous_io_in_rust/
* 2015-09-15 - Asynchronous IO in Rust - https://blog.skcript.com/asynchronous-io-in-rust-36b623e7b965
* 2015-08-18 - __rotor__ 0.1.0 - https://crates.io/crates/rotor/versions
* 2015-08-16 - Rust Web Framework Iron - https://news.ycombinator.com/item?id=10070312
* 2015-08-09 - __context-rs__ 0.1.0
* 2015-07-26 - Introducing __mioco__: MIO COroutines - async io made easy - https://www.reddit.com/r/rust/comments/3em7m7/introducing_mioco_mio_coroutines_async_io_made/
* 2015-07-20 - rustでtelnetチャットを書いた - http://gifnksm.hatenablog.jp/entry/2015/07/20/232334
* 2015-07-12 - My Basic Understanding of mio and Asynchronous IO - https://hermanradtke.com/2015/07/12/my-basic-understanding-of-mio-and-async-io.html
* 2015-07-10 - Rust in Detail: Writing Scalable Chat Service from Scratch - https://nbaksalyar.github.io/2015/07/10/writing-chat-in-rust.html
* 2015-05-23 - IronでHTTPサーバーを立てる - https://qiita.com/rejasupotaro/items/c1b66c2e7eb2d76b799e
* 2015-05-15 - __Rust Version 1.0.0__
* 2015-04-21 - Async IO - https://github.com/rust-lang/rfcs/issues/1081
* 2015-04-13 - __nickel.rs__ - https://github.com/nickel-org/nickel.rs/releases/tag/0.2.0
* 2015-03-03 - Getting Acquainted with MIO - https://hoverbear.org/2015/03/03/getting-acquainted-with-mio/
* 2015-02-11 - __mdo__ - https://crates.io/crates/mdo
* 2015-01-11 - __hyper__ - https://github.com/hyperium/hyper/releases/tag/v0.1.0


--------

この頃の Rust

* Rust 1.0 が出た
* hyper や iron、 nickel が利用されはじめる
* コンテキストスイッチライブラリ context-rs と mio を使った rotor, mioco, coio-rs などが試みられる

--------

#### この時代

* WebAssembly, AlphaGo, ResNet, R-CNN, Raspberry Pi 2, Raspberry Pi Zero, TypeScript(async-await)

--------

### 2014

--------

* 2014-10-09 - __Rust Version 0.12.0__
* 2014-09-30 - mio のドキュメント - https://legacy.gitbook.com/book/wycats/mio-book/details
* 2014-09-17 - Remove libgreen and runtime abstractions  - https://github.com/rust-lang/rust/issues/17325
* 2014-09-16 - https://github.com/rust-lang/meeting-minutes/blob/master/weekly-meetings/2014-09-16.md#removing-io-runtime-abstractions
* 2014-09-10 - __ランタイムを削除__ - https://github.com/aturon/rfcs/blob/remove-runtime/active/0000-remove-runtime.md
* 2014-09-10 - RFC: Remove runtime system, and move libgreen into an external library - https://github.com/rust-lang/rfcs/pull/230
* 2014-09-04 - __mio の開発開始__ - https://github.com/rust-lang/rfcs/pull/219#issuecomment-54336582
* 2014-08-30 - RFC: IO simplification - https://github.com/rust-lang/rfcs/pull/219
* 2014-07-02 - __Iron 登場__ - http://ironframework.io
* 2014-07-02 - Graydon が Swiftに気持ちを表明する - https://graydon2.dreamwidth.org/5785.html
* 2014-01-09 - Rust Version 0.9


#### この頃の Rust

* グリーンスレッドからネイティブスレッドモデルへと変更された
* グリーンスレッドやランタイムは削除された
* mio 開発開始
* rusti インタプリタが消えた

--------

#### この時代

* Swift, Flux, asyncio(Python3.4), asm.js, Emscripten, AWS Lambda, Kubernetes

--------

### 2013

--------


* 2013-09-26 - Rust Version 0.8
* 2013-12-04 - 5分で分かったふりができるRust紹介 - https://gist.github.com/voluntas/a75d38f27982ee0ae28e7a4510fec89b
* 2013-08-31 - __Graydon が Rust のテクニカルリーダーを辞める__ - https://www.reddit.com/r/rust/comments/1lfoaz/graydon_hoare_steps_down_as_technical_lead_on_rust/
* 2013-08-30 - [rust-dev] changing roles - https://mail.mozilla.org/pipermail/rust-dev/2013-August/005426.html
* 2013-07-15 - Rust 基礎文法最速マスター (rust 0.7 編) - http://gifnksm.hatenablog.jp/entry/2013/07/15/170736
* 2013-07-12 - A `yield` construct in the vein of C# - https://github.com/rust-lang/rust/issues/7746
* 2013-05-31 - Need a solution for select / async events - https://github.com/rust-lang/rust/issues/6842
* 2013-04-03 - Rust Version 0.6
* 2013-02-11 - Write a parallel deque for work stealing - https://github.com/rust-lang/rust/issues/4877
* 2013-01-11 - Scheduler rewrite with I/O event loop - https://github.com/rust-lang/rust/issues/4419


#### この頃の Rust

* __Graydon が Mozilla を辞める__
* Rust コミュニティが C++er, Script言語er, 関数型言語er に別れ始める

--------

#### この時代

*  ES6Promise, Raspberry Pi, Docker

--------

### 2012


* 2012-12-22 - Redesign the I/O library and traits - https://github.com/rust-lang/rust/issues/4248
* 2012-12-21 - Rust Version 0.5
* 2012-08-03 - Scheduler work stealing - https://github.com/rust-lang/rust/issues/3095
* 2012-08-03 - Graydon へのインタビュー - https://www.infoq.com/jp/news/2012/08/Interview-Rust
* 2012-07-02 - 並行言語 Rust - https://altenwald.org/2012/07/02/rust-otro-mas-para-concurrencia/
* 2012-02-08 - __Servo の開発がスタート__
* 2012-01-20 - __Rust Version 0.1__ - https://github.com/steveklabnik/history-of-rust/blob/gh-pages/index.md


-------

#### この頃の Rust

* インタプリタ、GC
* libuv で作られたランタイム
* グリーンスレッド、コルーチン、 work stealing スケジューラ
* チャンネル、アクター
* __Servo の開発がスタート__

--------

#### この時代

* Elixir, TypeScript, Unity 4

--------

### 2011


* 2011-04-01 - Future Tense - https://www.slideshare.net/BrendanEich/future-tense-7782010


--------

#### この頃の Rust

* Brendan Eich(JSの開発者)のServoの紹介スライドに出てくる程度
* 空白の時代

--------

#### この時代

* JAVA nio(jdk1.7), Kotlin, React, jQuery Deferred, Boost Context はてなブログ

--------

### 2006~2010

* 2010-11-29 - __Graydon の個人プロジェクトが Mozzila のプロジェクトになる__
* 2006: __Graydon Hoare が Rust を開発開始__

--------

* 2010-11-29 - __Graydon の個人プロジェクトが Mozzila のプロジェクトになる__ - https://jaxenter.com/mozillas-graydon-hoare-working-on-rust-102672.html
* 2006: __Graydon Hoare が Rust を開発開始__


--------

#### この時代

* 2010: async-await(C#5.0), NaCL
* 2009: golang, CoffeeScript, iPhone 3GS, RX(C#5.0), Android 1.6
* 2008: epoll, Boost ASIO , Task(C#4.0), node.js, Google Chrome, Github
* 2007: libev, Clojure, LINQ(C#3.0), D言語 ver2, Clang, Silverlight
* 2006: Firefox 2.0, jQuery1.0, Twitter, AWS

C10K 問題への対応として epoll システムコールを使った非同期並列 IO が普及しはじめる。
iPhone の発明

--------

### 2000~2005

--------

* 2005: gevent(eventlet), GoogleMap, F#
* 2004: nginx(AIO), Rails, GMail, Facebook
* 2003: libaio(Linux AIO, Linux2.6), Scala, LLVM
* 2002: C10K 問題, Firefox
* 2001: POSIX AIO(POSIX 1003.1b), IE6
* 2000: libevent(poll|select)

インターネットの普及で C10K が提唱されはじめる

--------

### 1990 年代

--------

* 1999: IE5
* 1998: Netscape Navigator 4, Google
* 1997: IE4
* 1996: Shockwave Flash, Concurrent Haskell, OCaml
* 1995: Apache, JavaScript, Ruby, Haskell 1.3(monad)
* 1994: PHP, Java, python1, Perl5, Common Lisp, Amazon
* 1991: Concurrent ML, Linux
* 1990: Haskell 1.0, D言語, SML

インターネットが普及する

--------

### 1980 年代

--------

* 1986: Erlang
* 1983: C++

言語実装が始まる

--------

### 1970 年代

* 1978: CSP
* 1977: Future(promise)
* 1973: Actor
* 1972: Prolog, C言語
* 1971: Research Unix V1

基礎理論ができる


## 今まで Rust は何をやっていたのか？

--------

### 2013 年以前

非同期IOのランタイムサポートがあったが、 Rust 1.0 で消えた

--------

### 2015 年

mio + coroutine の非同期　IO　があったが、 futures + tokio が出て消えた

--------

### 2017 年

 tokio-core が主流になったが、 tokio runtime + futures0.3 で混沌としている

--------

## 所感

* 環境変わりすぎ
* ライブラリが根付かない
* 賽の河原
* [Graydon が苦言を呈す](https://internals.rust-lang.org/t/rust-2018-an-early-preview/7776/14)


## あわせて読みたい

* [非同期 IO について] (https://qiita.com/DUxCA/items/1f3b1bd51e206ffdd2a6)
* [Rust で学ぶ epoll](https://qiita.com/DUxCA/items/7b16ab18d66485ace1c6)
* [Python の generator で asyncio.Future Monad と do 記法を実装する](https://qiita.com/DUxCA/items/0a4a13371c2b54d3fbe1)
* [JavaScript + generator で Maybe、 Either、 Promise、 継続モナドと do 構文を実装し async-await と比べてみる](https://qiita.com/DUxCA/items/0582e71f4e6984548933)
* [Boost.ASIO で callback | coroutine | future による非同期IO](https://qiita.com/DUxCA/items/3365b25eea13c0f2bb51)
