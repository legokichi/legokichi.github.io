この記事は [Rust Advent Calendar 2018](https://qiita.com/advent-calendar/2018/rust) の 12 + n 日目の記事です。
この記事は [Rust と非同期 IO の歴史(資料編)](https://qiita.com/legokichi/items/82140896c67da3f87a42) の続報です。

-------


あけましておめでとうございます。

2018-06 からの非同期 Rust の進捗をまとめました。


今年もよろしくおねがいします。

## 忙しい人のための Q&A

### Q. 2018-12 現在 いますぐ非同期 Rust を書きたい

#### A1. HTTP サーバが立てたいなら

* tokio 0.1 + futures 0.1 + hyper 0.12 + hyper-tls or hyper-native-tls
* actix + futures 0.1 + actix-web 0.7 

#### A2. HTTP リクエストしたいなら

* tokio 0.1 + futures 0.1 + hyper 0.12 + hyper-tls or hyper-native-tls
* tokio 0.1 + reqwest 0.9
* actix + futures 0.1 + actix-web 0.7 


### Q. 2018-12 現在 いますぐ async-await を試したい

A. rust nightly + futures 0.3 + romio or tokio-async-await

### Q. いつ async-await は stable で使えるようになるの？

A. https://areweasyncyet.rs/

### Q. tokio や mio 、 futures 、 async-await について学びたい

A. __太文字__ の記事をよく読む

### Q. async-await 、 futures 、 tokio 、 mio 、 tide 、 tower 、 tower-http 、tower-web 、 actix 、 actix-web って何がどう違うの？

A. このページで気になる単語をページ検索してリンク先をよく読む

# 2018-06 から 2019-02 までの時系列

## 2019-01

* 2019-01-11 - Denoを読む(1) - https://blog.bokuweb.me/entry/2019/01/11/102706
* 2019-01-10 - Create `tokio-test` for tokio testing utilities - https://github.com/tokio-rs/tokio/issues/841
    * tokio のテストフレームワークが提案された
* 2019-01-09 - reqwest 0.9.6 - https://github.com/seanmonstar/reqwest/releases/tag/v0.9.6
    * ついに reqwest で rustls が使えるようになった
* 2019-01-07 - __The Waker API I: what does a waker do?__ - https://boats.gitlab.io/blog/post/wakers-i/
    * __futures のしくみ解説記事__
* 2019-01-04 - Introduce `tokio-trace` #827 - https://github.com/tokio-rs/tokio/pull/827 - https://github.com/tokio-rs/tokio-trace-nursery
    * 非同期ログを取るための tokio-trace が提案された


## 2018-12

* 2018-12-29 - __areweasyncyet.rs__ - Are we async yet? - https://github.com/rust-lang-nursery/wg-net/issues/86 - https://github.com/upsuper/areweasyncyet.rs - https://areweasyncyet.rs/ - https://www.reddit.com/r/rust/comments/ac4sje/are_we_async_yet/
    * __非同期 Rust の状況がひと目で分かるサイト__
* 2018-11-25 - Rust: Converting AsyncRead and AsyncWrite to Futures, Sinks and Streams - https://jsdw.me/posts/rust-futures-tokio/
* 2018-12-20 - actix_web は Actorモデルでどのようにwebリクエストを捌いているのか - https://x1.inkenkun.com/archives/5890
* 2018-12-19 - A great 2018, an even better 2019 - https://tokio.rs/blog/2018-12-recap-2018/
    * tokio-trace と tokio-async-await の発表
* 2018-12-18 - Pin stabilization - https://github.com/rust-lang/rust/pull/56939
* 2018-12-14 - __async book__ - https://rust-lang.github.io/async-book/ - https://github.com/rust-lang/async-book
    * __非同期 rust 公式ガイドブック__
    * これのリバイバルっぽい → Async in Rust: what you need to know - https://aturon.github.io/apr/ - https://github.com/aturon/apr
* 2018-12-13 - Rustでサーバレスやりたい人が一番最初に見る場所 - https://qiita.com/shunp/items/8b11450155266d2fcfc1
* 2018-12-13 - http-service 0.1.1 - https://crates.io/crates/http_service
* 2018-12-13 - __wg-net: Async in Rust, circa 2018__ - https://rust-lang-nursery.github.io/wg-net/2018/12/13/async-update.html
    * 非同期 rust の状況報告
    * Romio, http-service, tiger なるクレートが発表された
    * tide を推していくようだ
* 2018-12-12 - Rustで書かれたVMM firecrackerを読もう！(1) - https://tomo-wait-for-it-yuki.hatenablog.com/entry/2018/12/12/214332
* 2018-12-12 - Rust における wasm-bindgen と wasm-pack と cargo-web と stdweb の違い - https://qiita.com/legokichi/items/5d6344314ab6d6633554
    * JS の Promise と WASM Rust の Future が相互運用可能になりつつある
* 2018-12-10 - Questions about lifetime errors on impl trait  - https://users.rust-lang.org/t/questions-about-lifetime-errors-on-impl-trait/23064
* 2018-12-09 - __Inside Rust's Async Transform__ - https://blag.nemo157.com/2018/12/09/inside-rusts-async-transform.html
    * __async-await が具体的にどのようにコルーチン｜ジェネレータを使ったコードに変換されるかの解説__
* 2018-12-07 - __demo for rust asynchronous io: from mio to stackless coroutine__ - https://github.com/Hexilee/async-io-demo
    * __async Rust のしくみを mio から romio まで丁寧に解説__
* 2018-12-05 - __Wherefore art thou Romio?__ - https://boats.gitlab.io/blog/post/romio/ - https://github.com/withoutboats/romio
    * tokio の fork。 futures-0.3 + async-await が試せる。toio は futures-0.1 ベースのままで futures-0.3 が試しづらいので。

## 2018-11

* 2018-11-29 - __Rust Runtime for AWS Lambda__ - https://aws.amazon.com/jp/blogs/opensource/rust-runtime-for-aws-lambda/
* 2018-11-28 - __net-wg: Rust Web Survey 2018__ - https://rust-lang-nursery.github.io/wg-net/2018/11/28/wg-net-survey.html
    * Rocket と actix-web の２強。ただし rocket は未だに非同期対応していない - https://github.com/SergioBenitez/Rocket/issues/17#issuecomment-269017203)
* 2018-11-28 - Serverless HTTP: Throw down your main! Rustlang Serverless HTTP applications won’t need them where they’re going - https://medium.com/@softprops/serverless-http-9a58f9b2df60
* 2018-11-27 - Tide's evolving middleware approach - https://rust-lang-nursery.github.io/wg-net/2018/11/27/tide-middleware-evolution.html
* 2018-11-27 - __Announcing the Firecracker Open Source Technology: Secure and Fast microVM for Serverless Computing__ - https://aws.amazon.com/jp/blogs/opensource/firecracker-open-source-secure-fast-microvm-serverless/
* 2018-11-26 - サーバーレスRust〜AWS LambdaとRustのマリアージュ〜 - https://speakerdeck.com/golddranks/sabaresurust-aws-lambdatorustfalsemariaziyu
* 2018-11-26 - RustのLT会！ Rust入門者の集い #6 - https://rust.connpass.com/event/105541/
* 2018-11-21 - https://twitter.com/qnighy/status/1064901297080459265 <br /><blockquote>
そうです。async/awaitではasync関数やasyncブロック自体はただFutureを組み立てるだけの人になるのでその辺りの混乱がなくなるので、async/awaitをはじめるといいです
</blockquote>
* 2018-11-19 - https://twitter.com/qnighy/status/1064527058091442179 <br /><blockquote>
Rust非同期プログラミングで使われるFutureという仕組みは、async/awaitで組み立てるので非透過的であり、透過的に非同期実行されるgoroutineとは使い勝手が結構違う。それとは別の視点からの違いとして、Futureは呼び出し元にキャンセル権があり、awaitの先が実行されない可能性がある。
そのため、呼び出される側の非同期関数で明示しなくても、「この非同期関数を実行して、3秒以内に終わらなかったらキャンセル」みたいなことができる。
逆に言うと、各種非同期プリミティブはキャンセルされる可能性を考慮して設計しないといけなくなる。
</blockquote>
* 2018-11-11 - Rustでfutures用Mutexを自作してみる (シングルスレッド編) - https://qiita.com/qnighy/items/81f060853b9debe2085f
* 2018-11-11 - New RFC to stabilize the `Future` API in Rust. Next stop: stable async/await! - https://twitter.com/aaron_turon/status/1061300546445860865 
* 2018-11-11 - RFC: stabilize `std::task` and `std::future::Future` - https://github.com/rust-lang/rfcs/pull/2592
* 2018-11-10 - __tokio-serde-json 0.2.0__ -  https://github.com/carllerche/tokio-serde-json
* 2018-11-10 - __tokio-serde 0.3.0__ - https://github.com/carllerche/tokio-serde
    * serde の非同期アダプタ。 非同期 Stream に対してパースできるようになった
* 2018-11-10 - Monadic do notation in Rust: Part I - https://varkor.github.io/blog/2018/11/10/monadic-do-notation-in-rust-part-i.html
    * なぜモナドではなくコルーチンによる async-await を採用するのかという解説記事
* 2018-11-08 - Making progress in await syntax - https://boats.gitlab.io/blog/post/await-syntax/
    * 2018-11-09 - https://mobile.twitter.com/qnighy/status/1060879019619405826 <br /><blockquote>
boats氏のブログにてasync/awaitの構文について触れられている。現在は暫定的にawait!というマクロとして扱われているが、本当は専用の構文にしたい。ただawaitと?演算子の優先順位は難しい判断が求められる。まずは両方に前方互換な構文だけを許すのがいいだろうという結論
</blockquote>
* 2018-11-08 - [Stabilization] Pin APIs - https://github.com/rust-lang/rust/issues/55766
    * 2018-11-08 - https://twitter.com/qnighy/status/1060342302877470720 <br /><blockquote>
Pin APIの安定化が提案された。普段だとこんなに速くないと思うので、やはりasync/awaitの整備が急がれているという感じがある。Pin API、特にUnpinの意味は、やはり難しいようだ。それなりにRustに慣れた人でもその内容を理解するのに苦戦しているっぽい。</blockquote>
* 2018-11-07 - Middleware in Tide - https://rust-lang-nursery.github.io/wg-net/2018/11/07/tide-middleware.html
* 2018-11-01 - __Rust速習会(3) Webサーバ__ - https://speakerdeck.com/qnighy/rustsu-xi-hui-3
    * __非同期 Rust 入門 日本語オススメ記事__



# 2018-10
* 2018-10-29 - Finding and fixing memory leaks in a Hyper application - https://blog.1aim.com/2018/10/finding-and-fixing-memory-leaks-in-a-hyper-application-or-how-i-learned-to-stop-worrying-and-love-the-allocator/
* 2018-10-29 - Announcing Gotham 0.3 - https://gotham.rs/blog/release/2018/10/29/gotham-0.3.html
* 2018-10-27 - [Rust] [tokio]Futureの実装とasync/await - https://qiita.com/maueki/items/b40deb303eb85940ce70
* 2018-10-26 - __Rust製の分散オブジェクトストレージをOSSとして公開しました__ - https://dwango.github.io/articles/frugalos/
    * 非同期 Rust が確立されていなかったころの苦労話は必読
* 2018-10-26 - Actix Web Async/Await Preview - https://github.com/mehcode/actix-web-async-await
* 2018-10-20 - https://twitter.com/qnighy/status/1053546425273872389 </br><blockquote>
Rustの非同期について。まずfutures-0.3, tokio, pin, async/awaitという組み合わせになっていくのはほぼ既定路線だと思う。ただ既存の非同期ライブラリはfutures-0.1で作ってしまったものが結構あるので(tokio自身もそう)、今非同期をやろうとするとそこの付き合い方が難しい。
で、このあたりが未成熟で飛び込みづらいのをライブラリ開発者もわかっていて、特にDBではdieselやr2d2などのライブラリがまだ非同期対応を開始していない。(でもそろそろfutures-0.3を前提に非同期対応を始めても問題ない気はする)
あとはWebフレームワークの覇権が決まってほしい。Actix-Webが暫定で最強だと思っているけど、Tower-Webとかにも期待したい。
</blockquote>
* 2018-10-17 - futures-0.3.0-alpha.8 - https://github.com/rust-lang-nursery/futures-rs/releases/tag/0.3.0-alpha.8
    * futures の 0.3 と 0.1 のアダプタが実装された
* 2018-10-05 - __Futureとその周辺 情報科学若手の会 #51__ - https://keens.github.io/slide/futuretosonoshuuhensa_bei/
    * __非同期 Rust の futures 、コルーチン入門スライド__

# 2018-09

* 2018-09-29 - Tower Web 0.3 — async/await and template support - https://medium.com/@carllerche/tower-web-0-3-async-await-and-template-support-e0bb8ed47941
* 2018-09-26 - AUTH WEB MICROSERVICE WITH RUST USING ACTIX-WEB - COMPLETE TUTORIAL PART 3 - https://gill.net.in/posts/auth-microservice-rust-actix-web-diesel-complete-tutorial-part-3/
* 2018-09-22 - Rust 勉強メモ: futures (LocalPoolとThreadPool) - https://qiita.com/S-YOU/items/07dbf8575cd9efcae30c
* 2018-09-20 - tokio-trace-prototype - https://github.com/hawkw/tokio-trace-prototype
* 2018-09-20 - Implementing SCTP to support WebRTC data channels in pure Rust -https://cafbit.com/post/rust_webrtc_data_channels/
* 2018-09-13 - The Networking Working Group Newsletter - 01 - https://internals.rust-lang.org/t/the-networking-working-group-newsletter-01/8391
* 2018-09-12 - actix-net 0.1.0 - https://github.com/actix/actix-net
* 2018-09-11 - __Rising Tide: building a modular web framework in the open__ - https://rust-lang-nursery.github.io/wg-net/2018/09/11/tide.html
    * __乱立するWebフレームワーク間の相互運用性を改善する Tide プロジェクトの解説__
* 2018-09-09 - Tower Web — Expanding the middleware stack - https://medium.com/@carllerche/tower-web-expanding-the-middleware-stack-f9bf55bfa109
* 2018-09-04 - Adventures in Rust: Futures and Tokio - http://bryangilbert.com/post/code/rust/adventures-futures-tokio-rust/
* 2018-09-06 - Net Web WG Meeting #1 - https://github.com/rust-lang-nursery/wg-net/blob/master/meetings/2018-09-06-net-web-wg.md
* 2018-09-02 - isucon7予選のアプリをRustに移植したから解説するね - https://keens.github.io/blog/2018/09/02/isucon7yosennoapuriworustniishokushitakarakaisetsusurune/

# 2018-08

* 2018-08-31 - wg-net: Net WG Lead Meeting #1 - https://github.com/rust-lang-nursery/wg-net/blob/master/meetings/2018-08-31-net-wg-leads.md
* 2018-08-29 - Port FlatBuffers to Rust  - https://github.com/google/flatbuffers/pull/4898
* 2018-08-28 - AUTH WEB MICROSERVICE WITH RUST USING ACTIX-WEB - COMPLETE TUTORIAL PART 2 - https://gill.net.in/posts/auth-microservice-rust-actix-web-diesel-complete-tutorial-part-2/
* 2018-08-28 - Tokio async/await preview - https://github.com/tokio-rs/tokio/tree/master/tokio-async-await
* 2018-08-28 - Feasible functors in Rust - https://varkor.github.io/blog/2018/08/28/feasible-functors-in-rust.html
    * なぜモナドではなくコルーチンを使った async-await なのかという問題提起に対する考察
* 2018-08-27 - Experimental async / await support for Tokio - https://tokio.rs/blog/2018-08-async-await/ - https://www.reddit.com/r/rust/comments/9as61i/tokio_experimental_async_await_support/
* 2018-08-24 - Tokio 0.1.8 with many incremental improvements - https://tokio.rs/blog/2018-08-incremental-improvements/
* 2018-08-22 - Another look at the pinning API - https://boats.gitlab.io/blog/post/rethinking-pin/
* 2018-08-19 - Rust / yew で 仮想DOMなウェブアプリケーションを作ってみた - https://qiita.com/mizchi/items/30e32dccf36a0610daef
* 2018-08-18 - AUTH WEB MICROSERVICE WITH RUST USING ACTIX-WEB - COMPLETE TUTORIAL PART 1 - https://gill.net.in/posts/auth-microservice-rust-actix-web-diesel-complete-tutorial-part-1/
* 2018-08-17 - Test utility for mocking a Tokio task - https://github.com/carllerche/tokio-mock-task
* 2018-08-17 - toykio (Fahrenheit) - https://rust-lang-nursery.github.io/futures-rs/blog/2018/08/17/toykio.html
    * fahrenheit (formerly known as toykio) - https://github.com/polachok/fahrenheit
* 2018-08-16 - Rust GraphQL webserver with Warp, Juniper and MongoDB - http://alex.amiran.it/post/2018-08-16-rust-graphql-webserver-with-warp-juniper-and-mongodb.html
* 2018-08-15 - __Futures 0.3.0-alpha.3__ - https://rust-lang-nursery.github.io/futures-rs/blog/2018/08/15/futures-0.3.0-alpha.3.html
* 2018-08-15 - Tower Web 0.2 — Now 100% comment attribute free. - https://medium.com/@carllerche/tower-web-0-2-now-100-comment-attribute-free-3ed0633e47e5
* 2018-08-10 - Call For Example Web Projects - https://github.com/rust-lang-nursery/wg-net/issues/44
* 2018-08-10 - __Tower Web — A new web framework for Rust__ - https://medium.com/@carllerche/tower-web-a-new-web-framework-for-rust-e2912856851b
    * __非同期リクエストレスポンスモデルを抽象化するクレート tower を使った web フレームワークの紹介記事__
        * tower 自体は プラガブルな http クライアントを実現するクレート - https://github.com/hyperium/http/issues/288
    * http なら tower-http, gRPC なら towor-grpc など
* 2018-08-09 - __MONADS AND RUST A THREAD - or - why async/await instead of do notation__ - https://twitter.com/withoutboats/status/1027702531361857536
    * なぜモナドではなくコルーチンを使った async-await なのかという問題提起
* 2018-08-09 - New crate: pin-cell - https://boats.gitlab.io/blog/post/pin-cell/
* 2018-08-09 - __The WG-Net vision for Rust 2018__ - https://rust-lang-nursery.github.io/wg-net/2018/08/09/going-live.html
    * Async Foundations - https://rust-lang-nursery.github.io/wg-net/async-foundations/
        * futures-0.3 と async-await を開発するグループ
    * Embedded Foundations - https://rust-lang-nursery.github.io/wg-net/embedded-foundations/
        * 組み込み向けの非同期 IO を開発するグループ
    * Web Foundations - https://rust-lang-nursery.github.io/wg-net/web-foundations/
        * Web フレームワークの解説や Tide を開発するグループ
* 2018-08-07 - __embrio-rs__ - https://github.com/Nemo157/embrio-rs
    * __no_std 環境での非同期 IO クレート__
* 2018-08-05 - Async Iron with Hyper tokio using a cpupool. - https://github.com/iron/iron/pull/523
* 2018-08-03 - https://medium.com/@saschagrunert/lessons-learned-on-writing-web-applications-completely-in-rust-2080d0990287
* 2018-08-03 - wg-net: meeting - https://github.com/rust-lang-nursery/wg-net/blob/master/meetings/2018-08-03.md
* 2018-08-02 - https://twitter.com/qnighy/status/1025336774841663488 - https://github.com/rust-lang/rust/issues/49150#issuecomment-409939585 </br><blockquote>
Pin APIはimmovable typeを擬似的に、しかもコンパイラを大きく変更せずに実現する仕組みである。Pin APIはasync fnのために試験的に実装されていたが、boats氏とRalf Jung氏がこのたび、本APIの安定化を目指す意向を決めた。
</blockquote>
* 2018-08-01 - Rust でクロスコンパイルして Raspberry Pi Zero W で動かす - https://qiita.com/legokichi/items/b78de0df844e63e77a12
* 2018-08-01 - Rust LT #2 〜いま使う！Rust〜 - https://rust.connpass.com/event/91177/
* 2018-08-01 - __warp__ - https://seanmonstar.com/post/176530511587/warp - https://users.rust-lang.org/t/announcing-warp-a-new-web-framework/19280 - https://www.reddit.com/r/rust/comments/93tcxs/warp_a_new_web_framework_for_rust/ - https://github.com/seanmonstar/warp


## 2018-07

* 2018-07-31 - __Rust-Cookbook: Web Programming__ https://rust-lang-nursery.github.io/rust-cookbook/web.html
* 2018-07-30 - __Futures 0.3.0-alpha.2__ - https://rust-lang-nursery.github.io/futures-rs/blog/2018/07/30/futures-0.3.0-alpha.2.html
* 2018-07-27 - wg-net: Kickoff meeting - https://github.com/rust-lang-nursery/wg-net/blob/master/meetings/2018-07-27.md
* 2018-07-26 - Artifact 2.0 Rust Full Stack Web And Cli Application - https://vitiral.github.io/2018/07/16/artifact-2.0-rust-full-stack-web-and-cli-application.html
* 2018-07-23 - tokio-uds で docker.sock を叩く - https://qiita.com/legokichi/items/736495610b23350bf826
* 2018-07-22 - A static web app in Rust - https://bluejekyll.github.io/blog/rust/2018/07/22/static-web-app-rust.html
* 2018-07-22 - https://twitter.com/qnighy/status/1020935950585643009 <br /><blockquote>
  今人間をやめたい人にオススメなのがfutures-0.1/tokioで、これでちゃんとした非同期I/Oを書こうとすると「async/awaitがない」「所有権を複製できない」「参照もとれない」の3重苦でコンパイラの奴隷になった気分を味わえます。 年内にはasync/awaitが入って人権が回復されると思うので今のうちです今人間をやめた async/awaitがない→いわゆるコールバック地獄みたいになる 所有権を複製できない→他言語のコールバック地獄に一味足している 参照もとれない→Rustらしい書き方もかなり制約を受ける 一方で型はちゃんとしているので、コンパイラがOKと言うまであれこれコールバックを調整する作業ゲーになる ちなみにfutures-awaitを使うと1だけ取り除かれます。 (1と3を解決するには組み込みのasync/awaitを待つ必要がある。3ができれば2の苦しみはかなり緩和されると思う)
</blockquote>
* 2018-07-19 - __Futures 0.3.0-alpha.1__ - https://rust-lang-nursery.github.io/futures-rs/blog/2018/07/19/futures-0.3.0-alpha.1.html
* 2018-07-19 - Futures-rs: New Website - https://rust-lang-nursery.github.io/futures-rs/blog/2018/07/19/new-website.html
* 2018-07-18 - Rebooting the network services working group - https://internals.rust-lang.org/t/rebooting-the-network-services-working-group/8036
* 2018-07-17 - async iron - https://github.com/iron/iron/pull/523#event-1732656179
* 2018-07-16 - Tokio と Future のチュートリアルとかのまとめ+α - https://raskr.hatenablog.com/entry/2018/07/16/214420
* 2018-07-15 - Tsukuyomi + Juniper で GraphQL サーバを作る - https://qiita.com/ubnt_intrepid/items/e6b979a500655a3d7666
* 2018-07-14 - tokio の work steal algorithm への質問 - https://github.com/tokio-rs/tokio/issues/424#issuecomment-404981619
* 2018-07-10 - Rust + actix-web in the on of the biggest music festival Atlas Weekend - https://www.reddit.com/r/rust/comments/8xdsx5/rust_actixweb_in_the_on_of_the_biggest_music/
* 2018-07-09 - Rust RocketでのWebAPIサーバーの書き方を解説してみる - https://qiita.com/yukinarit/items/c5128e67d168b4f39983
* 2018-07-08 - webapp.rs - https://medium.com/@saschagrunert/a-web-application-completely-in-rust-6f6bdb6c4471 - https://github.com/saschagrunert/webapp.rs
    * emscripten, stdweb, yew を使ったフロントエンド Rust のデモ
* 2018-07-02 - rust の非同期 IO ライブラリ tokio-core と tokio runtime と actix 0.5 と actix 0.6 でタイマーの比較 - https://qiita.com/legokichi/items/8ab8693e3ae3078fab24

## 2018-06

* 2018-06-30 - https://twitter.com/qnighy/status/1012987599529472001 <br /><blockquote>
  futures-0.2がyankされたという話があったが、これは現在の安定版と区別するためにfutures-preview-0.2に改名されたということらしい。
  もともとFutureベースの非同期I/OライブラリtokioからI/Oに依存しない部分を抽出したのがfutures-0.1で、それを改良(crate分割、FutureのAPIを変更)したのがfutures-0.2だった。 現在のtokioエコシステムは基本的にfutures-0.1を中心に動いている。
  async/awaitサポートのためにfutures-0.2のコア部分を標準ライブラリに取り入れることになり、それにともないさらにAPIが変更されることになった。これによりfuturesは0.3になることになった。
  0.2にせよ0.3にせよ、現在のtokioエコシステムと互換性がないので、これが "futuresの最新バージョン" として出てくるのは良くない、というのが多分改名の理由。
</blockquote>
* 2018-06-27 - futures-0.2 が yank - https://users.rust-lang.org/t/futures-0-2-has-been-moved-to-futures-preview/18329 - https://github.com/rust-lang-nursery/futures-rs/issues/1039
* 2018-06-27 - Azure/iotedge 1.0.0 - https://github.com/Azure/iotedge/tree/master/edgelet - https://twitter.com/maxgortman/status/1012011425353461760
* 2018-06-27 - __Rust と非同期 IO の歴史(資料編)__ - https://qiita.com/legokichi/items/82140896c67da3f87a42
* 2018-06-26 - Rust と非同期 IO の歴史 - https://qiita.com/legokichi/items/882bcacd12870d087555
* 2018-06-26 - Rust LT #1 - https://rust.connpass.com/event/88656/
* 2018-06-23 - asynx/await - https://github.com/rust-lang/rust/pull/51580?w=1#issuecomment-399665846
* 2018-06-23 - Rust で学ぶ epoll - https://qiita.com/legokichi/items/7b16ab18d66485ace1c6
* 2018-06-19 - Why Rust's async functions should use the outer return type approach - https://github.com/MajorBreakfast/rust-blog/blob/master/posts/2018-06-19-outer-return-type-approach.md
* 2018-06-11 - Tokio の Blocking API を試す - https://qiita.com/legokichi/items/30e577d996851b6b3ba1
* 2018-06-11 - actix – an actor framework for the Rust programming language - https://simplabs.com/blog/2018/06/11/actix.html
* 2018-06-11 - Actix::From(Rocket) - https://noyez.gitlab.io/post/2018-06-11-rocket-to-actix/
* 2018-06-09 - Rust WebフレームワークのACTIXで使う安全なセッション管理機構を考えてみる - https://qiita.com/segfo/items/8d992320184fa0a41157
* 2018-06-07 - RustでHTTP/2+TLS対応なWebAPIの入口を作ってみる - https://qiita.com/segfo/items/8ce5af89a6daf95fdfc9
* 2018-06-04 - Async Methods II: object safety - https://boats.gitlab.io/blog/post/async-methods-ii/
* 2018-06-01 - hyper v0.12 - https://seanmonstar.com/

## 2018-06 以前

[Rust と非同期 IO の歴史(資料編)](https://qiita.com/legokichi/items/82140896c67da3f87a42)

## 後記

また何か発掘したら追記します
