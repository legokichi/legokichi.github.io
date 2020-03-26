* この記事は [Rust LT #1](https://rust.connpass.com/event/88656/) で発表したものです

## 自己紹介

某社でサーバサイド Rust 書いてる


--------

## 最近のサーバサイド Rust の話題

--------

* iron, ~~[gotham](https://gotham.rs/blog/2018/05/31/the-state-of-gotham.html)~~ が開発停止
* tokio-core が tokio runtime になった
* RFCS に async-await が入った
* futures 0.2 が yank ~~されそう~~ [された](https://crates.io/crates/futures/versions)

--------

## 疑問

--------

* 2018 年にもなって非同期 IO で疲弊している
* Rust は 2006 年に開発が始まった
* node.js も go も 2009 年には存在していた
* __今まで Rust は何をやっていたのか？__

--------

## 今まで Rust は(非同期 IO で)何をやっていたのか？

--------

### 2006~2010

--------

#### この頃の Rust

* __Graydon Hoare が Rust を開発開始__

--------

#### この時代

* 2006: Firefox2.0, jQuery, Twitter, AWS
* 2007: __Clojure__, Clang, Silverlight
* 2008: __epoll__, __Boost ASIO__ , __node.js__, Chrome
* 2009: __Go__, CoffeeScript, iPhone3GS, Android

--------

### 2010

--------

#### この頃の Rust

* __Graydon の個人プロジェクトが Mozilla のプロジェクトになる__

--------

#### この時代

* async-await(C#5.0), NaCL

--------

### 2011

--------

#### この頃の Rust

* Brendan Eich(JSの開発者)のServoの紹介スライドに出てくる程度
* 空白の時代

--------

#### この時代

* JAVA nio(jdk1.7), Kotlin, React, jQuery Deferred, Boost Context はてなブログ

--------

### 2012

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

### 2013

--------

#### この頃の Rust

* __Graydon が Mozilla を辞める__
* Rust コミュニティが C++er, Script言語er, 関数型言語er に別れ始める

--------

#### この時代

*  ES6Promise, Raspberry Pi, Docker

--------

### 2014

--------

#### この頃の Rust

* グリーンスレッドからネイティブスレッドモデルへと変更された
* グリーンスレッドやランタイムは削除された
* mio 開発開始
* rusti インタプリタが消えた

--------

#### この時代

* Swift, Flux, asyncio(Python3.4), asm.js, Emscripten, AWS Lambda, Kubernetes

--------

### 2015

--------

#### この頃の Rust

* Rust 1.0 が出た
* hyper や iron、 nickel が利用されはじめる
* コンテキストスイッチライブラリ context-rs と mio を使った rotor, mioco, coio-rs などが試みられる

--------

#### この時代

* WebAssembly, AlphaGo, ResNet, R-CNN, Raspberry Pi 2, Raspberry Pi Zero, TypeScript(async-await)

--------

### 2016

--------

#### この頃の Rust

* ライブラリレベルのコンテキストスイッチは危険だとの議論
* mio ベースのゼロコスト futures-rs と tokio-core の開発が始まる
* rotor, mioco などのそれ以前の非同期IOライブラリが開発終了

--------

#### この時代

* Raspberry Pi 3

--------

### 2017

--------

#### この頃の Rust

* hyper が tokio_core + futures-rs に対応
* actix-web, Gotham の開発が始まる
* coroutines が nightly に入る
* mdo-future, futures-await がリリース

--------

#### この時代

* ES2017 に async-await が入る

--------

### 2018

--------

#### この頃の Rust

* tokio-core が tokio になった
* iron, gotham が開発停止
* futures0.2 が yank された
* async-await が RFCS に入った

--------

## 今まで Rust は(非同期 IO で)何をやっていたのか？

--------

### 2013 年以前

非同期IOのランタイムサポートがあったが、 Rust 1.0 で消えた

--------

### 2015 年

mio + coroutine の非同期 IO があったが、 futures + tokio が出て消えた

--------

### 2017 年

tokio-core が主流になったが、 tokio runtime + async-await で混沌としている

--------

## 所感

* 環境変わりすぎ
* ライブラリが根付かない
* 賽の河原


-------

## とはいえ

tokio runtime + async-await でようやく Rust 本来の仕事ができそう

-------

## おわり

-------

## 参考資料

* [Rust と非同期 IO の歴史(資料編)](https://qiita.com/DUxCA/items/82140896c67da3f87a42)
* [非同期 IO について](https://qiita.com/DUxCA/items/1f3b1bd51e206ffdd2a6)

