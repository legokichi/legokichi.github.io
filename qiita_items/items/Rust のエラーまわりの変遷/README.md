[Rust LT #6](https://rust.connpass.com/event/133657/) で発表したスライド


# `Error` トレイトについて

------

## `std::error::Error` トレイトとは

* [2014 年 10 月 にRFC 入りしたトレイト (rfcs#201)](https://github.com/rust-lang/rfcs/blob/master/text/0201-error-chaining.md)
* __[Rust 1.0 の半年前](https://blog.rust-lang.org/2015/05/15/Rust-1.0.html)__
* `Box<dyn Error>` として使うことを想定

------

## `Error` トレイト
こんなの

```rs
pub trait Error: Debug + Display {
    fn description(&self) -> &str;
    fn cause(&self) -> Option<&dyn std::error::Error>;
}
```

------

### `Error::description` でエラーの内容を表示できる

```rs
println!("{}", err.description());
```

------

### `Error::cause` でエラーの元を辿れる (cause chain)

```rs
let mut cause = err.cause();
while let Some(err) = cause {
  println!("{}", err.description());
  cause = err.cause();
}
```

------

## `Error` トレイトの問題点

------

### 1. `Error` と `Debug` と `Display` トレイトを実装しないといけない

Error を derive できない（※当時は derive macro などなかった）

------

```rs
#[derive(Debug)]
enum MyError {
    Io(std::io::Error),
    Parse(std::num::ParseIntError),
}

// ボイラープレート
// pub trait Error: Debug + Display { ... }
impl std::error::Error for MyError {
    fn description(&self) -> &str {
        match *self {
            MyError::Io(ref err) => err.description(),
            MyError::Parse(ref err) => err.description(),
        }
    }
    fn cause(&self) -> Option<&std::error::Error> {
        match *self {
            MyError::Io(ref err) => Some(err),
            MyError::Parse(ref err) => Some(err),
        }
    }
}

impl std::fmt::Display for MyError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            MyError::Io(ref err) => write!(f, "ファイル開けへんやんけ: {}", err),
            MyError::Parse(ref err) => write!(f, "パースできへんやんけ: {}", err),
        }
    }
}
```

------

### 2. スタックトレースがとれない

このエラーはどこから来たのかなんもわからん

------

### 3. `Error::description` と `Display` で役割が被ってる

```rs
// 何が違うの？
println!("{}", err.description());
println!("{}", err);
```

------

### 4. cause チェーンがイテラブルでない

```rs
// なぜそこで while
let mut cause = err.cause();
while let Some(err) = cause {
  cause = err.cause();
}
```

------

### 5. 元のエラー型にダウンキャストできない


```rs
// Error::cause の戻り値に 'static がついてない
let cause: Option<&Error> = err.cause();
```

------

### 6. Send も Sync も 'static ない

tokio などで使うのが大変

------

## 結論: `Error` トレイトは問題だらけなので……

* error-chain
  * 問題だらけの `Error` を使うためのベストプラクティス 
* failure
  *  そもそも `Error` なんて使わんきゃええ
* RFC2504, "Fix the Error Trait"
  *  いっそ `Error` を改善しよう 

------

# error-chain について

------

## error-chain クレート

* Rust 1.0 の半年後の [2016 年 1 月に作られた](https://users.rust-lang.org/t/announcing-error-chain-a-library-for-consistent-and-reliable-rust-error-handling/6133)
* [3 年後の 2019 年に開発停止](https://users.rust-lang.org/t/error-chain-is-no-longer-maintained/27561)

------

### `error_chain!` マクロ

マクロで `Error` トレイトのボイラープレートを一括生成できる

------

```rs
error_chain!{
  types {  MyError, MyErrorKind, MyResultExt, MyResult; }
  links {
    Another(AnotherError, AnotherErrorKind);
  }
  foreign_links {
    Io(::std::io::Error);
  }
  errors {
    CannotOpenFile(path: String) {
      description("cannot open file")
      display("CannotOpenFile: '{}'", path)
    }
  }
}
```

------

### この `error_chain!` マクロで生成されるコードは……

------

### `Error` トレイトを実装した `MyError` 構造体

```rs
pub struct MyError { ... }
impl MyError { ... }
// Error トレイト の実装
impl Error for MyError { ... }
impl Debug for MyError { ... }
impl Display for MyError { ... }
```

------

### エラー原因を保持するための `MyErrorKind` 列挙体

```rs
pub enum MyErrorKind {
  Msg(String),
  Another(AnotherErrorKind),
  Io(::std::io::Error),
  CannotOpenFile(path: String),
}
impl MyErrorKind { ... }
impl Debug for MyErrorKind { ... }
impl Display for MyErrorKind { ... }
```

------

### `error_chain::ChainedError` 拡張トレイトの実装

```rs
impl error_chain::ChainedError for MyError { ... }
```

----- 

### `ChainedError` を `Result` で使えるようにする `ResultExt` トレイトの実装

```rs
pub trait MyResultExt<T> { ... }
pub type MyResult<T> = Result<T, MyError>;
```

------

### `From` トレイトの実装

```rs
impl From<MyErrorKind> for MyError { ... }
impl<'a> From<&'a str> for MyError { ... }
impl From<String> for MyError { ... }
impl From<another_errors::Error> for MyError { ... }
impl<'a> From<&'a str> for MyErrorKind { ... }
impl From<String> for MyErrorKind { ... }
impl From<MyError> for MyErrorKind  { ... }
impl From<another_errors::ErrorKind> for MyErrorKind { ... }
```

------

## この膨大なコードのおかげで……

------

### 1. `enum ErorrKind` に原因のエラーを持てる

```rust
pub enum MyErrorKind {
  Msg(String),
  Another(AnotherErrorKind),
  Io(::std::io::Error),
  CannotOpenFile(path: String),
}
```

------

### 2. エラーチェーンを積める

```rs
use std::fs::File;
use errors::{MyError, MyErrorKind, MyResultExt};
try!(File::open("foo.txt")
  .map_err(MyErrorKind::Io)
  .chain_err(|| "ファイル開けへんやんけ"));
  .chain_err(|| MyErrorKind::CannotOpenFile("foo.txt".to_string)));
// CannotOpenFile -> Msg -> Io -> std::io::Error
```

------

### 3. `MyError::iter` で原因をイテレートできる

```rs
for err in err.iter() {
  println!("{}", err);
}
```

------

### 4. `MyError::backtrace` でバックトレースがとれる

```rs
// need RUST_BACKTRACE=1
if let Some(trace) = err.backtrace() {
  // io::Error ではなく MyError が作られた時点のトレースが得られる
  println!("{:?}", trace);
}
```

------

## error-chain の問題点

* derive macro なんてなかった
* 生成されるコードが __膨大__
* __初見殺し__
* 生成される `MyError` が `!Sync` で使い勝手が悪い

------

## 結論: error-chain は過去の遺産

------

## じゃあ `failure` を使えばいいのか？

------

# failure について

------

## failure クレート

* [2017 年 11 月に登場](https://boats.gitlab.io/blog/post/2017-11-16-announcing-failure/)
* 問題だらけの `Error` トレイトを置き換えるために開発された
* `Error` トレイトの代わりに `Fail` トレイトを導入
* `Box<dyn Error>` の代わりに `failure::Error` 構造体を導入

------

### derive できる

```rs
#[derive(Debug, Fail)]
pub enum MyError {
  #[fail(display = "Input was invalid UTF-8 at index {}", _0)]
  Utf8Error(usize),
  #[fail(display = "IoError: {}", _0)]
  Io(#[cause] io::Error),
}
```

------

### cause chain がイテラブル

```rs
impl Fail {
  pub fn iter_causes(&self) -> Causes { ... }
  ...
}
```

------

### backtrace が取れる

```rs
pub trait Fail: Display + Debug + Send + Sync + 'static {
  fn backtrace(&self) -> Option<&Backtrace> { ... }
  ...
}
```

------

### `Fail` に `Send + Sync + 'static` がついてる

tokio でも使える

```rust
pub trait Fail: Display + Debug + Send + Sync + 'static {
  ...
}
```

------

### ダウンキャストできる

`Fail` に `'static` がついてるので

```rust
pub trait Fail: Display + Debug + Send + Sync + 'static {
  ...
}
impl Fail {
  pub fn downcast_ref<T: Fail>(&self) -> Option<&T> { ... }
  pub fn downcast_mut<T: Fail>(&mut self) -> Option<&mut T> { ... }
  ...
}
```

------

### cause chain を積める

```rust
err
 .context(format_err!("Error code: なんかエラーおきた}"))
 .context("なんかエラーおきた".to_string());
```

------

## 結論: failure はすごくいいが……

------


# RFC 2504, "Fix the Error trait"

------

## 新しい Error トレイト

* failure から半年後の [2018 年 7 月に提案、　8 月にマージ](https://github.com/rust-lang/rfcs/pull/2504)
* failure の知見を取り込んだ改善

------

## 新しい Error トレイト

* backtrace できる
* ダウンキャストできる

```rust
trait Error: Display + Debug {
    fn backtrace(&self) -> Option<&Backtrace>;
    fn source(&self) -> Option<&dyn Error + 'static>;
}
```

------

## 新しい Error トレイト

以前のメソッドがひとつも残ってない

```rust
trait Error: Display + Debug {
    fn backtrace(&self) -> Option<&Backtrace>;
    fn source(&self) -> Option<&dyn Error + 'static>;
}
```

------

## 未解決の問題

* Backtrace の具体的な API が未定
* イテラブルな cause chain の API が未定
* `derive(Error)` できない

------

## 2019年6月現在

------

### error-chain は開発停止

------

### failure は凍結

* メジャーアップデートはは実装が落ち着くまで停止中 (まるで futures-0.1 のよう？)
   * `Fail` -は`ErrorExt` にして新 `Error` トレイトを継承するかも
   * `failure::Error` は`failure::DefaultError` になるかも

------

### ポスト failure が増殖中

* [err-derive](https://users.rust-lang.org/t/announcing-err-derive-yet-another-error-handling-library/23594)
* [SNAFU](https://users.rust-lang.org/t/snafu-0-2-1-released/27269)
* [errer](https://crates.io/crates/errer/0.13.0)

などなど

------

# おわり

------

# Appendix. Rust のエラーまわりの歴史

## 2014

* 2014-05-24 - Error handling in Rust is actually pretty awesome. - https://news.ycombinator.com/item?id=7792438
* 2014-08-16 - __RFC: error interoperation #201__ - https://github.com/rust-lang/rfcs/pull/201
* 2014-08-16 - 0.0.0 - https://github.com/reem/rust-error/
* 2014-08-30 - RFC: Error conventions #220 - https://github.com/rust-lang/rfcs/pull/220
* 2014-09-17 - First-class error handling with `?` and `catch` #243 - https://github.com/rust-lang/rfcs/pull/243
* 2014-10-13 - RFC: Anonymous enum types called joins, as `A | B` #402 - https://www.reddit.com/r/rust/comments/2j1k4h/how_do_i_handle_causes_with_generic_error_handling/
* 2014-10-16 - On Error Handling in Rust - http://lucumr.pocoo.org/2014/10/16/on-error-handling/
* 2014-10-16 - https://github.com/rust-lang/rfcs/pull/402
* 2014-10-30 - RFC: Error conventions, take 3 #236 - https://github.com/rust-lang/rfcs/pull/236
* 2014-10-31 - https://github.com/rust-lang/rfcs/blob/master/text/0236-error-conventions.md
* 2014-11-06 - http://lucumr.pocoo.org/2014/11/6/error-handling-in-rust/
* 2014-11-21 - Error Handling - https://doc.rust-lang.org/1.0.0/book/error-handling.html - https://github.com/rust-lang/book
* 2014-12-11 - Anonymous enum types (A|B) take 2 #514 - https://github.com/rust-lang/rfcs/pull/514


## 2015

* 2015-01-29 - Extensible enums #757 - https://github.com/rust-lang/rfcs/pull/757
* 2015-04-07 - Consider changing io::Error to use Arc so it can implement Clone #24135 - https://github.com/rust-lang/rust/issues/24135
* 2015-04-11 - Add `Sync` to the bounds in `io::Error` #24133 -  https://github.com/rust-lang/rust/pull/24133
* 2015-04-12 - RFC for adding Sync to io::Error #1057 - https://github.com/rust-lang/rfcs/pull/1057
* 2015-05-13 - Quick draft "Result::expect" rfc #1119 - https://github.com/rust-lang/rfcs/pull/1119
* 2015-05-14 - __Error Handling in Rust__ - https://blog.burntsushi.net/rust-error-handling/
* 2015-06-02 - Error handling in Rust - https://xania.org/201506/rust-errors
* 2015-07-24 - RFC: Stabilize catch_panic #1236 - https://github.com/rust-lang/rfcs/pull/1236
* 2015-08-16 - 0.1.1  - https://crates.io/crates/enum_derive
* 2015-08-29 - RFC : Try-Catch Blocks - https://users.rust-lang.org/t/rfc-try-catch-blocks/2663
* 2015-09-10 - 0.1.0 - https://github.com/tailhook/quick-error
* 2015-09-13 - Compare enums only by variant, not value - https://stackoverflow.com/questions/32554285/compare-enums-only-by-variant-not-value
* 2015-10-08 - Allow a custom panic handler #1328 - https://github.com/rust-lang/rfcs/pull/1328
* 2015-11-02 - RUST AND GO (ERROR HANDLING) - http://tyoverby.com/posts/rust-vs-go.html
* 2015-12-02 - `String, From and Box<Error>` interoperation could be better. #30156 - https://github.com/rust-lang/rust/issues/30156


## 2016

* 2016-01-08 - __Announcing error-chain, a library for consistent and reliable Rust error handling__ - https://users.rust-lang.org/t/announcing-error-chain-a-library-for-consistent-and-reliable-rust-error-handling/6133
* 2016-03-29 - Error Handling in Rust - https://facility9.com/2016/03/error-handling-in-rust/
* 2016-03-24 - 0.1.0 - https://crates.io/crates/simple-error
* 2016-04-13 - 0.2.0 - https://github.com/rust-lang/backtrace-rs
* 2016-04-29 - 0.1.8- https://crates.io/crates/error-chain
* 2016-05-18 - Adding Context to Rust Errors - https://medium.com/@paulcolomiets/adding-context-to-rust-errors-3c6f58a7074b
* 2016-05-27 - Rust 1.9 improves speed, error handling - https://www.infoworld.com/article/3075321/rust-19-improves-speed-error-handling.html
* 2016-06-08 - Why I’m dropping Rust - https://users.rust-lang.org/t/announcing-error-chain-a-library-for-consistent-and-reliable-rust-error-handling/6133
* 2016-07-16 - Can’t to introspect error cause - https://users.rust-lang.org/t/cant-to-introspect-error-cause/6536
* 2016-07-21 - Elegant Library APIs in Rust - https://deterministic.space/elegant-apis-in-rust.html
* 2016-07-22 - Rust - Custom Error Types | rust Tutorial - RIP Tutorial - https://riptutorial.com/rust/example/6788/custom-error-types
* 2016-08-01 - Add a `compile_error!` macro to libstd #1695 - https://github.com/rust-lang/rfcs/pull/1695
* 2016-08-03 - Helping with the Rust Errors  - https://www.jonathanturner.org/2016/08/helping-out-with-rust-errors.html
* 2016-08-14 - Problems getting error description in Rust - https://stackoverflow.com/questions/38936996/problems-getting-error-description-in-rust
* 2016-08-24 - __Error causes can't be downcast #35943__ - https://github.com/rust-lang/rust/issues/35943
* 2016-09-11 - The Error Model - http://joeduffyblog.com/2016/02/07/the-error-model/
* 2016-10-24 - Accumulating Results in Rust With Validated - https://beachape.com/blog/2016/10/24/accumulating-results-in-rust-with-validated/
* 2016-10-27 - Rewinding time with rr & Rust to debug a terrible error message - https://blog.faraday.io/reversing-rust-debugging-a-terrible-error-message-backwards-in-time-with-rr-2/
* 2016-11-20 - Error-chain 0.6.0 - https://users.rust-lang.org/t/error-chain-0-6-0/8079/2
* 2016-11-21 - 0.2.0  - https://crates.io/crates/macro_attr
* 2016-11-30 - Starting a new Rust project right, with error-chain - https://brson.github.io/2016/11/30/starting-with-error-chain
* 2016-12-01 - Starting a new Rust project right, with error-chain - https://www.reddit.com/r/rust/comments/5ftgv5/starting_a_new_rust_project_right_with_errorchain/
* 2016-12-15 - Stroustrup's Rule and Layering Over Time - https://thefeedbackloop.xyz/stroustrups-rule-and-layering-over-time/ - https://news.ycombinator.com/item?id=13192052
* 2016-12-18 - __24 days of Rust - error_chain__ - https://siciarz.net/24-days-rust-error_chain/


## 2017

* 2017-01-16 - 0.0.0 - https://crates.io/crates/derive-error
* 2017-01-20 - RFC 1859: Tryトレイト - https://github.com/rust-lang/rfcs/pull/1859 - https://gist.github.com/sile/7b967511f9d922ecfab80dc27d17d82d
* 2017-02-06 - Custom Error types in Rust and the ? operator - https://medium.com/@fredrikanderzon/custom-error-types-in-rust-and-the-operator-b499d0fb2925
* 2017-02-11 - A way to customize Rust error messages - https://deterministic.space/hook-into-rustc-errors.html
* 2017-02-15 - Nice error handling in Rust - https://www.reddit.com/r/rust/comments/5u0vkk/nice_error_handling_in_rust/
* 2017-02-22 - RFC: `?` in `main` #1937 - https://github.com/rust-lang/rfcs/pull/1937
* 2017-03-03 - How do you define custom `Error` types in Rust? - https://stackoverflow.com/questions/42584368/how-do-you-define-custom-error-types-in-rust/42584607#42584607
* 2017-03-13 - Rustでエラーを合成する - https://qiita.com/termoshtt/items/8c015d9289613ec640f1/revisions
* 2017-03-22 - Error Handling - https://stevedonovan.github.io/rust-gentle-intro/6-error-handling.html
* 2017-04-02 - Pre RFC ish: Bring ensure! macro from error-chain into std? - https://users.rust-lang.org/t/pre-rfc-ish-bring-ensure-macro-from-error-chain-into-std/10181
* 2017-05-06 - Error types - as enums or as structs - https://users.rust-lang.org/t/error-types-as-enums-or-as-structs/9797/3
* 2017-05-24 - __Future-proofing enums/structs with #[non_exhaustive] attribute #2008__ - https://github.com/rust-lang/rfcs/pull/2008
* 2017-06-03 - Rustのtry-catch構文 - https://qnighy.hatenablog.com/entry/2017/06/03/070000
* 2017-06-06 - Refactor error types #936 - https://github.com/diesel-rs/diesel/issues/936
* 2017-07-18 - __Tracking issue for RFC 1937: `?` in `main` #43301__ - https://github.com/rust-lang/rust/issues/43301
* 2017-08-27 - __Tracking issue for RFC 2008: Future-proofing enums/structs with #[non_exhaustive] attribute #44109__ - https://github.com/rust-lang/rust/issues/44109
* 2017-09-17 - Rust でエラー型に Clone が実装されていてほしい - https://blog.cardina1.red/2017/09/15/rust-clonable-error-types/
* 2017-10-26 - Improve errors and error handling #460 - https://github.com/amethyst/amethyst/issues/460
* 2017-11-03 - Rust futures: an uneducated, short and hopefully not boring tutorial - Part 2 - https://dev.to/mindflavor/rust-futures-an-uneducated-short-and-hopefully-not-boring-tutorial---part-2-8dd
* 2017-11-04 - Improving error matching consistency (at least for testing) - https://github.com/rust-lang-nursery/failure/issues/14
* 2017-11-16 - __Announcing Failure__ - https://boats.gitlab.io/blog/post/2017-11-16-announcing-failure/
* 2017-11-17 - Announcing Failure - https://www.reddit.com/r/rust/comments/7dg95u/announcing_failure/
* 2017-11-20 - Rustの『RFC 2113: dynトレイト構文』の要約メモ - https://gist.github.com/sile/0615a6daa1f5576d63c8775cc5a94f88
* 2017-11-24 - Consider examples for "failure" crate? #370 - https://github.com/rust-lang-nursery/rust-cookbook/issues/370
* 2017-11-30 - __Failure 0.1.1 released__ - https://boats.gitlab.io/blog/post/2017-11-30-failure-0-1-1/
* 2017-11-30 - Bury Error::description() #2230 - https://github.com/rust-lang/rfcs/pull/2230
* 2017-12-03 - error-chain and failure - https://amp.reddit.com/r/rust/comments/7h8v1z/errorchain_and_failure/


## 2018

* 2018-01-06 - https://mobile.twitter.com/qnighy/status/949648140759572480
* 2018-01-22 - Dyn Trait & #[derive(Debug, Fail)] - https://users.rust-lang.org/t/dyn-trait-derive-debug-fail/18250
* 2018-02-03 - Any plans for clonable errors? #148 - https://github.com/rust-lang-nursery/failure/issues/148
* 2018-02-18 - Rustのパニック機構 - https://qnighy.hatenablog.com/entry/2018/02/18/223000
* 2018-02-22 - __Failure 1.0.0 on March 15__ - https://boats.gitlab.io/blog/post/2018-02-22-failure-1.0/
* 2018-03-02 - Rustのエラーハンドリング - https://medium.com/@11Takanori/rust%E3%81%AE%E3%82%A8%E3%83%A9%E3%83%BC%E3%83%8F%E3%83%B3%E3%83%89%E3%83%AA%E3%83%B3%E3%82%B0-6660cd4d16c0
* 2018-03-08 - __error-chain deprecation #181__ - https://github.com/rust-lang-nursery/failure/issues/181
* 2018-03-09 - Redefining Failure - https://epage.github.io/blog/2018/03/redefining-failure/
* 2018-04-04 - Migrate application from error-chain to failure - https://users.rust-lang.org/t/migrate-application-from-error-chain-to-failure/16601
* 2018-04-04 - RFC: Reserve `try` for `try { .. }` block expressions #2388 - https://github.com/rust-lang/rfcs/pull/2388
* 2018-04-29 - https://cat-in-136.github.io/2018/04/rust-error-handling-question.html
* 2018-05-03 - "TL;DR - I think showing the Debug message of an error was a mistake, and that a better choice would be to use the Display message of an error." - https://github.com/rust-lang/rust/issues/43301#issuecomment-388575730
* 2018-05-26 - __Maintainership of failure #209__ - https://github.com/rust-lang-nursery/failure/issues/209
* 2018-06-30 - rust のエラーライブラリは failure を使え！ - https://qiita.com/legokichi/items/d76b6aa5dac2ad781bda
* 2018-07-19 - __Fix the Error trait #2504__ - https://github.com/rust-lang/rfcs/pull/2504
  * https://boats.gitlab.io/failure/error-msg.html
* 2018-07-22 - https://mobile.twitter.com/qnighy/status/1021021205166297090
* 2018-08-14 - Custom error guidelines? - https://users.rust-lang.org/t/custom-error-guidelines/19547
* 2018-08-19 - __Tracking issue for RFC 2504, "Fix the Error trait" #53487__ - https://github.com/rust-lang/rust/issues/53487
* 2018-10-08 - Current state of error handling in Rust? https://www.reddit.com/r/rust/comments/9m5w9a/current_state_of_error_handling_in_rust/
* 2018-11-02 - 1.0.0  - https://crates.io/crates/custom_error
* 2018-11-21 - `impl Error for String` - https://internals.rust-lang.org/t/impl-error-for-string/8881
* 2018-12-02 - Rust ergonomics for error handling #23 - https://github.com/awslabs/aws-lambda-rust-runtime/issues/23
* 2018-12-04 - Rustのエラー処理 - https://qiita.com/fujitayy/items/cafe661415b6aa33d884
* 2018-12-05 - __on error handling #502__ - https://github.com/rust-lang-nursery/rust-cookbook/issues/502
* 2018-12-05 - Introduce amethyst_error #1220 - https://github.com/amethyst/amethyst/pull/1220
* 2018-12-15 - The state of error handling in the 2018 edition - https://users.rust-lang.org/t/the-state-of-error-handling-in-the-2018-edition/23263
* 2018-12-18 - __Announcing `err-derive` - yet another error handling library__ - https://users.rust-lang.org/t/announcing-err-derive-yet-another-error-handling-library/23594
* 2018-12-18 - [RFC] Error handling in Rust runtime #54 - https://github.com/awslabs/aws-lambda-rust-runtime/issues/54
* 2018-12-18 - [RFC] Runtime crates refactor #53 - https://github.com/awslabs/aws-lambda-rust-runtime/issues/53
* 2018-12-19 - Fail-rs 0.2.1 released - https://users.rust-lang.org/t/fail-rs-0-2-1-released/23355
* 2018-12-30 - __Towards 0.2 and Beyond #287__ - https://github.com/rust-lang-nursery/failure/issues/287

## 2019

* 2019-01-07 - How to port error_chain to 2018? - https://users.rust-lang.org/t/how-to-port-error-chain-to-2018/23960
* 2019-01-13 - Best practice to return a `Result<_, impl Error>` and not a `Result<_, &str>` in Rust? - https://stackoverflow.com/questions/54159232/best-practice-to-return-a-result-impl-error-and-not-a-result-str-in-rus
* 2019-01-15 - The Evolution of a Rust Programmer - http://antoyo.ml/evolution-rust-programmer
* 2019-01-19 - 0.1.0 - https://crates.io/crates/errer
* 2019-01-20 - Rustでtraitのassociated typeに対してtrait boundaryを課す - https://in-neuro.hatenablog.com/entry/2019/01/20/210653
* 2019-01-21 - 0.1.0 - https://crates.io/crates/easy-error
* 2019-01-28 - 0.1.0 - https://crates.io/crates/snafu
* 2019-02-09 - Custom Exit Status Codes with ? in main - https://www.joshmcguigan.com/blog/custom-exit-status-codes-rust/
* 2019-02-17 - __Tracking issue for error source iterators #58520__ - https://github.com/rust-lang/rust/issues/58520
* 2019-03-01 - Rust: failureを用いたエラー型定義 - https://qiita.com/OvQ/items/cb866c04196dc59fe847
* 2019-03-02 - std: implement `Error` for `Box<dyn Error>` #58859 - https://github.com/rust-lang/rust/pull/58859
* 2019-03-11 - RFC 2504 "fix_error": Rustの新たなErrorトレイト - https://qiita.com/termoshtt/items/830008898f90c647a971
* 2019-03-22 - On error handling - https://users.rust-lang.org/t/on-error-handling/26515
* 2019-03-27 - 0.1.1 - https://crates.io/crates/error-rules
* 2019-04-02 - Review and address feedback #46 - https://github.com/shepmaster/snafu/issues/46#issuecomment-486482005
* 2019-04-09 - rust-lang/rustlings/exercises/error_handling/ - https://github.com/rust-lang/rustlings/tree/d7e58ee1af3945dc2b13f2ebb9daa7c2b2a26175/exercises/error_handling
* 2019-04-15 - SNAFU 0.2.1 Released - https://users.rust-lang.org/t/snafu-0-2-1-released/27269
* 2019-04-19 - removed error-chain from examples having only one error variant #525 - https://github.com/rust-lang-nursery/rust-cookbook/pull/525
* 2019-04-24 - __Error-chain is no longer maintained__ - https://users.rust-lang.org/t/error-chain-is-no-longer-maintained/27561
* 2019-04-25 - Failing to port from error-chain to std error handling - https://users.rust-lang.org/t/failing-to-port-from-error-chain-to-std-error-handling/27611
* 2018-04-25 - Any experiences with `snafu`? - https://www.reddit.com/r/rust/comments/bgy80y/any_experiences_with_snafu/
* 2019-04-26 - Today, I would probably not use any error handling helper library for a library. - https://users.rust-lang.org/t/error-chain-is-no-longer-maintained/27561/8
* 2019-04-29 - Rust’s 2019 roadmap - https://www.reddit.com/r/rust/comments/bgikmz/rusts_2019_roadmap/eln8h8y/
* 2019-05-13 - Security advisory for the standard library  -https://blog.rust-lang.org/2019/05/13/Security-advisory.html
* 2019-05-21 - `Returning errors: -> Result<(), Box<error::Error>>` https://www.reddit.com/r/rust/comments/br2gut/returning_errors_result_boxerrorerror/
* 2019-05-22 - https://twitter.com/wx257osn2/status/1131255087379013632

