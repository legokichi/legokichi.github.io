
続編 [Rustのエラーまわりの変遷](https://qiita.com/legokichi/items/d4819f7d464c0d2ce2b8)

## 動機

Rust で何かライブラリを作ったらエラーを定義設計する必要があります。
パースエラー、ネットワークエラー、etc...
例えば、

```rust
pub fn get_value_from_db(&self, key: &str) -> Result<String, Error>
```

のような API を公開するときに、 `Result<String, Error>` の `Error` をどうしようかという問題です。

2018 年 6 月現在の Rust のエラー設計のベストプラクティスは failure - https://github.com/rust-lang-nursery/failure - です。

### 補足情報

* 2年ほど前までは `error-chain` が推奨されていました - https://github.com/rust-lang-nursery/error-chain 
    * https://users.rust-lang.org/t/announcing-error-chain-a-library-for-consistent-and-reliable-rust-error-handling/6133
    * http://siciarz.net/24-days-rust-error_chain/
* しかし std の Error トレイトや error-chain マクロの欠点が明らかになり、 failure という新しいクレートと Fail トレイトが提案されました
    * この動画の 1:17:30 あたりが導入経緯について詳しいです -  https://air.mozilla.org/bay-area-rust-meetup-november-2017/
    * https://www.reddit.com/r/rust/comments/7h8v1z/errorchain_and_failure/
    * https://github.com/rust-lang-nursery/failure/issues/14
* このクレートは将来的に std に入るようです - https://github.com/rust-lang-nursery/failure/issues/209#issuecomment-400816826

## 使い方

※ この記事はライブラリ作者向けです。普段使いで unwrap しててエラー設計が必要ないという人は `failure::Error` 構造体を使ってください - https://boats.gitlab.io/failure/use-error.html


### `ErrorKind` を定義する

まず `enum ErrorKind` を定義します。
このとき 依存ライブラリのエラーも一緒に定義します。

```rust:error.rs
#[derive(Fail, Debug)]
pub enum ErrorKind {
    #[fail(display = "IO error")]
    Io,
    #[fail(display = "Serde error")]
    Serde,
    #[fail(display = "Hyper error")]
    Hyper,
    #[fail(display = "Cannot parse uri")]
    UrlParse,
    #[fail(display = "askama error")]
    Askama,
    #[fail(display = "service error")]
    Service,
}
```

この `Fail` トレイトがこのライブラリが提供する新しいエラー表現です。

### derive できないコードのボイラープレートを書く

次に以下のコードをコピペします。

```rust:error.rs
/* ----------- failure boilerplate ----------- */

use std::fmt;
use std::fmt::Display;
use failure::{Backtrace, Context, Fail};

#[derive(Debug)]
pub struct Error {
    inner: Context<ErrorKind>,
}

impl Fail for Error {
    fn cause(&self) -> Option<&Fail> {
        self.inner.cause()
    }

    fn backtrace(&self) -> Option<&Backtrace> {
        self.inner.backtrace()
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(&self.inner, f)
    }
}

impl Error {
    pub fn new(inner: Context<ErrorKind>) -> Error {
        Error { inner }
    }

    pub fn kind(&self) -> &ErrorKind {
        self.inner.get_context()
    }
}

impl From<ErrorKind> for Error {
    fn from(kind: ErrorKind) -> Error {
        Error {
            inner: Context::new(kind),
        }
    }
}

impl From<Context<ErrorKind>> for Error {
    fn from(inner: Context<ErrorKind>) -> Error {
        Error { inner }
    }
}

```

ここ https://boats.gitlab.io/failure/error-errorkind.html にかかれている通り、現状の failure 0.1 crate をうまく使うにはボイラープレートが伴います。
このボイラープレートは脳死でコピペしてください。マイクロソフトの [iotedge の edgelet](https://github.com/Azure/iotedge/tree/master/edgelet) もコピペしています。

* https://github.com/Azure/iotedge/blob/master/edgelet/edgelet-http/src/error.rs
* https://github.com/Azure/iotedge/blob/master/edgelet/edgelet-http-mgmt/src/error.rs
* https://github.com/Azure/iotedge/blob/master/edgelet/edgelet-utils/src/error.rs
* ...

現在このボイラープレートをマクロにしようという提案がされており、 1.0 では入るかもしれません。 - https://github.com/rust-lang-nursery/failure/issues/140#issuecomment-362439068

## エラー間の変換を書く
依存ライブラリのエラーを定義しようとしている自前のエラーへ変換するコードを書きます。
これを書いておくと依存ライブラリのエラーを自前のエラーへ `.map_err(Into::into)` で変換できるようになります。

```rust:error.rs
use failure::SyncFailure;
use hyper::Error as HyperError;
use askama::Error as AskamaError;
use std::io::Error as IOError;

impl From<IOError> for Error {
    fn from(error: IOError) -> Error {
        Error {
            inner: error.context(ErrorKind::Io),
        }
    }
}

impl From<HyperError> for Error {
    fn from(error: HyperError) -> Error {
        Error {
            inner: error.context(ErrorKind::Hyper),
        }
    }
}

impl From<UrlParseError> for Error {
    fn from(error: UrlParseError) -> Error {
        Error {
            inner: error.context(ErrorKind::UrlParse),
        }
    }
}

impl From<SyncFailure<AskamaError>> for Error {
    fn from(error: SyncFailure<AskamaError>) -> Error {
        Error {
            inner: error.context(ErrorKind::Askama),
        }
    }
}
```

現在の askama 0.5.0 のエラーには `error-chain` を利用していますが、`error-chain` が使用する std の Error トレイトは Fail トレイトが要求する `Sync` を満たしません。 - https://docs.rs/askama/0.7.0/askama/enum.Error.html#why-not-failureerror-chain
そのような場合は `SyncFailure` で包んでやる必要があります - https://github.com/rust-lang-nursery/failure/issues/109#issuecomment-350920299

※ askama 0.7.0 では `error-chain` の使用を辞めるようです - https://github.com/djc/askama/issues/92

### ライブラリを使用するコードを書く

ライブラリのエラーを↑で定義した自前の `struct Error` に変換します。

通常は `.map_err(Into::into)` する。 以下の例は `impl From<UrlParseError> for Error` を実装しているので `.map_err(Into::into)` できます。

```rust
serde_urlencoded::from_bytes(&buf).map_err(Into::into)
```

`Sync` を満たさないエラーは `.map_err(SyncFailure::new).map_err(Into::into)` する。
次の例は `impl From<SyncFailure<AskamaError>> for Error` を実装しているので into できます。

```rust
IndexTemplate { entries }.render().map_err(SyncFailure::new).map_err(Into::into)
```

`hyper` で書いた Web サーバでの使用例です。

```rust:main.rs
fn handler(ctx: service::Posts, req: Request<Body>) -> Box<Future<Item=Response<Body>, Error=Error> + Send + 'static> {
    let mut res = Response::new(Body::empty());
    match (req.method(), req.uri().path()) {
        (&Method::GET, "/") => {
            #[derive(Deserialize)]
            struct Query {
                offset: u64,
                limit: u64,
            }
            let fut = mdo!{
                let query = req.uri().query().unwrap_or("offset=0&limit=100");
                Query{ offset, limit } =<< future::result(serde_urlencoded::from_str(query)).map_err(Into::into);
                (_len, lst) =<< ctx.list(offset, limit).map_err(Into::into);
                let entries = lst.iter().map(|o| Entry{
                    timestamp: DateTime::from_utc(o.timestamp, Utc),
                    username: o.author.to_string(),
                    message: o.body.to_string()
                }).collect();
                tmp =<< future::result(IndexTemplate { entries }.render()).map_err(SyncFailure::new).map_err(Into::into);
                let _ = *res.body_mut() = Body::from(tmp);
                ret future::ok(res)
            };
            Box::new(fut)
        },
        (&Method::POST, "/") => {
            #[derive(Deserialize)]
            struct FormData {
                username: String,
                message: String,
            }
            let fut = mdo!{
                let body = req.into_body();
                buf =<< body.concat2().map_err(Into::into);
                FormData{ username, message } =<< future::result(serde_urlencoded::from_bytes(&buf)).map_err(Into::into);
                _ =<< ctx.create(&username, &message).map_err(Into::into);
                let _ = res.headers_mut().insert(LOCATION, HeaderValue::from_static("/"));
                let _ = *res.status_mut() = StatusCode::SEE_OTHER;
                ret future::ok(res)
            };
            Box::new(fut)
        },
        _ => {
            *res.status_mut() = StatusCode::NOT_FOUND;
            Box::new(future::ok(res))
        }
    }
}
```

#### 補足1 `mdo-future` について

`future::result(serde_urlencoded::from_bytes(&buf)).map_err(Into::into)` としているのは `mdo-future` - https://github.com/danslapman/rust-mdo-future - を使って非同期エラーと混ぜて使いたいからです。実際便利。

#### 補足2 `Fail` と `Future`

最新の tokio runtime は work stealing アルゴリズムでタスクキューに溜まった future をスレッドプールで処理します。
そのため future がどのスレッドで実行されるのかは実行時に決まります。
なので `Box<Future<Item=Response<Body>, Error=Error> + Send + 'static>` のように `Send` をつけてやる必要があります。
`Fail` トレイトは `Display + Debug + Send + Sync + 'static` を要求するのでスレッドセーフです - https://docs.rs/failure/0.1.1/failure/trait.Fail.html

### エラーハンドリングを書く

Web サーバを書いているとエラーに応じてレスポンスのステータスコードを変えたくなります。
以下の例は `hyper` で書いた Web サーバでエラーハンドリングをする例です。

`handler(srv.clone(), req).then(error_handler)` のように `.then` でつなげて書くことを意図しています。

```rust:main.rs
fn error_handler(ret: Result<Response<Body>, Error>) -> Box<Future<Item=Response<Body>, Error=hyper::Error> + Send + 'static> {
    match ret {
        Ok(res) => Box::new(future::ok(res)),
        Err(err) =>{
            let mut fail: &Fail = &err;
            let mut message = err.to_string();
            
            while let Some(cause) = fail.cause() {
                message.push_str(&format!("\n\tcaused by: {}", cause.to_string()));
                fail = cause;
            }
            let status_code = match *err.kind() {
                ErrorKind::UrlParse | ErrorKind::Hyper => StatusCode::BAD_REQUEST,
                _ => StatusCode::INTERNAL_SERVER_ERROR,
            };

            let body = json!({
                "message": message,
            }).to_string();

            let res: Response<Body> = Response::builder()
                .status(status_code)
                .header(CONTENT_TYPE, "application/json")
                .header(CONTENT_LENGTH, body.len().to_string().as_str())
                .body(body.into())
                .expect("response builder failure");

            Box::new(future::ok(res.map(Into::into)))
        }
    }
}
```

このようにエラーの原因を遡って表示できます。

```json
{"message":"service error\n\tcaused by: db error\n\tcaused by: diesel query result error\n\tcaused by: attempt to write a readonly database"}
```

このWebサーバはビジネスロジックを記述した `service` クレートを使用していますが、その中で DB アクセスを抽象化した DB クレートでエラーが起きており、 さらにその中の diesel を使っている部分がエラーを返していることがこのエラーからわかります。これらはすべて failure を使っているので統一的に扱えています。

このエラーの原因を遡る方法はこの部分で実現されています。

```rust
let mut fail: &Fail = &err;
let mut message = err.to_string();

while let Some(cause) = fail.cause() {
    message.push_str(&format!("\n\tcaused by: {}", cause.to_string()));
    fail = cause;
}
```

`fail.cause()` はそのエラーの根本原因を返します。
それを `fail = cause;` で代入してループすることでより深い原因を探っています。


`fail.causes()` はイテレータを返すので、人によってはそれを `fold` したほうがわかりやすいかもしれません - https://docs.rs/failure/0.1.1/failure/trait.Fail.html#method.causes

バックトレースがほしい場合は この `cause` に対して `backtrace()` してやると、そのエラーが `backtrace` を持ってい場合は `Option<&Backtrace>` の `Some` が返ります。 - https://docs.rs/failure/0.1.1/failure/trait.Fail.html#method.backtrace

## ドキュメントのわかりくさについて

公式のドキュメント - https://boats.gitlab.io/failure/ - がわかりにくいという issue - https://github.com/rust-lang-nursery/failure/issues/140#issuecomment-369963154 - が立っており、現在ここ - https://github.com/rust-lang-nursery/failure/issues/209#issuecomment-394914709 - で新しいドキュメントの草稿が練られています。


## 感想

* 正直マイクロソフトの https://github.com/Azure/iotedge/blob/master/edgelet のコード例見るまでなんもわからんかった
* futures も failure も 0.1 で基礎的なライブラリが安定してないので厳しい
* ここで使ったサンプルコードは https://github.com/legokichi/rust-sandbox/tree/89c01aa583f785efa4205c07b6410664b8336bec/diesel-server にあります

## バージョン情報
* serde_urlencoded 0.5.2
* failure 0.1.1
* failure_derive 0.1.1
* askama 0.5.0
* futures 0.1.21 
* hyper 0.12.3
* rustc 1.27.0 (3eda71b00 2018-06-19)

--------

最新情報への誘導リンク

* Rustでエラーを合成する - https://qiita.com/termoshtt/items/8c015d9289613ec640f1
* Rust: failureを用いたエラー型定義 -https://qiita.com/OvQ/items/cb866c04196dc59fe847
