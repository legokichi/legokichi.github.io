[WebAssembly Advent Calendar 2018 12日目 Rust における wasm-bindgen と wasm-pack と cargo-web と stdweb の違い](https://qiita.com/legokichi/items/5d6344314ab6d6633554) の続編

あれから一年経ったので rustasync と rustwasm の進捗をチェック！

# wasm-pack で JS の Promise を await できる非同期 Rust を書いて node.js で動かす


## setup

```bash
rustup default nightly
rustup target add wasm32-unknown-unknown
curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
cargo install cargo-generate
cargo generate --git https://github.com/rustwasm/wasm-pack-template
```

* nightly, wasm32-unknown-unknown, wasm-pack, cargo-generate をインストールして wasm-pack を使うためのディレクトリを生成する
* https://rustwasm.github.io/docs/wasm-pack/tutorials/npm-browser-packages/getting-started.html
* 1.41.0-nightly

## Cargo.toml の構成

```toml:Cargo.toml
[package]
name = "rust-wasm-nodejs"
version = "0.1.0"
edition = "2018"

[lib]
crate-type = ["cdylib", "rlib"]

[features]
default = []

[dependencies]
wasm-bindgen = { version = "0.2.55", features = ["serde-serialize", "nightly"] }
wasm-bindgen-futures = "0.4.5"
js-sys = "0.3.32"
web-sys = { vesrion = "0.3.32", features = ["console"] }
serde = { version = "1", features = ["derive"] }
```

* `wasm-bindgen = { version = "0.2", features = ["serde-serialize"] }` について
  * https://rustwasm.github.io/docs/wasm-bindgen/reference/arbitrary-data-with-serde.html
  * https://docs.rs/wasm-bindgen/0.2.55/wasm_bindgen/
  * https://github.com/rustwasm/wasm-bindgen/blob/b9c93a3c24f25456c9867b4fe50ad19fb844a6f1/Cargo.toml#L26
* `js-sys` について
  * `js_sys::Promise` が生えてる
  * https://github.com/rustwasm/wasm-bindgen/tree/b9c93a3c24f25456c9867b4fe50ad19fb844a6f1/crates/js-sys
  * https://docs.rs/js-sys/0.3.32/js_sys/
* `web-sys = { vesrion = "0.3", features = ["console"] }` について
  * 今回は nodejs で動かすので `console` だけ有効にする
  * https://github.com/rustwasm/wasm-bindgen/blob/b9c93a3c24f25456c9867b4fe50ad19fb844a6f1/crates/web-sys/Cargo.toml#L1288
  * https://docs.rs/web-sys/0.3.32/web_sys/
* `wasm-bindgen-futures` について
  * `wasm_bindgen_futures::JsFuture` が生えてる
  * https://github.com/rustwasm/wasm-bindgen/tree/b9c93a3c24f25456c9867b4fe50ad19fb844a6f1/crates/futures
  * https://docs.rs/wasm-bindgen-futures/0.4.5/wasm_bindgen_futures/index.html

## setTimeout を Promise で包んで wasm rust に渡す

```rust:src/lib.rs
use serde::Deserialize;
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::JsFuture;
use js_sys::{Promise, Error};

#[wasm_bindgen(inline_js = "module.exports.sleep = function sleep(ms) { return new Promise((resolve)=> setTimeout(resolve, ms)); }")]
extern "C"  {
    fn sleep(ms: f64) -> Promise;
}

#[derive(Deserialize)]
pub struct Opt {
    pub count: u32,
    pub wait: f64,
}

#[wasm_bindgen]
pub async fn handler(opt: JsValue) -> Result<JsValue, JsValue> {
    let Opt{ count, wait } = opt.into_serde()
        .map_err(|err| JsValue::from(Error::new(&format!("{:?}", err))))?;

    for i in 0_u32..count {
        JsFuture::from(sleep(wait)).await?;
        web_sys::console::log_1(&format!("{}", i).into());
    }

    Ok(JsValue::undefined())
}
```

* `opt.into_serde()` について
  * https://rustwasm.github.io/wasm-bindgen/api/wasm_bindgen/struct.JsValue.html#method.into_serde
  * https://rustwasm.github.io/docs/wasm-bindgen/reference/arbitrary-data-with-serde.html
* `js_sys::Promise` について
  * https://docs.rs/js-sys/0.3.32/js_sys/struct.Promise.html
* `Closure` について
  * https://docs.rs/wasm-bindgen/0.2.55/wasm_bindgen/closure/struct.Closure.html
* `#[wasm_bindgen(inline_js = "...")]` について
  * wasm-bindgen の inline js を wasm の import 関数に割り当てる機能
  * 他に `#[wasm_bindgen(module = "aws-sdk-js")]` みたいに書くこともできる
  * https://rustwasm.github.io/docs/wasm-bindgen/contributing/design/importing-js.html
  * https://rustwasm.github.io/docs/wasm-bindgen/reference/attributes/on-js-imports/module.html
  * https://rustwasm.github.io/docs/wasm-bindgen/reference/attributes/on-js-imports/raw_module.html

## ビルド

```bash
wasm-pack build -t nodejs
```

* `wasm-pack build -t nodejs` について
  * nodejs は 8.0.0 あたりから WebAssembly に対応している
  * https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/WebAssembly
  * https://rustwasm.github.io/docs/wasm-pack/commands/build.html

## nodejs から呼ぶ

node v12.13.1

```js:example.js
const pkg = require("./pkg");
pkg.handler({count: 10, wait: 1000})
  .then(console.log)
  .catch(console.error);
```

出力

```
0
1
2
3
4
5
6
7
8
9
undefined
```

## sleep (setTimeout) 関数を wasm rust の引数として渡す

wasm の関数に 非同期 IO 関数を渡したい。
wasm への入出力には通常 wasm-bindgen の `serde-serialize` を使う。
しかしこれは一旦 JS オブジェクトを JSON 文字列に変換してから Rust の serde でデシリアライズしているため、 JS の関数を渡すことはできない。
一方で wasm-bindgen では JsValue (JS の any 値) が渡せるため、
型キャストすることで JsValue を Function として呼ぶことができる。

ここでは `impl TryInto<JsValue> for Response` と `impl TryFrom<JsValue> for Request` を実装して Rust の値と JsValue 値の変換できるようにした

```rust
use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use wasm_bindgen_futures::JsFuture;
use js_sys::{Promise, Error, Function, Reflect};
use web_sys::console;
use std::convert::{TryFrom, TryInto};

pub struct Request {
    pub count: u32,
    pub wait: f64,
    pub sleep: Box<dyn Fn(f64) -> Result<JsFuture, JsValue>>,
}

impl TryFrom<JsValue> for Request {
    type Error = JsValue;
    fn try_from(o: JsValue) -> Result<Self, Self::Error> {
        #[derive(Deserialize)]
        pub struct _Request {
            pub count: u32,
            pub wait: f64,
        }
        let sleep = {
            let cb = Reflect::get(&o, &JsValue::from("sleep"))?;
            if !Function::instanceof(&cb) {
                return Err(JsValue::from(Error::new("sleep is not function")));
            }
            Function::unchecked_from_js(cb)
        };
        let _req: _Request = o.into_serde()
            .map_err(|err| Error::new(&format!("{:?}", err)))?;
        Ok(Request{
            count: _req.count,
            wait: _req.wait,
            sleep: Box::new(move |ms|{
                let prm = sleep.call1(&JsValue::NULL, &JsValue::from(ms))?;
                if !Promise::instanceof(&prm) {
                    return Err(JsValue::from(Error::new("return value is not instanceof Promise")));
                }
                Ok(JsFuture::from(Promise::unchecked_from_js(prm)))
            })
        })
    }
}

pub struct Response {
}

impl TryInto<JsValue> for Response {
    type Error = JsValue;
    fn try_into(self) -> Result<JsValue, Self::Error> {
        #[derive(Serialize)]
        pub struct _Response {
        }
        let Response {} = self;
        JsValue::from_serde(&_Response{})
            .map_err(|err| JsValue::from(Error::new(&format!("{:?}", err))))
    }
}

#[wasm_bindgen]
pub async fn handler(req: JsValue) -> Result<JsValue, JsValue> {
    set_panic_hook();

    let Request{sleep, count, wait} = Request::try_from(req)?;

    for i in 0_u32..count {
        sleep(wait)?.await?;
        console::log_1(&JsValue::from(format!("{}", i)));
    }

    Response{}.try_into()
}

pub fn set_panic_hook() {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();
}
```

* `Reflect::get`, `Reflect::set` について
  * any な JsValue からプロパティ値の読み書きを試みる
  * https://rustwasm.github.io/docs/wasm-bindgen/reference/accessing-properties-of-untyped-js-values.html
  * https://rustwasm.github.io/wasm-bindgen/api/js_sys/Reflect/index.html

## JS 側から sleep 関数を wasm rust に渡してみる

```js
const pkg = require("./pkg");
pkg.handler({
    sleep(ms){ return new Promise(resolve=> setTimeout(resolve, ms)); },
    count: 10,
    wait: 1000
}).then(console.log).catch(console.error);
```

これでうまくうごく。

```
0
1
2
3
4
5
6
7
8
9
{}
```

## 非同期 IO ストリームを模した periodic (setInterval) 関数を wasm rust に渡す

sleep を導入することができたので次は非同期 IO ストリームを模して setInterval をラップしたこんな関数を渡すことを考えてみる

```js
const pkg = require("./pkg");
pkg.handler2({
    sleep(ms){ return new Promise(resolve => setTimeout(resolve, ms)); },
    periodic(ms, cb){
        let i = 0;
        let tid = setInterval(() => { cb(i++); }, ms);
        return ()=> clearInterval(tid);
    },
});
```

TypeScript ではこう

```ts
interface Request {
    sleep(wait: number): Promise<void>,
    periodic(wait: number, listener: (i: number)=> any): ()=> void,
}
```

この JS の引数オブジェクトの型は Rust での型はこんな感じになる

```rust
pub struct Request {
    pub sleep: Box<dyn Fn(f64) -> Result<JsFuture, JsValue>>,
    pub periodic: Box<dyn Fn(f64, Box<dyn FnMut(f64)>)
        -> Result<Box<dyn FnOnce() -> Result<JsValue, JsValue>>, JsValue>>,
}
```


例によって JsValue から Rust の型にコンバートする TryFrom を実装する

```rust
impl TryFrom<JsValue> for Request {
    type Error = JsValue;
    fn try_from(o: JsValue) -> Result<Self, Self::Error> {
        #[derive(Deserialize)]
        pub struct _Request {
        }
        let sleep = {
            let cb = Reflect::get(&o, &JsValue::from("sleep"))?;
            if !Function::instanceof(&cb) {
                return Err(JsValue::from(Error::new("sleep is not function")));
            }
            Function::unchecked_from_js(cb)
        };
        let periodic = {
            let cb = Reflect::get(&o, &JsValue::from("periodic"))?;
            if !Function::instanceof(&cb) {
                return Err(JsValue::from(Error::new("periodic is not function")));
            }
            Function::unchecked_from_js(cb)
        };
        let _req: _Request = o.into_serde()
            .map_err(|err| Error::new(&format!("{:?}", err)))?;
        Ok(Request{
            sleep: Box::new(move |ms|{
                let prm = sleep.call1(&JsValue::NULL, &JsValue::from(ms))?;
                if !Promise::instanceof(&prm) {
                    return Err(JsValue::from(Error::new("return value is not instanceof Promise")));
                }
                Ok(JsFuture::from(Promise::unchecked_from_js(prm)))
            }),
            periodic: Box::new(move |wait, cb|{
                let cb = Closure::wrap(Box::new(cb) as Box<dyn FnMut(f64)>);
                let stopfn = periodic.call2(&JsValue::NULL, &JsValue::from(wait), AsRef::<JsValue>::as_ref(&cb))?;
                if !Function::instanceof(&stopfn) {
                    return Err(JsValue::from(Error::new("return value is not instanceof Function")));
                }
                let stopfn = Function::unchecked_from_js(stopfn);
                Ok(Box::new(move ||{
                    let ret = stopfn.call0(&JsValue::NULL);
                    cb.forget();
                    ret
                }))
            })
        })
    }
}
```

* periodic を rust のクロージャに変換しているところ `let cb = Closure::<dyn FnMut(f64)>::new(cb);` で 作った cb を 最後 `cb.forget();` としているところに注意
  * ここでは JS の世界の setInterval の引数として Rust の `Box<dyn FnMut(f64)>` クロージャを wasm のヒープに置いて `wasm_bindgen::closure::Closure` で包んで渡している
  * このヒープ上の `Box<dyn FnMut(f64)>` クロージャを最後 forget させるのを忘れるとメモリリークになる
  * https://rustwasm.github.io/wasm-bindgen/reference/passing-rust-closures-to-js.html
  * https://docs.rs/wasm-bindgen/0.2.55/wasm_bindgen/closure/struct.Closure.html
* `js_sys::Function` の引数は `&JsValue` が要求されるので `wasm_bindgen::closure::Closure` を AsRef を使って `&JsValue` にアップキャストしている


ここまできれいにRust のコードに包むと使い勝手はふつうの Rust コードとほとんど変わらない

```rust
#[wasm_bindgen]
pub async fn handler(req: JsValue) -> Result<JsValue, JsValue> {
    set_panic_hook();

    let Request{periodic, sleep} = Request::try_from(req)?;

    let stop = periodic(100.0, Box::new(|i|{
        console::log_1(&JsValue::from(format!("{}", i)));
    }))?;

    sleep(3000.0)?.await?;

    stop()?;
    
    Response{}.try_into()
}
```

実行すると 3 秒間コンソールに数字が流れるようになる。


## 所感

* ここまで楽に書けるといよいよ実用できそうな気持ちがしてくる
* `aws-sdk-js` を `wasm-bindgen` で wasm rust の世界に取り込んで aws lambda nodejs v12 runtime で実行したりできると個人的に嬉しい
* wasm-bindgen での async の扱いの詳細はこちら
  * https://rustwasm.github.io/wasm-bindgen/reference/js-promises-and-rust-futures.html?highlight=async#return-values-of-async-fn



## その他メモ

* wasm-pack-template についてくる `wee_alloc` について
  * rust をそのまま wasm にするとデフォルトアロケータ `dlmalloc` (の移植版) のランタイムが wasm ファイルのうち 10 KB を消費してしまう
  * `wee_alloc` に切り替えると速度を犠牲に wasm のファイルサイズを削減できる
  * https://rustwasm.github.io/docs/wasm-pack/tutorials/npm-browser-packages/template-deep-dive/wee_alloc.html
  * https://rustwasm.github.io/docs/book/reference/code-size.html
  * https://github.com/rustwasm/wee_alloc
* wasm-pack-template についてくる `utils::set_panic_hook` と `console_error_panic_hook` について
  * wasm 内の rust のスタックトレースを記録しパニック時に `console.error` に吐くことができるようになる便利機能
  * https://rustwasm.github.io/docs/wasm-pack/tutorials/npm-browser-packages/template-deep-dive/src-utils-rs.html
  * https://github.com/rustwasm/console_error_panic_hook
* serde-wasm-bindgen について
  * wasm-bindgen 付属の `from_serde` , `to_serde` は JsValue を JSON 文字列を介して rust の世界に持ってきている
  * serde-wasm-bindgen を使うと JsValue から直接 rust の値への変換をよしなに試みるようになる
  * 実行速度は JSON 文字列を介する場合と比べてもケースバイケースで早くなったり遅くなったりするようだ
  * __関数のデシリアライズはできない__
  * https://github.com/cloudflare/serde-wasm-bindgen
  * https://docs.rs/serde-wasm-bindgen/0.1.3/serde_wasm_bindgen/index.html
