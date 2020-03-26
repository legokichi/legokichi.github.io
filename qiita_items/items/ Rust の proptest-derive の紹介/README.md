前回紹介した [Rust の Proptest の紹介](https://qiita.com/legokichi/items/a2f98e99dfcb7536dde9) では、struct や enum の値を生成する Arbitrary を作るのが面倒でした。
しかし先月更新された proptest 0.9 から [proptest-derive が導入され、 struct や enum の値を簡単に生成できるようになりました](https://altsysrq.github.io/proptest-book/proptest-derive/index.html) 。
その簡単な紹介をします。


## 例

JSON にシリアライズできる Foo と Bar という型があったとします。

```rust
// Arbitrary を derive できるようになりました
#[derive(Arbitrary, Serialize, Deserialize, Clone, Debug)]
pub struct Foo {
    pub bar: Bar,

    #[proptest(strategy = "crate::arb_datetime()")]
    pub rfc3339: DateTime<Utc>,

    #[proptest(strategy = "crate::arb_datetime()")]
    #[serde(with = "::chrono::serde::ts_milliseconds")]
    pub unix_millis: DateTime<Utc>,

    #[proptest(strategy = "crate::arb_datetime()")]
    #[serde(with = "::chrono::serde::ts_seconds")]
    pub unix_micros: DateTime<Utc>,

    #[proptest(strategy = "crate::arb_url()")]
    #[serde(with = "::url_serde")]
    pub url: Url,

    #[proptest(strategy = "crate::arb_uuid()")]
    pub uuid: Uuid,

    #[proptest(strategy = "prop::collection::vec(proptest::num::u8::ANY, 0..2)")]
    #[serde(with = "Base64Standard")]
    pub buffer: Vec<u8>,

    #[proptest(strategy = "crate::arb_json(4)")]
    pub json: serde_json::Value,

    #[proptest(strategy = "prop::collection::vec(crate::arb_json(4), 0..2)")]
    pub jsons: Vec<serde_json::Value>,

    #[proptest(strategy = "crate::arb_regex()")]
    #[serde(with = "::serde_regex")]
    pub pattern: Regex,
}

// enum にも Arbitrary を derive できます
#[derive(Arbitrary, Serialize, Deserialize, Clone, Debug)]
#[serde(tag = "type", content = "value")]
pub enum Bar {
    A(String),
    B(u8),
    C((u8, u16)),
    D(bool),
}
```

ここで、 `serde_json::Value`, `Url`, `Uuid`, `DateTime<Utc>`, `Regex` 等の特殊な型に関しては Strategy を定義しておきます。


```rust
fn arb_datetime() -> impl Strategy<Value = ::chrono::DateTime<::chrono::Utc>> {
    Just(::chrono::Utc::now())
}

fn arb_url() -> impl Strategy<Value = ::url::Url> {
    Just("https://example.com/".parse().unwrap())
}

fn arb_uuid() -> impl Strategy<Value = ::uuid::Uuid> {
    Just(::uuid::Uuid::new_v4())
}

// 再帰的な JSON 型
fn arb_json(depth: u32) -> impl Strategy<Value = ::serde_json::Value> {
    let leaf = prop_oneof![
        Just(::serde_json::Value::Null),
        any::<bool>().prop_map(|o| serde_json::to_value(o).unwrap()),
        any::<f64>().prop_map(|o| serde_json::to_value(o).unwrap()),
        ".*".prop_map(|o| serde_json::to_value(o).unwrap()),
    ];
    leaf.prop_recursive(
        depth,   // n levels deep
        256, // Shoot for maximum size of 256 nodes
        10,  // We put up to 10 items per collection
        |inner| {
            prop_oneof![
                // Take the inner strategy and make the two recursive cases.
                prop::collection::vec(inner.clone(), 0..10)
                    .prop_map(|o| serde_json::to_value(o).unwrap()),
                prop::collection::hash_map(".*", inner, 0..10)
                    .prop_map(|o| serde_json::to_value(o).unwrap()),
            ]
        },
    )
}

fn arb_regex() -> impl Strategy<Value = ::regex::Regex> {
    Just(Regex::new(r"(\d{4})-(\d{2})-(\d{2})").unwrap())
}
```

テストを書いて（といってもこのテストは何もしていませんが）実行してみましょう

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        proptest!(|(foo: Foo)| {
            dbg!(&foo);
            let foo_json = serde_json::to_string_pretty(&foo).unwrap();
            println!("{}", &foo_json);
        });
    }
}
```

## テストケース出力例

```console
$ cargo test -- --nocapture

...

[src/main.rs:98] &foo = Foo {
    bar: B(
        64
    ),
    rfc3339: 2019-03-16T10:49:06.216534201Z,
    unix_millis: 2019-03-16T10:49:06.216537367Z,
    unix_micros: 2019-03-16T10:49:06.216537833Z,
    url: "https://example.com/",
    uuid: Uuid {
        bytes: [
            63,
            123,
            73,
            99,
            187,
            156,
            68,
            112,
            175,
            109,
            115,
            253,
            52,
            167,
            60,
            160
        ]
    },
    buffer: [
        217
    ],
    json: Array(
        [
            Object(
                {
                    "%\\r,\'{\u{5e480}\u{feff}F?\u{5}+\u{a0e71}â.\t\u{0}\t\u{feff}<Zy\u{7ec15}Ⱥ$": Number(
                        8016288550892050000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.0
                    ),
                    "{X\u{feff}¥\u{5}k\"\u{b56fa}%ѨÓ\u{3e458}\u{71736}{\u{0}Q\tð": Bool(
                        true
                    ),
                    "\u{a38c3}`\u{8298b}.\u{7977d}\u{77b90}\u{7f}\u{0}*/\u{b}\u{44b4c}%\u{1}\u{cb38d}🕴<aG.\u{3b6fc}Ⱥ聝": String(
                        "$\u{8}\u{202e}`k\t-\u{7f}𡁚`\\Y�<\u{be27b}ì%Hü\r/"
                    )
                }
            ),
            Number(
                -1220054938564386000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.0
            ),
            Array(
                [
                    Null,
                    String(
                        "ѨY\u{202e}\u{1efa2}\u{6}N\u{7f}==㪙`\"`\u{0}\u{41c4b}\u{942a4}@O>&=m\u{edde6}"
                    ),
                    Bool(
                        true
                    ),
                    Number(
                        -0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000005238922386438217
                    ),
                    Null,
                    Bool(
                        false
                    ),
                    Bool(
                        true
                    ),
                    String(
                        "*\u{7f}\u{b38ad}o:M\u{5143a}\u{104091}\"\u{1b}^\u{202e}\u{e0e88}Ⱥ🕴🕴»?!"
                    ),
                    Array(
                        [
                            Number(
                                -0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000046545058226182346
                            ),
                            Number(
                                -0.00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001309919205043035
                            ),
                            Bool(
                                true
                            ),
                            Bool(
                                false
                            ),
                            Number(
                                5703267498246020000000000000000000000000000000000000000000000000000000000000000000000.0
                            ),
                            String(
                                "Ѩ$T*a\u{0}$\u{ce7ff}¥\u{1da2f}𐅗\u{202e}\u{ed80d}¥�:\u{52bd9}\u{1b}\\\u{332c9}Ⱥ\u{b46b1}\u{cce8b}æ🕴&Ѩ\u{b}=\\pz"
                            ),
                            Null,
                            Null,
                            Null
                        ]
                    )
                ]
            ),
            Object(
                {
                    "\u{6}\u{f6078}à\u{ae4e1}<=🕴gºꡥ\u{c693d}%r\tk": String(
                        "$\u{7}&\u{c99cd}]婷{hk`棋�\u{ed251}d�T\u{fbd2d}�\u{b8156}&\"0!%"
                    ),
                    "\rRG\u{0}\u{beebb}<X>\\\u{cb6d9}h\t\u{3a583}{\"/𗿋$`": Bool(
                        false
                    ),
                    "m;nò1r𨀟:\'\u{e98cf}%\t?𣑢i{\\\u{e2379}H\u{b}\u{9310a}7/q\u{b}\u{3}": Number(
                        -0.0000000000000000000000000000005653934938866869
                    ),
                    "\u{feff}*{\u{feff}\u{6a95c}yE\u{b}{\u{1b}\t¥\u{202e}": Number(
                        -0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000013908092603567689
                    ),
                    "\u{107306}Ѩ\u{3}": Bool(
                        false
                    )
                }
            ),
            Array(
                [
                    String(
                        ":\u{f3b98}🕴.g\u{8a810}\u{1b}D7\u{85313}\'\u{825e5}\u{56353}á"
                    ),
                    Bool(
                        true
                    )
                ]
            ),
            Object(
                {
                    "$": Number(
                        -0.0
                    ),
                    "/\u{7f}=N&%i[𧋝`/$\u{0}:\\\u{0}": Null,
                    "\u{7f}%\u{e954a}\"%\u{77809}\u{202e}{õ\u{10582b}\u{b}\t\u{0}": Number(
                        -0.0000000000000000000000000000000000000000000000000000000000000000000000000020098450520050888
                    )
                }
            )
        ]
    ),
    jsons: [
        Array(
            []
        )
    ],
    pattern: (\d{4})-(\d{2})-(\d{2})
}
{
  "bar": {
    "type": "B",
    "value": 64
  },
  "rfc3339": "2019-03-16T10:49:06.216534201Z",
  "unix_millis": 1552733346216,
  "unix_micros": 1552733346,
  "url": "https://example.com/",
  "uuid": "3f7b4963-bb9c-4470-af6d-73fd34a73ca0",
  "buffer": "2Q==",
  "json": [
    {
      "%\\r,'{񞒀﻿F?\u0005+򠹱â.\t\u0000\t﻿<Zy񾰕Ⱥ$": 8.01628855089205e108,
      "{X﻿¥\u0005k\"򵛺%ѨÓ𾑘񱜶{\u0000Q\tð": true,
      "򣣃`򂦋.񹝽񷮐\u0000*/\u000b񄭌%\u0001󋎍🕴<aG.𻛼Ⱥ聝": "$\b‮`k\t-𡁚`\\Y�<򾉻ì%Hü\r/"
    },
    -1.220054938564386e126,
    [
      null,
      "ѨY‮𞾢\u0006N==㪙`\"`\u0000񁱋򔊤@O>&=m󭷦",
      true,
      -5.238922386438217e-199,
      null,
      false,
      true,
      "*򳢭o:M񑐺􄂑\"\u001b^‮󠺈Ⱥ🕴🕴»?!",
      [
        -4.6545058226182346e-119,
        -1.309919205043035e-308,
        true,
        false,
        5.70326749824602e84,
        "Ѩ$T*a\u0000$󎟿¥𝨯𐅗‮󭠍¥�:񒯙\u001b\\𳋉Ⱥ򴚱󌺋æ🕴&Ѩ\u000b=\\pz",
        null,
        null,
        null
      ]
    ],
    {
      "\u0006󶁸à򮓡<=🕴gºꡥ󆤽%r\tk": "$\u0007&󉧍]婷{hk`棋�󭉑d�T󻴭�򸅖&\"0!%",
      "\rRG\u0000򾺻<X>\\󋛙h\t𺖃{\"/𗿋$`": false,
      "m;nò1r𨀟:'󩣏%\t?𣑢i{\\󢍹H\u000b򓄊7/q\u000b\u0003": -5.653934938866869e-31,
      "﻿*{﻿񪥜yE\u000b{\u001b\t¥‮": -1.3908092603567689e-161,
      "􇌆Ѩ\u0003": false
    },
    [
      ":󳮘🕴.g򊠐\u001bD7򅌓'򂗥񖍓á",
      true
    ],
    {
      "$": -0.0,
      "/=N&%i[𧋝`/$\u0000:\\\u0000": null,
      "%󩕊\"%񷠉‮{õ􅠫\u000b\t\u0000": -2.0098450520050888e-75
    }
  ],
  "jsons": [
    []
  ],
  "pattern": "(\\d{4})-(\\d{2})-(\\d{2})"
}
...
```

簡単にテストケースが生成できるようになりました。

## 所感

JSON Schema validator の [rustless/valico](https://github.com/rustless/valico) などと組み合わせると RESTfull API のテストが捗りそうですね。

## 参考

* Rust の Proptest の紹介 - https://qiita.com/legokichi/items/a2f98e99dfcb7536dde9
* crates.io - https://crates.io/crates/proptest
* github - https://github.com/AltSysrq/proptest/
* proptest のマニュアル - https://altsysrq.github.io/proptest-book/intro.html
* proptest-derive のリファレンス - https://altsysrq.github.io/proptest-book/proptest-derive/modifiers.html
* rustdoc - https://altsysrq.github.io/rustdoc/proptest/latest/proptest/


## 付録. サンプルコード全体

```toml:Cargo.toml
[package]
name = "proptest-sandbox"
version = "0.1.0"
edition = "2018"

[dependencies]
base64 = "0.10"
base64-serde = "0.3"
chrono = { features = ["serde"], version = "0.4" }
dotenv = "0.13"
env_logger = "0.6"
envy = "0.3"
failure = "0.1"
log = "0.4"
proptest = "0.9"
proptest-derive = "0.1"
regex = "1.0"
serde = "1"
serde_derive = "1"
serde_json = "1"
serde_regex = "0.3"
url = "1.7"
url_serde = "0.2"
uuid = { features = ["serde", "v4"], version = "0.6" }

```

```rust:src/lib.rs
#![allow(clippy::unit_arg)]

use base64::STANDARD;
use base64_serde::base64_serde_type;
use chrono::prelude::*;
use proptest::prelude::*;
use proptest_derive::*;
use regex::Regex;
use serde_derive::*;
use url::Url;
use uuid::Uuid;

base64_serde_type!(Base64Standard, STANDARD);

fn arb_datetime() -> impl Strategy<Value = ::chrono::DateTime<::chrono::Utc>> {
    Just(::chrono::Utc::now())
}

fn arb_url() -> impl Strategy<Value = ::url::Url> {
    Just("https://example.com/".parse().unwrap())
}

fn arb_uuid() -> impl Strategy<Value = ::uuid::Uuid> {
    Just(::uuid::Uuid::new_v4())
}

fn arb_json(depth: u32) -> impl Strategy<Value = ::serde_json::Value> {
    let leaf = prop_oneof![
        Just(::serde_json::Value::Null),
        any::<bool>().prop_map(|o| serde_json::to_value(o).unwrap()),
        any::<f64>().prop_map(|o| serde_json::to_value(o).unwrap()),
        ".*".prop_map(|o| serde_json::to_value(o).unwrap()),
    ];
    leaf.prop_recursive(
        depth, // n levels deep
        256,   // Shoot for maximum size of 256 nodes
        10,    // We put up to 10 items per collection
        |inner| {
            prop_oneof![
                // Take the inner strategy and make the two recursive cases.
                prop::collection::vec(inner.clone(), 0..10)
                    .prop_map(|o| serde_json::to_value(o).unwrap()),
                prop::collection::hash_map(".*", inner, 0..10)
                    .prop_map(|o| serde_json::to_value(o).unwrap()),
            ]
        },
    )
}
fn arb_regex() -> impl Strategy<Value = ::regex::Regex> {
    Just(Regex::new(r"(\d{4})-(\d{2})-(\d{2})").unwrap())
}

#[derive(Arbitrary, Serialize, Deserialize, Clone, Debug)]
struct Foo {
    pub bar: Bar,
    #[proptest(strategy = "crate::arb_datetime()")]
    pub rfc3339: DateTime<Utc>,
    #[proptest(strategy = "crate::arb_datetime()")]
    #[serde(with = "::chrono::serde::ts_milliseconds")]
    pub unix_millis: DateTime<Utc>,
    #[proptest(strategy = "crate::arb_datetime()")]
    #[serde(with = "::chrono::serde::ts_seconds")]
    pub unix_micros: DateTime<Utc>,
    #[proptest(strategy = "crate::arb_url()")]
    #[serde(with = "::url_serde")]
    pub url: Url,
    #[proptest(strategy = "crate::arb_uuid()")]
    pub uuid: Uuid,
    #[proptest(strategy = "prop::collection::vec(proptest::num::u8::ANY, 0..2)")]
    #[serde(with = "Base64Standard")]
    pub buffer: Vec<u8>,
    #[proptest(strategy = "crate::arb_json(4)")]
    pub json: serde_json::Value,
    #[proptest(strategy = "prop::collection::vec(crate::arb_json(4), 0..2)")]
    pub jsons: Vec<serde_json::Value>,
    #[proptest(strategy = "crate::arb_regex()")]
    #[serde(with = "::serde_regex")]
    pub pattern: Regex,
}

#[derive(Arbitrary, Serialize, Deserialize, Clone, Debug)]
#[serde(tag = "type", content = "value")]
pub enum Bar {
    A(String),
    B(u8),
    C((u8, u16)),
    D(bool),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        proptest!(|(foo: Foo)| {
            dbg!(&foo);
            let foo_json = serde_json::to_string_pretty(&foo).unwrap();
            println!("{}", &foo_json);
        });
    }
}

```

```
rustc 1.33.0 (2aa4c46cf 2019-02-28)
```
