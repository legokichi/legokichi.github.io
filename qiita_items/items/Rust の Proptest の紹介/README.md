# Rust の Proptest の紹介

この記事は [Property-Based Testing Advent Calendar 2018](https://qiita.com/advent-calendar/2018/property-based-testing) の 13 日目の記事です。

これ→ https://github.com/AltSysrq/proptest/

型からテストを生成する quickcheck と違って具体的な値の範囲を設定できるのが特徴です。

## 0 から 9 の値の生成

```rust
#[macro_use]
extern crate proptest;

proptest! {
    #[test]
    fn test_addition(a in 0..10, b in 0..10) {
        println!("a:{}, b:{}", a, b);
        prop_assert!(a + b <= 18);
        // a:1, b:8
        // a:9, b:0
        // a:5, b:4
        // a:5, b:1
        // a:8, b:5
        // ...
    }
}
```

## 日付の生成

例えば 西暦0年から 3000 年までの日付のテストケースを 10 個を生成するにはこう書きます。
値の型も合わせて書けます

```rust
proptest! {
    // #[test] の前にテストの設定が書けます
    #![proptest_config(ProptestConfig {
        cases: 10,
        .. ProptestConfig::default()
    })]
    #[test]
    fn parses_date_back_to_original(y in 0u32..3000, m in 1u32..13, d in 1u32..32) {
        println!("{}-{}/{}", y, m, d);
        // 2089-2/26
        // 2743-9/7
        // 1331-4/9
        // 19-8/22
        // 617-5/30
        // 2562-9/2
        // 2867-3/4
        // 2629-10/5
        // 32-1/30
        // 2125-12/19
    }
}
```

## 型による生成

`std::u8::MIN` から `std::u8::MAX` までの値が生成されます

```rust
proptest! {
    #[test]
    fn addition_is_commutative(a: u8, b: u8) {
        println!("a:{}, b:{}", a, b);
        prop_assert_eq!(a as u16 + b as u16, b as u16 + a as u16);
        // a:144, b:204
        // a:199, b:67
        // a:61, b:137
        // a:9, b:246
        // a:217, b:63
        // a:254, b:41
        // a:102, b:49
        // a:228, b:58
        // a:120, b:77
        // a:180, b:206
        // a:80, b:238
        // a:214, b:10
        // a:27, b:60
    }
}
```


## 文字列の生成

String 型を指定すると任意長の文字列が生成されます。

```rust
proptest! {
    #[test]
    fn test_string_concat2(a in ".*", b: String) {
        println!("a:{}, b:{}", a, b);
        let cat = format!("{}{}", a, b);
        prop_assert_eq!(a.len() + b.len(), cat.len());
        // a:
        // '%C<*3/𻰵       ¥?�򶦳wNW:uXѨෆ
        // a:§<, b:㇃.$𛰐.<𝕆𑵠શ&𐣴Lι¥"
        // a:=Ѩ򁊤~)﻿򯔻𨠊`$﻿𞟰, b:5.2�~ⷍ,ꡄë⃡
        // a:󳦦<?򊲋*u%, b:Cో\ই𒑈ೌ🕴
        // a:񵧘
        //    򥰡)*\򤴗b﻿󢪏/�Ѩo&iz󦝋<0O)x𿋄?$&g, b:>ે={𑚐ಿ𖿡𐼱
        // a:'{w"𵱭񙎯%󉸇<ùª, b:"<)፶𝍫[ැ.𐊲ල/ὐ\=%T𑄿OH
    }
}
```


文字列の生成には正規表現も使えます。

```rust
proptest! {
    #[test]
    fn test_string_concat(a in ".*", b in ".*") {
        println!("a:{}, b:{}", a, b);
        let cat = format!("{}{}", a, b);
        prop_assert_eq!(a.len() + b.len(), cat.len());
        // a:L򙼳RH�񷡯񥝎򜱇.$�*$x󡡙󄸑?, b:𐋈Ѩ.︖
        // N/, b:/0U&\𐤦/፸{\°鲙/🧀aꬌ{Ὄ0ᰡ%.5
        // a:񠼯$$�򨛓%ѨT8^Kl¥So<
        //                 , b:l#.ῳ𐊉ඃ𗫸{Ὀ'
        // a:񗜸ë"ñ𴐨=t=*&򗨭|{YW¥$`{<'{𑂸.􀧬{"Ѩ z, b:C'@?5𐄳=ڼ3?\o⿲ⴑ¥𐨺🦲
        // 􂘰., b:$᳆xணⶥ,᪳Itﷁ 𑋷ￏ𐣵꣏xcm?WූU𐨥$ⶭ*
        // a:+%<\<򧎍򬽶\'!:󎟪M🕴{🕴i🕴&y.󬅟, b:<🕴rjc𞹋mRෟq{֎ꬃ'𑌪q{{=²$
        // a:𹂟𽏊{󳫒?p,*$?r<", b:
        // OѨ&񤊤'񻲖}񴳋.'﻿."    E.񁟍%LE%=k³$\�򢹜b/򁅃󘏾\, b:𐁒mږ{¥`uລ
        // a:򄈴"Ѩ$*'%, b:"ﵜ𑌳\
        // a:B=/&🕴򀣷�󆉝¥":Y  kL, b:$¥\?caP'¥0/&"r︙Ὓ
        // ...
    }
}
```

日付っぽい文字列

```rust
proptest! {
    #[test]
    fn parses_all_valid_dates(s in "[0-9]{4}-[0-9]{2}-[0-9]{2}") {
        println!("{}-{}/{}", y, m, d);
        // 1667-77-67
        // 5076-51-20
        // 4487-07-47
        // 3281-25-92
        // 2818-71-60
        // 5361-52-97
        // 0196-34-59
        // 9202-17-16
        // 8864-17-26
    }
}
```

任意の非制御文字で構成された任意の文字列

```rust
proptest! {
    #[test]
    fn doesnt_crash(s in "\\PC*") {
        println!("{}", s);
        // %'úయ$︨Ὓn'{𑘴<ᬍ/:𐤿({🕴𐡇{=ZѨ𑒸ா𞸻˱༣:I
        // F`ﬗw½:`𑆔6
        // u
        // 🉢࿈6'🦴𐨗𐠸ಸ|
        // lୈ﹩?/7𖭟qt
        // ?}&`I�`சퟀ'f;្
        // yE$/¥=`�ׯె=𑴡=𫮓7
        // ລ"{ೖ꧳ೲx{%%7hך
        // 9v^/ⷃ`
        // L6
    }
}
```

## テストのタイムアウト

テストはデフォルトの設定でフォークしたスレッドで実行されます。
これによりテストにタイムアウトを設定できます。
このフィボナッチ数関数はテストのタイムアウト、スタックオーバーフロー、または u64 のオーバーフローによってパニックしテストが失敗します。

```rust
fn fib(n: u64) -> u64 {
    if n <= 1 {
        n
    } else {
        fib(n - 1) + fib(n - 2)
    }
}

proptest! {
    #![proptest_config(ProptestConfig {
        fork: true,
        timeout: 1000,
        .. ProptestConfig::default()
    })]
    #[test]
    fn test_fib(n: u64) {
        assert!(fib(n) >= n);
    }
}
```

デフォルトのタイムアウトは環境変数でも渡せます

```console
PROPTEST_FORK=true cargo test
PROPTEST_TIMEOUT=1000 cargo test
```

`PROPTEST_TIMEOUT` を設定したら自動的にフォークされるので設定するのは `fork` か `timeout` のどちらかで良いです。

## ひとつのテストの中で複数のテストケースを作る

ひとつのテストの中で複数のテストケースを生成することもできます。
テストのたびに重い初期化処理をしなくてもよくなるため便利です。
ただしこの方法で作られたテストではテストのタイムアウトをすることができません。

```rust
#[test]
fn my_test() {
    // 環境変数を読んだり
    dotenv::dotenv().unwrap();
    env_logger::try_init().ok();
    #[derive(Clone, Debug, Deserialize)]
    struct Config {
        database_url: String,
    }
    let config = envy::from_env::<Config>().unwrap();

    // データベースを初期化したりできます
    let db = Db::new(&config.database_url).unwrap();

    // マクロの都合クロージャの引数はタプルで渡す必要があります
    proptest!(|(x in 0u32..42u32, y in 1000u32..100000u32)| {
        println!("x:{}, y:{}", x, y);
    });

    // move クロージャも使えます
    proptest!(move |(x in 0u32..42u32)| {
        println!("x:{}", x);
    });

    // マクロの第一引数にテストの設定を渡せます
    proptest!(ProptestConfig::with_cases(1), |(x: i32)| {
        println!("x:{}", x);
    });
}
```

## 失敗したテストケースの保存

`proptest-regressions` というファイルに失敗したテストケースが保存されます

```:tests/test.proptest-regressions
# Seeds for failure cases proptest has generated in the past. It is
# automatically read and these particular cases re-run before any
# novel cases are generated.
#
# It is recommended to check this file in to source control so that
# everyone who runs the test benefits from these saved cases.
xs 1701150751 4032634173 2240973666 2150442205 # shrinks to all_data = -1, offset = 0, limit = 0
xs 284604027 460482942 1202867123 218470716 # shrinks to all_data = 0, offset = 9223372036854775809, limit = 1
```

## Strategy によるカスタムテストケースの生成

https://altsysrq.github.io/rustdoc/proptest/latest/proptest/index.html#strategy-basics

`proptest!` マクロの中では次のようにテストケースを生成しています

```rust
extern crate proptest;

use proptest::test_runner::TestRunner;
use proptest::strategy::{Strategy, ValueTree};

fn some_function(v: i32) {
    // Do a bunch of stuff, but crash if v > 500
    assert!(v <= 500);
}

#[test]
fn some_function_doesnt_crash() {
    let mut runner = TestRunner::default();
    for _ in 0..256 {
        let val = (0..10000i32).new_tree(&mut runner).unwrap();
        some_function(val.current());
    }
}
```

## Shrinking による最小限の失敗テストケースの探索

https://altsysrq.github.io/rustdoc/proptest/latest/proptest/#shrinking-basics

次の例は失敗するとわかっている関数に関してより単純なテストケースを探索するプログラムです。
`simplify` によって生成したテストケースをより単純なものへと縮小していき、
`complicate` でより複雑なものへと拡大していくことで、
最低限の失敗するテストケースを探索することができます。

```rust
extern crate proptest;

use proptest::test_runner::TestRunner;
use proptest::strategy::{Strategy, ValueTree};

// この some_function は 501 以上の値を入力すると失敗する関数だとします。
// ある失敗するとわかっているテストに関して、
// テストケースの成功と失敗を bool で表すことにより、
// より単純なケースを探索します
fn some_function(v: i32) -> bool {
    v <= 500
}

fn main() {
    let mut runner = TestRunner::default();

    // 256 回探索を試みる
    for _ in 0..256 {
        // 10000i32 から値を絞り込みます。この場合は 10000 から 0 へと絞り込んでいきます
        let mut val = (0..10000i32).new_tree(&mut runner).unwrap();
        if some_function(val.current()) {
            // このテストケースは成功したので他に失敗するテストケースを探す
            continue;
        }
        // some_function が false を返したのでよ単純なテストケースを探索する
        loop {
            if !some_function(val.current()) {
                // テストが落ちたのでより単純なケースを見つけた
                if !val.simplify() {
                    // これ以上単純化できない
                    break;
                }
            } else {
                // この単純化したテストは成功してしまったのでもう少し複雑なテストケースを生成します
                if !val.complicate() {
                    break;
                }
            }
        }

        println!("The minimal failing case is {}", val.current());
        assert_eq!(501, val.current());
        return;
    }
    panic!("Didn't find a failing test case");
}
```

## パニックハンドラ

https://altsysrq.github.io/rustdoc/proptest/latest/proptest/#using-the-test-runner

proptest の TestRunner は `Result<_, TestError>` またはパニックを失敗として取り扱うことができます

## カスタム戦略を作る

任意の i8 相当の String を生成

```rust
proptest!{
    #[test]
    fn test(o in any::<i8>().prop_map(|v| v.to_string())){
        println!("{:?}", o);
        // "-65"
        // "-119"
        // "43"
        // "-56"
        // "65"
        // "-48"
        // "22"
    }
}
```

構造体

```rust
#[derive(Clone, Debug)]
struct Order {
  id: String,
  item: String,
  quantity: u32,
}

fn gen_order() -> impl Strategy<Value = Order>{
    (any::<u32>().prop_map(|v| v.to_string()),
        "[a-z]*", 1..1000u32).prop_map(
            |(id, item, quantity)| Order { id, item, quantity })
}

proptest!{
    #[test]
    fn test_order(o in gen_order()){
        println!("{:?}", o);
        // Order { id: "3372296639", item: "yybosjbnvryotdoeglcmpxia", quantity: 383 }
        // Order { id: "2833625993", item: "deshvxgsqywccnllzjasffgfpqlbzlo", quantity: 486 }
        // Order { id: "3260047346", item: "hhhsylstdlxotrjrvj", quantity: 256 }
        // Order { id: "3833458271", item: "pbmrvxbguybyvwjzod", quantity: 305 }
        // Order { id: "3864500675", item: "idmugc", quantity: 454 }
        // Order { id: "2807396473", item: "jjnbygsbrjkkgzyrdtadhslmxllie", quantity: 701 }
        // Order { id: "3619398342", item: "flcqcwuimcqgiajtmzev", quantity: 760 }
        // Order { id: "2618995400", item: "wpbcsnhwpauvysjlxtbakiimj", quantity: 375 }
        // Order { id: "3882359522", item: "tzfwrudqwko", quantity: 486 }
        // Order { id: "407582390", item: "mnxpkntwukoh", quantity: 221 }
    }
}
```


`Option<i8>` のような列挙型は `prop_oneof!` マクロを使う。
リストの一番後ろを一番複雑なケースにして、手前ほど簡単なケースにするとうまく shrinking をやってくれる - https://altsysrq.github.io/rustdoc/proptest/latest/proptest/#generating-enums

```rust

fn gen_opt() -> impl Strategy<Value = Option<i8>> {
    prop_oneof![
        Just(None),
        any::<i8>().prop_map(|i| Some(i)),
    ]
}

proptest!{
    #[test]
    fn opt_test(o in gen_opt()){
        println!("{:?}", o);
        // None
        // Some(24)
        // None
        // Some(-89)
        // None
        // None
        // None
        // Some(-54)
        // None
    }
}
```


## 高階ストラテジー
任意長のの Vec と、そのインデックスの usize が欲しい場合、まず任意長のが Vec を作ってから、 その len の範囲内でインデックスを生成すると便利です

```rust
fn vec_and_index() -> impl Strategy<Value = (Vec<u8>, usize)> {
    prop::collection::vec(any::<u8>(), 1..10)
        .prop_flat_map(|vec| {
            let len = vec.len();
            (Just(vec), 0..len)
        })
}

proptest!{
    #[test]
    fn vec_test(o in gen_opt()){
        println!("{:?}", o);
        // ([155, 132, 216, 101, 111, 103, 141, 154], 6)
        // ([24, 240], 1)
        // ([105, 54, 102, 54, 187, 32, 252], 6)
        // ([252, 88, 150, 52, 105, 102, 7, 152], 0)
        // ([218, 143, 62, 85, 194, 73, 57], 5)
        // ([45, 145, 182, 93, 225, 140], 1)
        // ([254], 0)
        // ([123, 80, 121, 19, 228, 48, 134], 4)
        // ([181, 37], 1)
        // ([214, 251, 140, 179, 227, 126], 0)
        // ([154, 203, 139, 20, 83, 15], 0)
        // ([14], 0)
    }
}
```

## その他の機能

ここが詳しいです ~~https://altsysrq.github.io/rustdoc/proptest/latest/proptest/~~ → https://altsysrq.github.io/proptest-book/intro.html

