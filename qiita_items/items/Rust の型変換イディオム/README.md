# Rust の型変換イディオム

この記事は [Rustその2 Advent Calendar 2018](https://qiita.com/advent-calendar/2018/rust2) の 12 日目の記事です。


## `Option<String>` to `&Option<&str>`

* https://stackoverflow.com/questions/31233938/converting-from-optionstring-to-optionstr

```rust
let a: Option<String> = Some("0".to_string());
let b: Option<&str> = a.as_ref().map(AsRef::as_ref);
let c: &Option<&str> = &b;
```

## `Vec<String>` to `&[&str]`

```rust
let a: Vec<String> = vec!["0".to_string()];
let b: Vec<&str> = a.iter().map(AsRef::as_ref).collect();
let c: &[&str] = b.as_ref();
```

## `Option<Vec<String>>` -> `&Option<&[&str]>`

```rust
let a: Option<Vec<String>> = Some(vec!["0".to_string()]);
let b: Option<&[String]> = a.as_ref().map(AsRef::as_ref);
let c: Option<Vec<&str>> = b.map(|lst| lst.iter().map(|s| s.as_ref()).collect::<Vec<_>>());
let d: Option<&[&str]> = c.as_ref().map(|lst| lst.as_ref());
```

```rust
let a: Option<String> = Some("0".to_owned());
let b: Option<&String> = a.as_ref();
let c: Option<&str> = b.map(|x| &**x);
let d: &Option<&str> = &c; //   ||||
                                |||+ x: &String
                                ||+ *x: String
                                |+ **x: str:  (*<String as Deref<Target = str>>::deref(*x))
                                + &**x: &str 
```

## `Option<T>` to `Option<&T>`

```rust
let a: Option<i32> = Some(0);
let b: Option<&i32> = a.as_ref();
```

## `Result<T, E>` to `Result<&T, &E>`

```rust
let a: Result<i32, ()> = Ok(0);
let b: Result<&i32, &()> = a.as_ref();
```

## `String` -> `&str`

```rust
let a: String = "0".to_string();
let b: &str = &a.to_string()[..])?;
```

```rust
let a: String = "0".to_string();
let b: &str = &*a;
```

## `Result<T, E>` -> `Option<T>`

```rust
let a: Result<i32, ()> = Ok(0);
let b: Option<i32> = a.ok();
```

## `Result<T, E>` -> `Option<E>`

```rust
let a: Result<i32, ()> = Ok(0);
let b: Option<()> = a.err();
```

## `Option<T>` -> `Result<T, E>`

```rust
let a: Option<i32> = Some(0);
let b: Result<i32, ()> = a.ok_or(());
```

```rust
let a: Option<i32> = Some(0);
let b: Result<i32, ()> = a.ok_or_else(|| ());
```

## `Option<T>` -> `Vec<T>`

```rust
let a: Option<i32> = Some(0);
let b: Vec<i32> = a.into_iter().collect::<Vec<_>>();
```

## `Option<T>` -> `Vec<&T>`

```rust
let a: Option<i32> = Some(0);
let b: Vec<&i32> = a.iter().collect::<Vec<_>>();
```

## `Vec<T>` -> `Option<T>`

```rust
let a: Vec<i32> = vec![0];
let b: Option<i32> = a.into_iter().next();
```

## `Vec<T>` -> `Option<&T>`

```rust
let a: Vec<i32> = vec![0];
let b: Option<i32> = a.first();
```

## `Option<Option<T>>` -> `Option<T>`

```rust
let a: Option<Option<i32>> = Some(Some(0));
let b: Option<i32> = a.and_then(|opt| opt);
```

## `Result<Option<T>, E>` -> `Option<Result<T, E>>`
* nightly だと transpose が使える - https://doc.rust-lang.org/std/option/enum.Option.html#method.transpose

```rust
let a: Result<Option<i32>, ()> = Ok(Some(0));
let b: Option<Result<i32, ()>> = match a {
    Ok(Some(x)) => Some(Ok(x)),
    Ok(None) => None,
    Err(e) => Some(Err(e)),
};
```

## `Option<Result<T, E>>` -> `Result<Option<T>, E>`
* nightly だと transpose が使える - https://doc.rust-lang.org/std/option/enum.Option.html#method.transpose

```rust
let a: Option<Result<i32, ()>> = Some(Ok(0));
let b: Result<Option<i32>, ()> = match self {
    Some(Ok(x)) => Ok(Some(x)),
    Some(Err(e)) => Err(e),
    None => Ok(None),
};
```

## `Vec<Result<T, E>>` -> `Result<Vec<T>, E>`

```rust
let a: Vec<Result<i32, ()>> = vec![Ok(0), Err(())];
let b: Result<Vec<i32>, ()> = a.into_iter().collect();
assert_eq!(b, Err(()));
```

## `Vec<Option<T>>` -> `Option<Vec<T>>`

```rust
let a: Vec<Option<i32>> = vec![Some(0), None];
let b: Option<Vec<i32>> = a.into_iter().collect();
assert_eq!(b, None);
```

## `&str` -> `&[u8]`

```rust
let a: &str = "0";
let b: &[u8] = a.as_bytes();
```

## `String` -> `Vec<u8>`

```rust
let a: String = "0".to_string();
let b: Vec<u8> = a.into_bytes();
```

## `&[u8]` -> `&str`

```rust
let a: &[u8] = &[48];
let b: &str = std::str::from_utf8(a).unwrap();
assert_eq!("0", b);
```

## `Vec<u8>` -> `String`

```rust
let a: Vec<u8> = vec![48];
let b: String = String::from_utf8(a).unwrap();
assert_eq!("0".to_string(), b);
```

## `String`, `&str` -> `u8`, `u16`, `u32` `u64`, `usize`, `i8`, `i16`, `i32`, `i64`, `isize`, `f32`, `f64` （文字列から数値への変換）

```rust
let a: &str = "0";
let b: u8 = a.parse::<u8>().unwrap();

let a: String = "48".to_string();
let b: f64 = a.parse::<f64>().unwrap();
```

## `u8` -> `u16` -> `u32` -> `u64` （安全なアップキャスト）

安全なアップキャスト

```rust
let a: u8 = 0;
let b: u16 = From::from(a);
let c: u32 = From::from(b);
let d: u64 = From::from(c);
```

## `u64` -> `u32` -> `u16` -> `u8` （安全なダウンキャスト）

`as` を使った危険なダウンキャストを避ける。
nightly なら `try_from` を使えるが

```rust
#![feature(try_from)]
use std::convert::TryFrom;

let a: u64 = 255;
let b: u32 = TryFrom::try_from(a).unwrap();
let c: u16 = TryFrom::try_from(b).unwrap();
let d: u8 = TryFrom::try_from(c).unwrap();
```

stable では毎回境界チェックが必要

```rust
fn i64_to_i8(u: i64) -> Option<i8> {
    let min = u8::min_value() as i64;
    let max = u8::max_value() as i64;
    if u < min || u > max {
        None
    } else {
        Some(u as i8)
    }
}

let a: i64 = 255;
let b: i8 = i64_to_i8(a).unwrap();
```

または polyfill を使う - https://crates.io/crates/try_from


以下都度追記予定・・・
