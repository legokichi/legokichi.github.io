# Rust と C++17 のソースコードを混ぜてビルドする
## 動機
* C や C++ で書かれたライブラリを利用しつつ、 Rust を書きたい
* https://crates.io/crates/cc というライブラリを使う
* cargo による rust のコンパイル時に C++ のコンパイルもいっしょにできる
* これで C や C++ のライブラリの FFI をいちから手書きするのではなく、 C や C++ のライブラリを呼ぶ部分は C++ で書いておいて Rust から呼ぶということができる

## 導入
* Cargo.toml に cc を追加する

```rs:Cargo.toml
[package]
name = "hello-rs"
version = "0.1.0"
links = "hello"
build = "build.rs"

[build-dependencies]
cc = { version = "1.0", features = ["parallel"] }

[dependencies]
libc = "0.2"
```

* parallel は make -jN 相当の並列コンパイル
* libc は char や int などの C API の型が定義されている - https://crates.io/crates/libc
* links - は依存ライブラリを記述するもの - http://doc.crates.io/build-script.html#the-links-manifest-key

## C++ のコードを書く
```c:src/cpp/include/hello.h
#pragma once

#ifdef __cplusplus
extern "C" {
#endif

  void hello();
#ifdef __cplusplus

}
#endif
```

```cpp:src/cpp/src/hello.cpp
#include <iostream>
#include <string>

void hello(){
  std::cout << "hello cpp" << std::endl;
}
```

* 基本的には C API で書くので Rust から呼び出す関数は C の型を使う

## Rust で C の FFI を書く

```rs:src/main.rs
extern crate libc;

use libc::{ c_void, c_char, size_t };
use std::ffi::{ CStr, CString };

#[link(name = "hello", kind = "static")]
extern "C" {
  fn hello();
}

fn main() {
  unsafe{
    hello();
  }
}
```

* ffi の書き方は公式ドキュメントを参考に - https://rust-lang-ja.github.io/the-rust-programming-language-ja/1.6/book/ffi.html


## build.rs に C++ コードのビルド方法を記述する

```rs:build.rs
extern crate cc;

fn main(){
    println!("cargo:rustc-link-search=native=/usr/local/lib");
    cc::Build::new()
        .cpp(true)
        .warnings(true)
        .flag("-std=c++17")
        .flag("-Wall")
        .flag("-Wextra")
        .flag("-v")
        .flag("-g")
        .file("src/cpp/src/hello.cpp")
        .include("src/cpp/include")
        .compile("libhello.a");
}
```

* flag はひとつづつ書く
* 任意のバージョンの c コンパイラを使いたい場合は環境変数に指定する
    * `env CXX=/usr/bin/g++-7 cargo build`
    * `env CXX=/usr/bin/g++-7 cargo run`


## 参考
* http://mmi.hatenablog.com/entry/2017/02/28/213656
