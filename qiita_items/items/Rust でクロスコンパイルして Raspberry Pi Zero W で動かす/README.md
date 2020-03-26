このスライドは [Rust LT #2 〜いま使う！Rust〜](https://rust.connpass.com/event/91177/) で発表した内容です

-----

## 自己紹介

某社でサーバサイド Rust 書いてる

-----

## いま使う！Rust

-----

## どこで使う？Rust

-----

## Rust は IoT と相性が良い

* ネイティブバイナリなので省リソース
* クロスコンパイル可能
* 言語機能がセキュア

-----

## 最近の IoT 実行環境

* コンテナでデプロイ ([Azure IoTEdge](https://docs.microsoft.com/ja-jp/azure/iot-edge/about-iot-edge))
* Kubernetes で管理 ([virtual-kubelet](https://github.com/Azure/iot-edge-virtual-kubelet-provider))
* 省リソースなコンテナ実行環境 ([resion.io](https://resin.io/how-it-works/))

-----

## 今回の話題

* RaspberryPi で IoT
* Rust でクロスコンパイル
* コンテナで DevOps

-----

## もくじ
 
1. クロスコンパイル入門
2. Dockernize 入門
3. RaspberryPi Zero で動かす

----

# 1. クロスコンパイル入門

----

## クロスコンパイルで考えること

1. ターゲットトリプル
2. クロスコンパイラ
3. 標準 C ライブラリ
4. Rust の対応状況

----

## 1. ターゲットトリプル

----

### ターゲットとは

* ホスト: プログラムをコンパイルする環境
* __ターゲット__: プログラムを実行する環境

----

### トリプル(triplet)とは

* コンパイルで必要なシステムを表す
* [`{arch}-{vendor}-{sys}-{abi}`](http://archive.linux.or.jp/JF/JFdocs/LFS-BOOK/chapter05/toolchaintechnotes.html)
* ABI は省略してトリプル

----

### ターゲットトリプルの例

* x86_64-pc-windows-msvc
* x86_64-apple-darwin
* x86_64-unknown-linux-gnu
* [(gcc と rust では一致しない場合もある)](https://wiki.debian.org/Multiarch/Tuples)

----

## 2. クロスコンパイラ

----

### クロスコンパイラとは

* ターゲット向けのバイナリを作るコンパイラ、リンカ...etc
* `rustc` はリンカに `gcc` を使っている
* 対応したリンカが必要

-----

### リンカの例

* debian の [cross-toolchain](https://wiki.debian.org/CrossToolchains)
* armhf(ARMv6) 向け: [raspberrypi/tools](https://github.com/raspberrypi/tools)
* コンパイル済みツールチェーン全部盛り: [crosstool-ng](http://crosstool-ng.github.io/)
* [japaric/cross](https://github.com/japaric/cross): docker + qemu (仮想マシン) を使った環境全部載せ

----

### リンカ等の指定方法

* [`build.rs`](https://doc.rust-lang.org/cargo/reference/build-scripts.html) を使ってコンパイラに渡す文字列を構築する
* [`.cargo/config`](https://doc.rust-lang.org/cargo/reference/config.html) を使って特定のターゲット向けにコンパイラ設定を書く

-----

```rust:build.rs
fn main() {
    println!("cargo:rustc-linker=arm-linux-gnueabihf-gcc");
    println!("cargo:rustc-flags=static=link-arg=-march=armv6");
}
```

----

```toml:.cargo/config
[target.arm-unknown-linux-gnueabihf]
linker = "arm-linux-gnueabihf-gcc"
rustflags = [
  "-C", "link-arg=-march=armv6",
]
```

----

## 3. 標準 C ライブラリ

----

### 標準 C ライブラリとは

* システムコールを C 言語でラップしたもの
* いわわゆる libc
* POSIX: libc の API など OS が満たすべき仕様

----

### glibc と musl

* [glibc](https://www.gnu.org/software/libc/): GNU が作った Linux(POSIX) 向けの libc
* [musl](https://wiki.musl-libc.org/projects-using-musl.html): 静的リンクされることにに特化した POSIX 互換 libc

----

## 4. Rust の対応状況

----

### rust ツールチェーンの対応状況

* ターゲット向けのコンパイル済み `std` crate はあるか
* [platform-support](https://forge.rust-lang.org/platform-support.html) に一覧がある
* [`rustup target add [target_triple]`](https://blog.rust-lang.org/2016/05/13/rustup.html) などでターゲットを指定できる

----

### 依存クレートの対応状況

* libc だけでなく依存クレート毎の共有ライブラにも注意
* SQLite, openssl, etc...

----

## 例: RaspberryPi Zero W の場合

-----

### ハードウェア

* SoC: [BCM2835(BCM2708)](https://raspberrypi.stackexchange.com/questions/840/why-is-the-cpu-sometimes-referred-to-as-bcm2708-sometimes-bcm2835)
* CPU: [ARM1176JZF-S](https://www.aps-web.jp/academy/ca/)
* 命令セット: __ARMv6__
* メモリ: 512MB
* [__Vector Floating Point__](http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.dui0205ij/CJAGBDFI.html): ハードウェア浮動小数演算)

----

### OS とトリプル

* OS: Raspbian Stretch Lite
    * Raspbian はハードウェア浮動小数点演算(HF) に対応した debian
* トリプル: `arm-unknown-linux-gnueabihf`
    * _gnueabihf_: glibc + HF [EABI](https://ja.wikipedia.org/wiki/Application_Binary_Interface#EABI)

----

1. トリプル は `arm-unknown-linux-gnueabihf` (ARMv6 HF)
2. リンカは [raspberrypi/tools](https://github.com/raspberrypi/tools)
3. 標準 C ライブラリは raspbian の glibc でよさそう
4. sqlite とか使う場合は debian の [マルチアーキテクチャ](https://debian-handbook.info/browse/ja-JP/stable/sect.manipulating-packages-with-dpkg.html) でを使ってクロスコンパイル用の依存ライブラリを raspbian のリポジトリとかから用意する

----

### 依存ライブラリをもってくる

```sh
echo "deb [arch=armhf] http://archive.raspbian.org/raspbian jessie main contrib non-free"
  | tee -a /etc/apt/sources.list
wget https://archive.raspbian.org/raspbian.public.key -O - | apt-key add -
dpkg --add-architecture armhf
apt-get update
apt-get install libsqlite3-dev:armhf
```

-----

#### `arm-unknown-linux-gnueabihf` って？

* `arm-unknown-linux-gnueabihf`: ARMv6+VFP (Pi,PiZ) 
* `armv7-unknown-linux-gnueabihf`: ARMv7+NEON (Pi2,Pi3)
* ARNv7 は ARMv6 互換

----

#### `armhf` って?

* `arm-unknown-linux-gnueabihf` または `armv7-unknown-linux-gnueabihf` の debian 上での呼称
* [debian の armhf は ARMv7向け](http://asobibalinux.hateblo.jp/entry/2013/01/08/021119)
* [raspbian の armhf は ARMv6 向け](http://www.raspbian.org/RaspbianAbout)

----

# 2. Dockernize

----

## Docker + Raspberry Pi

* [Raspberry Pi でも Docker が動くようになった](https://www.raspberrypi.org/blog/docker-comes-to-raspberry-pi/)
* ARM 向け Docker イメージをコンテナのベースに使う必要あり
-----

### [インストール方法](https://docs.docker.com/install/linux/docker-ce/debian/)

```sh
curl -fsSL https://download.docker.com/linux/debian/gpg | sudo apt-key add -
echo "deb [arch=armhf] https://download.docker.com/linux/debian \
     $(lsb_release -cs) stable" | \
    sudo tee /etc/apt/sources.list.d/docker.list
sudo apt-get update
sudo apt-get install docker-ce
sudo docker run armhf/hello-world
```

----

## ARM 向け Docker リポジトリとイメージ

* [docker-library/official-images](https://github.com/docker-library/official-images) にある
* [arm32v6/alpine](https://hub.docker.com/r/arm32v6/alpine/)
* [arm32v7/debian](https://hub.docker.com/r/arm32v7/debian)
* [resin/rpi-raspbian](https://hub.docker.com/r/resin/rpi-raspbian/)

----

## 理想の Docker イメージ

* デプロイ時間の短縮のためにコンテナは薄く軽くしたい
* Image がでかいとダウンロード時間かかる
* 非力な CPU だと解凍時間も長くなる

----

## イメージサイズ

* [arm32v6/alpine:latest](https://hub.docker.com/r/arm32v6/alpine/tags/) - 2MB
* [arm32v7/debian:stretch](https://hub.docker.com/r/arm32v7/debian/tags/) - 42MB
* [arm32v7/debian:stretch](https://hub.docker.com/r/arm32v7/debian/tags/) - 42MB
* [arm32v7/debian:stretch-slim](https://hub.docker.com/r/arm32v7/debian/tags/) - 19MB
* [resin/rpi-raspbian:stretch](https://hub.docker.com/r/resin/rpi-raspbian/tags/) - 53MB

----

### `arm32v6/alpine:latest`

* 軽い (2MB)
* そのままでは glibc がないので使うなら [musl](https://qiita.com/skame/items/12d12fef0902d162279e) か [glibc の](https://github.com/sgerrand/alpine-pkg-glibc) [インストールが必要](https://wiki.alpinelinux.org/wiki/Running_glibc_programs)

-----

### `arm32v7/debian:stretch-slim`

* 比較的軽い (19MB)
* ARMv6 ダメ

----

### `resin/rpi-raspbian:stretch`

* やや重い (53MB)
* ARMv6 + HF が使える

-----

## 例: RaspberryPi Zero W の場合

* ARMv6 なので `arm32v6/alpine` か `resin/rpi-raspbian`
* 共有ライブラリ使わないなら musl を静的リンクして `alpine`
* 使うならとりあえず `raspbian` で始めて徐々に `alpine`

----

# 3. RaspberryPi Zero W で動かす

-----

## おひとりさまミニブログ

* ツイ禁のために自作
* 衝動買いした RaspberryPi Zero W を使う
* [Tor Onion Service + IoT](https://blog.torproject.org/quick-simple-guide-tor-and-internet-things-so-far) でセキュアでグローバルIP＆ドメインいらずの自宅サーバ
* ~~自宅サーバは IoT か？~~

----

## DevOps とか

* actix-web + askama + diesel (sqlite)
* 開発マシン(ubuntu) 上の debian コンテナでクロスビルド
* rpi-raspbian でパッケージして Docker Hub に push 
* PiZ の Raspbian 上の docker で pull して実行

----

## クロスコンパイルテクニック

1. docker multistage-build を使う
2. SQLite を静的リンクする
3. `.cargo/config` で linker を指定する
4. `readelf` でターゲットを確認する

----

### 1. docker [multistage-build](https://docs.docker.com/develop/develop-images/multistage-build/)

----

```Dockerfile:Dockerfile
FROM debian:jessie as builder
WORKDIR /source
ADD . /source
RUN cargo build --target=arm-unknown-linux-gnueabihf -p server --release

FROM resin/rpi-raspbian
COPY --from=builder /source/target/arm-unknown-linux-gnueabihf/release/server /opt/server
CMD ["/opt/server"]
```

----

* ひとつの Dockerfile だけでビルドして成果物の別のコンテナに入れる機能
* debian でビルドしたバイナリを alpine に入れてパッケージ化とかできる
* [GoogleContainerTools/skaffold](https://github.com/GoogleContainerTools/skaffold/) 使うとデプロイまで自動化できる

----

* 今回は debian でビルドして raspbian でパッケージ化
* alpine 使いたかったが libsqlite3 も含めて musl にするのは面倒
* まずはとりあえず raspbian

-----

### 2. SQLite を静的リンクする

----

#### 静的リンクする動機

* 開発マシンでは rpi-raspbian ステージの apt は動かない(armhf なので)
* multi-stage build でクロスコンパイルする場合特有の問題
* なるべく debian 側でリンクしてしまいたい

-----

* raspbian の `libsqlite3-dev:armhf` を静的リンクする
* [deisel を使う側の `Cargo.toml` で `libsqlite3-sys` クレートの bundled feature を使う](https://github.com/diesel-rs/diesel/issues/1461)

-----

#### libsqlite3-sys の bundled を使う

```toml:Cargo.toml
[dependencies]
diesel = { version = "1.3", features = ["sqlite"] }
libsqlite3-sys = { version = "0.9", features = ["bundled"] }
```

-----

### 3. `.cargo/config` でリンカを指定する

* `raspberrypi/tools` の `arm-linux-gnueabihf-gcc` にパスを通しておく
* `/usr/lib/arm-linux-gnueabihf` に `libsqlite3` があるので linker にパスを渡しておく

-----

#### リンカオプションてんこもり

```toml:Cargo.toml
[target.arm-unknown-linux-gnueabihf]
linker = "arm-linux-gnueabihf-gcc"
rustflags = [
  "-C", "link-arg=-march=armv6",
  "-C", "link-arg=-mfpu=vfp",
  "-C", "link-arg=-mfloat-abi=hard",
  "-C", "link-arg=-L/opt/gcc-linaro-arm-linux-gnueabihf-raspbian-x64/arm-linux-gnueabihf/libc/usr/lib/arm-linux-gnueabihf/",\n\
  "-Z", "print-link-args",
]
```

-----

### 4. `readelf` で細かくバイナリを確認する

----

#### 怪奇現象

----

ARMv6 向け設定でビルドしたのに結果は ARMv7 になる現象

```console
$ readelf --arch-specific ./target/arm-unknown-linux-gnueabihf/debug/server
Attribute Section: aeabi
File Attributes
  Tag_CPU_name: "7-A"
  Tag_CPU_arch: v7
...
```

-----

期待する結果 (ARMv6) だとこう表示されるはず

```console
$ readelf --arch-specific ./target/arm-unknown-linux-gnueabihf/debug/server
Attribute Section: aeabi
File Attributes
  Tag_CPU_name: "6"
  Tag_CPU_arch: v6
...
```

---------

#### 原因を探る

1. `Tag_CPU_arch: v6` になるまでソースコードをコメントアウトする
2. `Tag_CPU_arch: v7` の原因となるライブラリの Cargo.toml の features を調べる

----

#### 原因の特定

3. actix-web のコードをコメントアウトすると `Tag_CPU_arch: v6` に戻ることが判明
4. `actix-web` が依存していた `cookie-rs` の `secure` featrue が `ring` crate に依存していたのが原因だと分かる
5. [`ring` crate が c コンパイラに依存していたのが原因](https://github.com/actix/actix-web/blob/2071ea053293e1f1bfde4e43bfab9137ac62ba48/Cargo.toml#L43-L44)

----

## 感想

----

## 感想

* クロスコンパイルは難しい
* alpine linux は不便なのでそのうち廃れそう
* ビルド＆パッケージ化を Dockerfile で管理できて便利

----

### クロスコンパイルは難しい

* Linux の基礎知識が必要
* オススメの情報源
    * [Linuxプログラミングインターフェース](https://www.oreilly.co.jp/books/9784873115856/)
    * [低レベルプログラミング](https://www.shoeisha.co.jp/book/detail/9784798155036)
    * [japaric/rust-cross](https://github.com/japaric/rust-cross)

-----

## おわり

