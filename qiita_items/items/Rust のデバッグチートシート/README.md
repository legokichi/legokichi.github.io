
# Rust Debugging Cheatsheet

## 開発の基本

### VSCode + rust-analyzer

https://marketplace.visualstudio.com/items?itemName=matklad.rust-analyzer


### フォーマッタ

* `cargo fmt`
* `cargo tomlfmt` - https://github.com/tbrand/cargo-tomlfmt

### cargo watch

https://github.com/passcod/cargo-watch

* `cargo watch -x clippy` - ファイル保存の度に lint
* `cargo watch -x test -- --nocapture` - ファイル保存の度にテスト(ログも表示)
* `cargo watch -x 'run -- --some-arg'` - ファイル保存の度に実行
* `env RUST_BACKTRACE=1 RUST_LOG=trace cargo watch -x test` - スタックトレース有効＆ env_logger 有効でコンパイル＆テスト
    * `RUST_LOG=tokio=info,mycrate=trace` のようにクレート単位で指定できる

### 依存関係を最新に保つ

* `cargo outdated` - https://qiita.com/sinkuu/items/3ea25a942d80fce74a90#cargo-outdated-cargo-outdated -  https://github.com/kbknapp/cargo-outdated
* `cargo update` - Cargo.toml の依存関係のバージョン制約内で最新に更新
* `cargo audit` - 依存クレートに脆弱性がないかチェック - https://github.com/RustSec/cargo-audit

## Rust のデバッグ

### 変数の型名を取得

関数 `the_answer_of_everything` 帰り値の型が知りたいとき

```rust
fn the_answer_of_everything() -> i32 { 42 }

fn main() {
  let _ :() = the_answer_of_everything();
}
```

```
error[E0308]: mismatched types
 --> src/main.rs:4:15
  |
4 |   let _ :() = the_answer_of_everything();
  |          --   ^^^^^^^^^^^^^^^^^^^^^^^^^^ expected `()`, found `i32`
  |          |
  |          expected due to this
```

* rls や rust-analyzer が使えないときに便利
* https://qiita.com/ubnt_intrepid/items/4995c51d1271cc9529f4

### コンパイラエラーコードの索引

* Rust Compiler Error Index - https://doc.rust-lang.org/error-index.html

### マクロ展開

error-chain のような巨大なマクロが何をしているのか見るのに便利

* `cargo expand` - 全部展開
* `cargo expand -- main` - 特定の関数だけ展開

### 依存クレートのデバッグ

* doc.rs をよくよく読む
    * [src] ボタンで定義のソースコードが見ることができるので実装もよく読む
* github などの issues を検索してよくよく調べる
* 各クレートの issues, discord, gitter, slack, irc で訊いてみる

### なんもわからん・・・てなったとき

* rust コミュニティで訊いてみる
    * rust-jp slack - https://rust-jp.herokuapp.com/
    * rust の公式 discord - https://discordapp.com/invite/rust-lang
    * ユーザフォーラム - https://users.rust-lang.org/


## 実行バイナリ解析

* とりあえず [cargo-binutils](https://github.com/rust-embedded/cargo-binutils) を入れておくこと
* linux のメモリマップについて知る
    * stack, heap, shared library - http://www.coins.tsukuba.ac.jp/~yas/coins/os2-2011/2012-01-24/index.html
* elf フォーマットについて知る
* 共有ライブラリの仕組みについて知る

おすすめの書籍

* 電話帳より厚いことで有名な [Linuxプログラミングインタフェース](https://www.amazon.co.jp/Linux%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%9F%E3%83%B3%E3%82%B0%E3%82%A4%E3%83%B3%E3%82%BF%E3%83%95%E3%82%A7%E3%83%BC%E3%82%B9-Michael-Kerrisk/dp/487311585X) 
* 内容はハイレベルな [低レベルプログラミング](https://www.amazon.co.jp/%E4%BD%8E%E3%83%AC%E3%83%99%E3%83%AB%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%9F%E3%83%B3%E3%82%B0-Igor-Zhirkov/dp/4798155039/)
* 入門には最適な [Linuxのしくみ](https://www.amazon.co.jp/%E8%A9%A6%E3%81%97%E3%81%A6%E7%90%86%E8%A7%A3-Linux%E3%81%AE%E3%81%97%E3%81%8F%E3%81%BF-%E5%AE%9F%E9%A8%93%E3%81%A8%E5%9B%B3%E8%A7%A3%E3%81%A7%E5%AD%A6%E3%81%B6OS%E3%81%A8%E3%83%8F%E3%83%BC%E3%83%89%E3%82%A6%E3%82%A7%E3%82%A2%E3%81%AE%E5%9F%BA%E7%A4%8E%E7%9F%A5%E8%AD%98-%E6%AD%A6%E5%86%85-%E8%A6%9A/dp/477419607X)
* 古のテクニックが載っている [Binary Hacks ―ハッカー秘伝のテクニック100選](https://www.amazon.co.jp/Binary-Hacks-%E2%80%95%E3%83%8F%E3%83%83%E3%82%AB%E3%83%BC%E7%A7%98%E4%BC%9D%E3%81%AE%E3%83%86%E3%82%AF%E3%83%8B%E3%83%83%E3%82%AF100%E9%81%B8-%E9%AB%98%E6%9E%97-%E5%93%B2/dp/4873112885)


### シンボル解析

* `cargo nm -- --demangle /path/to/executable | less` - rust のシンボル名の一覧
* `strings /path/to/executable | less` - バイナリに含まれている文字列列挙

### アセンブリ解析

* `cargo objdump -- -disassemble /path/to/executable | less` - rust の関数名とアセンブリの一覧
* `cargo asm rr_sandbox::id --rust --build-type debug`
* `cargo llvm-ir rr_sandbox::main --rust --build-type debug`
* https://rust.godbolt.org/ - オンライン rust asm ビューア

### 共有ライブラリ

* `ldd /path/to/executable` - 共有ライブラリの依存関係
* `find ./ -name "*.so*" -print | xargs readelf -d` - 共有ライブラリのシンボルテーブルを探す
* `LD_DEBUG=all <コマンドを実行>` - 共有ライブラリの読み込みに関して大量のデバッグログが吐かれる
* `env | grep LD` - パスを調べる
* `readelf -a hogehoge.so | grep SONAME` - SONAME を調べる
* `find build/ -name '*so' | xargs ldd | less` - 依存関係を調べる
* `readelf -d /path/to/executable` - elf のダイナミックセクションの表示

### elf

* `readelf -a /path/to/executable | less` - 全部表示
* `readelf -s /path/to/executable` - シンボルテーブル一覧
* `readelf -h /path/to/executable` - ELF ファイルヘッダ, ELFヘッダー情報
* `readelf -l /path/to/executable` - プログラムヘッダー情報
* `readelf -S /path/to/executable` - セクションヘッダー情報
* `readelf -A /path/to/executable` - アーキテクチャ固有情報、クロスコンパイル時に見る

### システムコール

* `strace -tt -T -f /path/to/executable` - システムコールの呼び出しをトレース

## プロファイリング

まだちゃんと使ったことない

### cargo-profiler

* https://github.com/kernelmachine/cargo-profiler

```sh
sudo apt-get install valgrind
cargo install cargo-profiler
```

```sh
cargo profiler callgrind
cargo profiler cachegrind --release
```

### Thread Profilertcargo binutils

* https://github.com/glennw/thread_profiler


## rr + rust-gdb

ステップ実行のリプレイが可能なデバッガ


### usage

* `rr record /path/to/executable`
* `rr replay -d rust-gdb`

### installation
```
echo 1 | sudo tee /proc/sys/kernel/perf_event_paranoid
sudo apt update && sudo apt install rr
```

### rr references

- https://github.com/mozilla/rr/wiki/
- using rust with rr - https://gist.github.com/spacejam/15f27007c0b1bcc1d6b4c9169b18868c
- /proc/sys/kernel/perf_event_paranoid - https://access.redhat.com/documentation/ja-jp/red_hat_enterprise_linux/7/html/7.3_release_notes/known_issues_compiler_and_tools
- https://github.com/mozilla/rr/wiki/Using-rr-in-an-IDE
- https://bitshifter.github.io/rr+rust/index.html



### .gdbinit

現在の状態を見やすくするユーザスクリプト

```
wget -P ~ https://git.io/.gdbinit
```

### gdb usage

#### 基本

* `run [args]` - `Start it from the beginning? (y or n)`
* `kill` - `Kill the program being debugged? (y or n)`
* `quit`

#### ブレイクポイントの管理

* `break <where>` - ソースコードの n 行目にブレークポイントを入れる
* `info breakpoints` - ブレークポイントの一覧
* `delete n` - ブレークポイント番号 n を消す
* `clear` - 全部消す
* `enable n` - ブレークポイント番号 n を有効
* `continue n` 無効

#### ウオッチポイントの管理

* `watch <where>`
* `info watchpoints`

#### &lt;where&gt; に指定できるもの

* `<function_name>`
* `<line_number>`
* `<file:line_number>`

#### (スタック)フレームの管理

* `backtrace`
    * `where` - bt と同じ
    * `backtrace full` - 呼び出しスタックを表示し、各フレームのローカル変数も出力します。
* `info locals` - 局所変数
* `info args` - 関数の引数
* `whatis <variable_name>` - 変数の型を表示

#### プログラム変更

* `set variable <variable_name>=<value>` - 変数をセット
    * `set var <variable_name>=<value>` - 同上
    * `set <variable_name>=<value>` - 同上
* `return <expression>` - 現在の関数から強制リターン

#### ステッピングの管理

* `s` - `step` - ステップ（呼び出された関数に入る）
    * `rs` - `reverse-step` - 逆のステップ
* `n` - `next` - 次（呼び出された関数には入りません）	
    * `rn` - `reverse-next` - 次を逆に
* `c` - `continue` - 次のブレークポイントまで続行
    * `rc` - `reverse-continue`
* `si` - `stepi` - 1マシン命令を実行
    * `rsi` - `reverse-stepi`
* `ni` - `nexti` - 1マシン命令を実行、関数呼び出しは無視
    * `rni` - `reverse-nexti`
* `f` - `finish` - 現在の関数のリターンまで続行
    * `rf` - `reverse-finish`

#### ソースコード

* `list` - 現在のポイントの周辺のソースコードを表示
* `disassemble (<where>)` - 現在または指定箇所のメモリ配置を表示

#### アセンブリ

* `layout asm` - 全体をページャで表示
* `info sharedlibrary` - ロード済み共有ライブラリの一覧

#### シグナル

* `info signals` - シグナルのハンドリング状況
    * `info handle` 同上

#### スレッド

* `info threads`

### gdb references

- https://qiita.com/miyagaw61/items/4a4514e2de0b458c2589
- https://darkdust.net/files/GDB%20Cheat%20Sheet.pdf
- https://github.com/cyrus-and/gdb-dashboard
- https://sourceware.org/gdb/onlinedocs/gdb/index.html#Top
- https://sourceware.org/gdb/onlinedocs/gdb/Rust.html#Rust

## ネットワーク

* tcp/ip スタックについてよく知る


### DNS

* `nslookup google.com`
* `dig google.com` - nslookup より詳しい

### tcpdump

actix-web の keep-alive がバグってるのを調べるのに使った

* `sudo /usr/sbin/tcpdump -nnn -A dst port 10179 -w 10179.dump`
* `tcpdump -vvv -s 0 -nX tcp`

### ip、ポート、プロトコルとプロセスの一覧

* `netstat -anp`

### iptables のルールやチェインの一覧 

* `sudo iptables -vnL`

### ip 経路情報 

* `traceroute google,com`
* `traceroute6 google,com` - NGN 閉域網で rumqtt が ipv6 につなぎにいってしまったときに使った
* `mtr google.com` - リッチな traceroute

### ディレクトリ、ファイル、ソケットを握ってるプロセスを調べる

* `sudo fuser --verbose ./foo` - foo を握ってるプロセスを調べる
* `fuser --namespace tcp 8080` tcp 8000 を握ってるプロセスを調べる
* `sudo fuser -k 8000/tcp` - ポート掴んでるプロセスを殺す

## ブートログ、カーネルログ、システムログ

* `sudo less /var/log/boot.log` - ブートログ
* `dmesg` - カーネルログ
* `journalctl` - systemd のログ



# todo

+ wasm
+ cross compile

