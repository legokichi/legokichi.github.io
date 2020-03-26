この記事は [伺か Advent Calendar 2016](http://www.adventar.org/calendars/1472)の1日目の記事です。

# Makefile と里々

## 概要

[伺か](https://ja.wikipedia.org/wiki/伺か)（うかがか）というデスクトップマスコットの振る舞いを記述するためのスクリプト言語として[里々](http://soliton.sub.jp/satori/)（さとり）がある。一方で、プログラムのビルド自動化スクリプトとして[makefile](https://ja.wikipedia.org/wiki/Make)がある。
今回、一見オライリーの `GNU Make` という本を読んでいて、全くドメインの異なるこれら２つの言語に共通点があることを発見した。特に、関数周りの記法に多くの共通点を見出し、里々は謂わば SakuraScript を make するための言語であるとみなせることが分かった。以前から、里々は[そのカッコのネストから](http://emily.shillest.net/specwiki/index.php?レゴキチ%2F里々自作関数まとめ)[Lisp だと](http://study.shillest.net/2008/1103/)[指摘されてきた](http://qiita.com/DUxCA/items/05ecf98e9ab30a1f2c03)が、今回の発見によりこれら従来の見解は覆ることになるかもしれない。

## 里々とは

里々とは、デスクトップマスコットの振る舞いを記述するためのスクリプト言語である。伺かというデスクトップマスコット互換環境のベースウェアは、pngファイルのコマ送りによるアニメーションを設定するShellと、マウスカーソルによるユーザからの入力などに対して反応を生成するGhostというサブシステムから成り、Ghostは栞（しおり）というDLLプラグインによって構成されている。

ベースウェアと栞DLLは[WebブラウザとHTTPサーバの関係に似ており](http://ssp.shillest.net/ukadoc/manual/memo_shiorievent.html)、ベースウェアがブラウザ、栞DLLがHTTPサーバ、そしてHTTPの代わりに[SHIORIプロトコル](http://usada.sakura.vg/contents/specification2.html)、HTMLの代わりに[SakuraScript](http://usada.sakura.vg/contents/sakurascript.html)がある。

ベースウェアは各種イベントに対応したSHIORIリクエストを栞DLLに投げ、

```[request]
GET SHIORI/3.0
Sender: Materia
ID: OnBoot

```

DLLはSakuraScriptというマスコット動作記述スクリプトを返す。

```[response]
SHIORI/3.0 200 OK
Sender: SomeGhost
Value: \0\s[0]こんにちは世界！\e

```

栞DLLはイベントに応じた当意即妙な SakuraScript を生成する必要があるため、
多くの栞DLLプラグインはこの SakuraScript を生成するためのイベントドリブンなスクリプト言語のインタプリタを持つ。

里々というスクリプト言語は、この栞DLLプラグインの一種であり、他の言語に[華和梨](http://kawari.sourceforge.net/)や[YAYA](http://emily.shillest.net/ayaya/index.php?StartUp)がある。

[里々で上記のレスポンスを返すスクリプトは次のように記述できる](http://soliton.sub.jp/satori/index.php?%A5%A4%A5%D9%A5%F3%A5%C8)。

```satori[dic00.txt]
＊OnBoot
：こんにちは（世界）！

＠世界
世界
ワールド
せかい
```

`＊` 記号は反応するイベント名を定義する。
`：` 記号はキャラクターの切り替え（伺かは複数のマスコットキャラクタの会話を記述できる）コマンドで、 `\0` を意味する。
`＠` 記号は単語群を定義し、全角カッコで呼び出すとランダムにどれか一つを選んで返す。[詳しくはここを読むこと](http://soliton.sub.jp/satori/index.php?%C6%C3%BC%EC%B5%AD%B9%E6%B0%EC%CD%F7)。

```satori[dic00.txt]
＊
：（栞）と（言語）が似てるんだって！
：そんな馬鹿な……？

＠栞
里々
華和梨
文

＠言語
makefile
シェルスクリプト
C言語

```

このようにトークのテンプレートに対してランダムに単語を割り当てることで会話パターンを水増し、ユーザに飽きさせないようにしている。多くの栞DLLはこの機能を持っている。

# Makefile と里々

では先の例を makefile で書いてみよう。

```make[makefile]
OnBoot:
@echo \0こんにちは$(世界)!\e

世界 = $(call choice1,世界 ワールド せかい)
```

makefile では `$(call [関数名])` を使うことで関数を呼び出すことができる。 makefile のユーザ定義関数 choice は空白区切り単語列から単語を一つ選ぶ関数とする。 makefile では乱数を扱えないため（ビルドシステムなので至極当然である）このような関数が存在するとして話を進める。

実は、里々も単語群に対して `（call、[関数名]）` を使うことができる。

```satori[dic00.txt]
＊OnBoot
：（call,add,1,1）

＠add
（calc,（A0）+（A1））
```

里々は全角カッコで呼び出せるもの（変数、単語群、トーク定義など）に対して `call` をつけて呼ぶことで引数付きで呼び出すことができる。上の例で呼び出している単語群 `＠add` の中ではただひとつの単語 `（calc,（A0）+（A1））` が定義されている。 `calc` は第一引数の文字列パースして四則演算する内部関数である。

makfile と比べてみよう。

```make[makefile]
OnBoot:
@echo \0$(call add,1,1)\e

add = $(shell expr $1+$2)
```

makefile も `$()` で呼び出せる再帰的変数・マクロに対して `call` で引数付きで呼び出すことができる。何よりも、里々も makefile も **文字列に対して逐次 eval されていくという点で似ている**。

## 決定的な違い

里々と makefile の決定的な違いは **goto の有無**である。
里々には `＞` というコマンドが用意されており、これは任意の `＊` 定義へと処理をジャンプする命令である。

```
＊OnMinuteChange
＞（現在時）時【タブ】（現在分）＝＝０

＊０時
：もう寝なさい
：誰このジジイ

```

makefile には goto はないのでこのような定義はできない。
むしろ makefile では `＊` のような逐次的な処理を記述することが困難である。

そもそも、里々でトークを呼び出す場合、通常は `call` や `（トーク名）` は使わず `＞` でトークの流れを処理するように書く。`call`を使う人のほうが例外的なのだ。

## 考察

里々を使っている人は makefile に触れることはなく、makefile を使っている人は里々に触れることはない。

このため、長い間この共通点は指摘されなかったのだろうと思われる。

## 結論

Makefile と里々の関数呼び出しが似ているという話をした。

実際に[里々という言語を開発した櫛ヶ浜やぎ](http://www.geocities.co.jp/SiliconValley-Cupertino/8536/satori.html)さんが makefile を意識していたのかは不明である。


