
## emacs と clisp のインストール

とりあえずclispで

```sh
$ sudo apt-get install clisp emacs
```

## quicklisp のインストール
* http://modern-cl.blogspot.jp/2011/03/quicklisp.html

```sh
$ cd ~/
$ wget http://beta.quicklisp.org/quicklisp.lisp
$ clisp
CL-USER> (load "quicklisp.lisp")
CL-USER> (quicklisp-quickstart:install :path ".quicklisp/")
CL-USER> (ql:add-to-init-file)
CL-USER> (exit)
```

## quicklisp からslimeをインストール
slimeとは emacs + common lisp な IDE

* http://qiita.com/garaemon/items/f65fbd59130337b83cf7

```sh
$ clisp
CL-USER> (ql:quickload :quicklisp-slime-helper)
To load "quicklisp-slime-helper":
  Load 1 ASDF system:
    quicklisp-slime-helper
; Loading "quicklisp-slime-helper"
.
slime-helper.el installed in "/Users/**/**/slime-helper.el"

To use, add this to your ~/.emacs:

  (load (expand-file-name "~/.quicklisp/slime-helper.el"))
  ;; Replace "sbcl" with the path to your implementation
  (setq inferior-lisp-program "sbcl")

CL-USER> (exit)
```

[Emacs22 からは `~/.emacs` ではなく `~/.emacs/init.el`に書くべし](http://sakito.jp/emacs/emacs23.html#emacs-d-init-el-emacs-el)


```sh
$ touch ~/.emacs.d/init.el
$ vim ~/.emacs.d/init.el

(load (expand-file-name "~/.quicklisp/slime-helper.el"))

(setq inferior-lisp-program "clisp")
;; ccl sbcl clisp など

;; SLIMEのロード
(require 'slime)
(slime-setup '(slime-repl slime-fancy slime-banner slime-indentation))

;; SLIMEからの入力をUTF-8に設定
(setq slime-net-coding-system 'utf-8-unix)
```

## emacs+slimeでよくつかうコマンド

* [SLIMEの使い方](http://modern-cl.blogspot.jp/2011/04/3-slime.html)
* [SLIMEにおける開発サイクル](http://modern-cl.blogspot.jp/2011/07/5-slime.html)
* [emacs23](http://sakito.jp/emacs/emacs23.html)
* [Emacs による Lisp Hacking](http://lispuser.net/emacs/lisphacking.html)
* [Emacs](http://hooktail.org/computer/index.php?Emacs)
* [Emacsの主要操作(早見表)](http://www.rsch.tuis.ac.jp/~ohmi/literacy/emacs/quick.html)

などを参照

* `M-x slime`: slimeを起動
* `C-c C-]`: 開いてる括弧をそこで全て閉じる括弧を追加する
* `C-c C-c`: カーソル位置のフォームを一番外からコンパイルしてロード
* `C-c C-k`: ファイル全体をコンパイルしてロード
* `C-c C-z`: REPLを表示・移動
* `M-p`: REPLの入力履歴を前へ
* `M-n`: REPLの入力履歴を次へ
* `C-x o`: 分割画面での移動
* `C-x C-b`: バッファ一覧表示
* `C-x b [buffer]`: 画面バッファの変更
* `C-x 2`: 表示するバッファを上下に分割
* `C-x 3`: 表示するバッファを左右に分割
* `C-x 1`: 表示するバッファを選択画面のみにする
* `C-x 0`: 選択画面を消す
* `C-l`: 画面再描画
* `C-x k`: 現在のバッファを閉じる
* `C-x f [ファイル名]`: ファイルを開く
* `C-x C-s`: ファイル保存
* `C-_`: 元に戻す
* `C-/`: やり直し
* `C-@`: 選択
* `M-w`: コピー
* `C-w`: 切り取り
* `C-y`: 貼り付け
* `C-c`: 困ったら連打
* [なんか間違ったキーバインド入力してよくわからんくなったら `C-g` を連打する](http://no-maddojp.hatenablog.com/entry/2014/04/26/171131)


## コミュニティ・情報源
* [ja.stackoverflow common-lisp](http://ja.stackoverflow.com/questions/tagged/common-lisp?sort=active&pageSize=50)
* [reddit lisp_ja](https://www.reddit.com/r/lisp_ja/)
* [Common Lisp: loopマクロ用法抄](http://smpl.seesaa.net/article/29800843.html)
* [Common Lisp の解説と小品](http://www.shido.info/lisp/idx_cl.html)
* [Cより高速なCommon Lispコードを書く](http://blog.8arrow.org/entry/2014/10/23/063044)
* [#:g1: frontpage](http://g000001.cddddr.org/)
* [逆引きCommon Lisp](http://tips.cddddr.org/common-lisp/)
* [qiita common-lisp](http://qiita.com/tags/common-lisp)
* [Lisp の型システム](http://blog.livedoor.jp/s-koide/archives/1885863.html)
* [lisphub](http://lisphub.jp/)
* [shibuya.lisp](http://shibuya.lisp-users.org/)
* [2016年のCommon Lispはこれだ!](http://qiita.com/g000001/items/36e3d502c2903ba7b661)
  * [2016年のCommon Lispの処理系はこれだ!](http://qiita.com/g000001/items/c2b25b8a72c96e9f4639)
  * [2016年のCommon Lispのドキュメントはここだ!](http://qiita.com/g000001/items/b342049b65e07615274e)
  * [2016年のCommon Lispの定番ライブラリはこれだ!](http://qiita.com/g000001/items/c6fa1444d1a49f1b6f76)
  * [2016年のCommon Lispの質問場所はここだ!](http://qiita.com/g000001/items/35a95ccb1a5b9a19d451)
  * [2016年のCommon Lispのライブラリ管理はこれだ!](http://qiita.com/g000001/items/98b5d67f527563895f78)
  * [2016年のCommon Lispの遊び場はここだ!](http://qiita.com/g000001/items/b16afa7c46a6fca57d6b)
  * [2016年のCommon Lispの情報ソースはここだ!](http://qiita.com/g000001/items/12e4b3d0c352e796de2e)
* [Common Lisp のリファレンスを使う](http://qiita.com/asesino/items/d60b48ee201949388800) HyperSpecインストール
* [xyzzy Lisp Programming](http://www.geocities.jp/m_hiroi/xyzzy_lisp.html)
* [Rosetta Code Category:Common_Lisp](http://rosettacode.org/wiki/Category:Common_Lisp)
* [Google Common Lisp スタイルガイド 日本語訳](http://lisphub.jp/doc/google-common-lisp-style-guide/lispguide.xml)
* [Alexandria Manual - draft version](https://common-lisp.net/project/alexandria/draft/alexandria.html)
* [SLIME User Manual Key-Index](https://common-lisp.net/project/slime/doc/html/Key-Index.html)
* [format関数](http://super.para.media.kyoto-u.ac.jp/~tasuku/format-func.html)
