
最新情報

___[Haskell Language Server](https://marketplace.visualstudio.com/items?itemName=alanz.vscode-hie-server)を使え___

以下古い内容

<br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br />



# VisualStduioCodeでHaskell開発環境を整える

* [vscode-ghc-mod](https://marketplace.visualstudio.com/items?itemName=hoovercj.vscode-ghc-mod)
* [haskell-linter](https://marketplace.visualstudio.com/items?itemName=hoovercj.haskell-linter)
* [language-haskell](https://marketplace.visualstudio.com/items?itemName=justusadam.language-haskell)

を入れる。

## 手順

1. VisualStudioCodeを起動
    1. `Ctrl+P` でコンソールを出す
    2. `ext install vscode-ghc-mod`
    3. `ext install haskell-linter`
    4. `ext install language-haskell`
    5. `File > Preferences > Workspace Settings`
    5. `.vscode/settings.json` に以下のように記述。<pre>
       {
           "haskell.hlint.executablePath": "/path/to/hlint",
           "haskell.ghcMod.maxNumberOfProblems": 100,
           "haskell.ghcMod.executablePath": "/path/to/ghc-mod",
           "haskell.ghcMod.onHover": "fallback",
           "haskell.ghcMod.check": true,
           "haskell.ghcMod.logLevel": "error" 
       }
       </pre>
       詳細は以下のページで
       * [vscode-ghc-mod](https://marketplace.visualstudio.com/items?itemName=hoovercj.vscode-ghc-mod)
       * [haskell-linter](https://marketplace.visualstudio.com/items?itemName=hoovercj.haskell-linter)
3. ターミナルを起動
    1. `stack install hlint`
    2. 「[stackでghc-modを使う試行錯誤のメモ - Qiita](http://qiita.com/hiratara/items/15aba2534b16c2af8a02)」「[stackとghc-modを一緒に使う時の手順 - Qiita](http://qiita.com/siphilia_rn/items/bba4519710bb0513d6f9)」あたりを読む
    3. ライブラリの補完が効かない。わからん

## 参考

* [VS CodeでHaskellの簡単な環境を作る - クソザコの進捗ガレージ](http://azaika.hateblo.jp/entry/2015/12/19/151554)
