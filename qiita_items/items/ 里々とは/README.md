この記事は[マイナー言語 Advent Calendar 2013](http://qiita.com/advent-calendar/2013/minor-language) の17日目です。

# 里々とは
[伺か(ukagaka)](https://www.google.co.jp/search?q=伺か)というデスクトップマスコットの実行環境があります。このデスクトップマスコットのことを伺かではゴーストといい、このゴーストのAIを担当するプログラムのことを[SHIORI](http://usada.sakura.vg/contents/specification2.html#shiori)（栞）とよびます。
[里々(satori)](http://ms.shillest.net/satoriya.xhtml)はその栞の一種で、とにかく簡単にゴーストのプログラミングができるため、もっとも人気な栞のひとつです。

さて、今回は例として艦隊これくしょんのキャラクターのセリフを書いてみましょう。

# トーク
キャラクターのおしゃべりをこんな風に書けます。

```
＊OnBoot
：英国で産まれた帰国子女の金剛デース！
ヨロシクオネガイシマース！
```

簡単ですね。

「＊ラベル名」でトークを定義できます。
「OnBoot」はその名のとおりのシステムイベントです。伺かはイベント駆動です。

```
＊出撃
：連装砲ちゃん、一緒に行くよ
：[＞ヮ＜]
：しまかぜ、出撃しまーす！
```

トーク行頭についている「：」は、たとえば島風と連装砲ちゃんといった二人で一組のキャラクターを会話させるのに使う機能なのですが、艦これではキャラクター同士の会話がほとんどないため、今回はほとんどつかいません。

# ジャンプ

```
＊OnMouseDoubleClick
＞母港/詳細閲覧

＊母港/詳細閲覧
：紅茶が飲みたいネー

＊母港/詳細閲覧
：HI!今日もイイ天気ネー！

＊母港/詳細閲覧
：HEY！提督ぅー。触ってもイイけどサー、時間と場所をわきまえなヨー！
```

「＞ラベル名」で他のトークへジャンプできます。
おなじラベル名のトークが複数あると、ランダムに選ばれます。

# 条件ジャンプ
条件ジャンプも使えるので時報トークも簡単につくれます。

```
＊OnMinuteChange
＞０分【タブ】（現在分）＝＝０
＃条件ジャンプ

＊０分
＞（現在時）時
＃ジャンプ先のラベル名を動的に決められるのでこんなこともできる

＊０時
：午前零時justデース！

＊１時
：午前一時。提督は働き者デース
```

「【タブ】」はタブキーです。全角スペースも半角スペースもダメです。
行頭「＃」はコメントアウトです。行頭以外ではコメントアウトになりません。
「（変数名）」で変数を参照できます。「（現在時）」と「（現在時）」は環境変数です。

# 変数

トーク内ではジャンプの他、変数宣言もできます。
金剛の放置ボイスを作ってみましょう。

```
＊OnSatoriBoot
＄放置時間【タブ】０

＊OnMouseClick
＄放置時間＝０

＊OnMinuteChange
＄放置時間＝（放置時間）＋１
＞放置時【タブ】（放置時間）＞５

＊放置時
＄放置時間＝０
：目を離さないでって言ったのにー
提督ぅー。何してるデース！
```

「＄変数名【タブ】式」で変数代入できます。型は基本的に文字列しかないです。
「＄変数名＝式」で代入すると、式である文字列が計算可能であれば計算してから代入されます。計算できなければそのまま文字列が代入されます。特にエラーとか吐きません。

# 単語群
単語群を使うことで語彙を広げることもできます。

```
＊遠征選択時
：Wow！（つづきのセリフ）！

＠つづきのセリフ
Congratulations
提督にプレゼンツネー
```

「＠ラベル名」で単語群を定義できます。
「（単語群名）」で単語群を呼ぶと、単語群の中からランダムに単語が選ばれます。
これを使うと[コピペジェネレータ・ジェネレータ](http://tanakh.jp/tools/copipe-gen-gen.html)みたいなトークを簡単に作ることができます。

艦これのキャラクターのセリフは全て上記の機能だけで記述することができます。

# 関数
「（関数名、引数）」というように括弧に引数をつけることで内部関数を呼び出すこともできます。

```
＊OnMouseClick
：（call、足し算、1、1）

＊足し算
：（A0）＋（A1）は（call、add、（A0）、（A1））だよー。

＠add
（calc、（A0）＋（A1））
```

callという内部関数はトークや単語群を引数付きで呼び出す関数です。
またcalcは式を計算式としてパースして結果を返す関数です。

さて、この関数を使うことでLispのようなプログラム（見た目だけ）をかけるようになるのですが、詳しくはこちらで。
- [レゴキチ/里々自作関数まとめ](http://emily.shillest.net/specwiki/index.php?%E3%83%AC%E3%82%B4%E3%82%AD%E3%83%81%2F%E9%87%8C%E3%80%85%E8%87%AA%E4%BD%9C%E9%96%A2%E6%95%B0%E3%81%BE%E3%81%A8%E3%82%81)
- [うかべん 資料 - リリカル☆Lisp開発日記](http://blog.bugyo.tk/lyrical/archives/181)

----------------------
興味をもたれたらこちら
- [里々wiki](http://soliton.sub.jp/satori/)
