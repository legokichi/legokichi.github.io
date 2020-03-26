この記事は CUDA & OpenCL Advent Calendar 2014 の 6 日目です。

<blockquote class="twitter-tweet" lang="en"><p>スキルセットにHTML5入れるんならWebAudioとかWebCLとかもちろん使えるんですよね？？？？</p>&mdash; 自然界 (@mizchi) <a href="https://twitter.com/mizchi/status/540123533259464705">December 3, 2014</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

という煽りを見たのでWebCLを触ってみました

[http://webcl.nokiaresearch.com/](http://webcl.nokiaresearch.com/)
![webcl.nokiaresearch.com screenshot](http://i.gyazo.com/eccb0e1beb2878d7ea0976f203055ec7.png)
とあるので、その通りFirefox35をインストールしました。Firefox37 Nightlyはダメでした。

![webcl firefox addon screenshot](http://i.gyazo.com/961271769ce1f8bc353a5035393c16c1.png)
こんなが入ります

![webcl.nokiaresearch.com screenshot](http://i.gyazo.com/0d1726f5edf97f387d2bf780241251da.png)
準備できました

とりあえず[WebCL Tutorial](http://webcl.nokiaresearch.com/tutorials/tutorials.html)というチュートリアルのコードを写経して雰囲気を掴んでいきます。

前日_likrさんが[WebCLプログラミング入門](http://qiita.com/_likr/items/a4c34a906e0e3d2d2843)というのを書かれているので解説は省きます。

OpenCL C言語については[OpenCL 1.1 Reference Pages](https://www.khronos.org/registry/cl/sdk/1.1/docs/man/xhtml/)を覗きながら書きました

また、OpenCLもGPGPUも初めてでしたので、ローカルワークサイズとグローバルワークサイズについては[ワークアイテム・ワークグループ・次元数について](http://neareal.net/index.php?Programming%2FOpenCL%2FDimentionWorkGroupWorkItem)という記事を眺めてました

こうしてできたのがこちらになります

<blockquote class="twitter-tweet" lang="en"><p>&#39;WebCLでライフゲーム&#39; <a href="http://t.co/Ik2lypzlHV">http://t.co/Ik2lypzlHV</a> <a href="https://twitter.com/hashtag/altjsdoit?src=hash">#altjsdoit</a> <a href="http://t.co/Nge5t7zXMT">pic.twitter.com/Nge5t7zXMT</a></p>&mdash; 初期値鋭敏性 (@duxca) <a href="https://twitter.com/duxca/status/540491720169758720">December 4, 2014</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

この実装では

```coffeescript
globalWS = [width, height]
cmdQueue.enqueueNDRangeKernel(kernel, globalWS.length, null, globalWS)
```

というように次元数２でローカルワークアイテムを使わずグローバルワークアイテムのみをつかう実装にしました。

ところが、これで2000x2000のライフゲームを100ステップ動かしてみると、GPUよりCPUを使った方が早いという結果になりました

<blockquote class="twitter-tweet" lang="en"><p>WebCLでの2000x2000のライフゲーム、HD Graphics 4000は16722ミリ秒、Intel(R) Core(TM) i5-3427U CPU @ 1.80GHzは3778ミリ秒 <a href="http://t.co/XKRyAvrMVA">http://t.co/XKRyAvrMVA</a> <a href="https://twitter.com/hashtag/altjsdoit?src=hash">#altjsdoit</a></p>&mdash; 初期値鋭敏性 (@duxca) <a href="https://twitter.com/duxca/status/540655483669864448">December 4, 2014</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

カーネルからグローバルメモリにアクセスしているのが原因のようです。

<blockquote class="twitter-tweet" lang="en"><p><a href="https://twitter.com/duxca">@duxca</a> 昨晩altjsdoitに載せていたソースですかね？各ワークアイテムでグローバルメモリの読み込みが8回入るのが気になります。ローカルメモリはワークグループで共有されるので、ローカルメモリへ読み込み、同期、自分の場所を計算して書き込みとすると良いような気がします。</p>&mdash; Yosuke ONOUE (@_likr) <a href="https://twitter.com/_likr/status/540656424208973824">December 4, 2014</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

```c
kernel void cell(global char* vectorIn1,
                 global char* vectorOut,
                 uint   vectorWidth){
  int x = get_global_id(0);
  int y = get_global_id(1);
  int width  = get_global_size(0);
  int height = get_global_size(1);
  int index = width * y + x;
  int xplus  = (x+1 <= width-1  ? x+1 : 0);
  int yplus  = (y+1 <= height-1 ? y+1 : 0);
  int xminus = (0   <= x-1      ? x-1 : width-1);
  int yminus = (0   <= y-1      ? y-1 : height-1);
  int mylife = vectorIn1[width * y + x] > 0;
  //グローバルメモリに9回アクセスしている
  int nears[8] = {
    vectorIn1[width * yplus  + xminus],
    vectorIn1[width * yplus  + x     ],
    vectorIn1[width * yplus  + xplus ],
    vectorIn1[width * y      + xminus],
 // vectorIn1[width * y      + x     ],
    vectorIn1[width * y      + xplus ],
    vectorIn1[width * yminus + xminus],
    vectorIn1[width * yminus + x     ],
    vectorIn1[width * yminus + xplus ]
  };
  int lives = 0;
  for(int i=0; i<8; i++){
    if(nears[i]) lives++;
  }
  if(mylife){
    if(lives == 0 || lives == 1){
      vectorOut[index] = 0;
    }else if(lives == 2 || lives == 3){
      vectorOut[index] = 1;
    }else{
      vectorOut[index] = 0;
    }
  }else{
    if(lives == 3){
      vectorOut[index] = 1;
    }else{
      vectorOut[index] = 0;
    }
  }
}
```

OpenCLではメモリが階層化されており、アクセスの高速な順に次のようになっています

1. プライベートメモリ
2. ローカルメモリ
3. コンスタントメモリ
4. グローバルメモリ
5. ホストメモリ

グローバルメモリはカーネルが直接アクセスできるメモリの中で一番遅いようです

そこでローカルメモリを利用したプログラムを書こうと思ったのですが・・・

<blockquote class="twitter-tweet" lang="en"><p>ローカルメモリへ読み込み、同期、自分の場所を計算して書き込み、barrierを使えばできる気がするが・・・ムーア近傍取ってるのでローカルメモリの大きさ＝ワークグループ内のアイテムの数にならないのがつらみ</p>&mdash; 初期値鋭敏性 (@duxca) <a href="https://twitter.com/duxca/status/540692188707827713">December 5, 2014</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

<blockquote class="twitter-tweet" lang="en"><p>どんどんGPUデバイスべったりなコードになっていくつらみ</p>&mdash; 初期値鋭敏性 (@duxca) <a href="https://twitter.com/duxca/status/540692538491797505">December 5, 2014</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

<blockquote class="twitter-tweet" lang="en"><p>21世紀のプログラマが書くプログラムじゃない</p>&mdash; 初期値鋭敏性 (@duxca) <a href="https://twitter.com/duxca/status/540692620775665664">December 5, 2014</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

というように普段ブラウザで富豪プログラミングしてる私にはつらい領域に達してきたので今回はここまでです

ローカルメモリを使った実装はまた今度挑戦します

Advent Calendarの次の担当は@ykstさんです。
