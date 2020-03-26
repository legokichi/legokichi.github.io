# MediaRecorder API で録画した動画の seek に時間がかかる問題

## 問題

Chrome の MediaRecorderAPI で保存した動画は `video.controls` が [seekable ではなく](https://bugs.chromium.org/p/chromium/issues/detail?id=569840) 、そのまま `HTMLVideoElement` に読み込ませるとシークできなくなる。
[前回の記事](http://qiita.com/DUxCA/items/14a8d39dbaa90c2a5ccb) の通り、  `video.duration` に大きな数字を入れることで `video.controls` は一応 seekable になる。

```ts
const video = document.createElement("video");
let now = 0;
video.addEventListener("loadeddata", ()=>{
  video.currentTime = 24*60*60*1000;
  now = performance.now();
});
video.addEventListener("seeked", ()=>{
  console.info("seeked", performance.now() - now);
  document.body.appendChild(video);
});
video.src = "recorded_by_media_recorder_api.webm";
video.controls = true;
```

しかし、 1時間近く録画して生成した大きな webm 動画に関しては、 __`video.controls` を利用した seek が非常に遅くなる問題が発生する__。

## 原因
 
1. MediaRecorder API で 録画した webm の コンテナである [EBML](https://www.matroska.org/technical/specs/index.html) のヘッダ領域に `SeekHead` 要素が [ない](https://bugzilla.mozilla.org/show_bug.cgi?id=969290) ので `loadedmetadata` 時点では `duration` 情報は一切ない。
2. [上述のハック](http://qiita.com/DUxCA/items/14a8d39dbaa90c2a5ccb) では `HTMLVideoElement` が [HTTP/1.1 Range Request](https://triple-underscore.github.io/RFC7233-ja.html) をしながらファイルの先頭から順番に読み込んでいき、 EBML の `Segment > Cluster > Timecode` 要素と `Segment > Cluster > SimpleBlock` 要素を [逐次読み込んで時刻情報を得ていく](https://dev.w3.org/html5/pf-summary/video.html) ので、極めて効率が悪い。
f

## 対処法

### 1. その場しのぎ「あらかじめ動画全体を読み込んでおく」
Range Request で逐次解析するのが極端な遅さの原因なので、あらかじめ動画ファイルをメモリに乗せておけばよい。

```ts
const xhr = new XMLHttpRequest();
xhr.responseType = responseType;
xhr.open("GET", "foo.webm");
xhr.send();
xhr.onreadystatechange = ()=>{
  if(xhr.readyState !== 4){ return; }
  if(200 !== xhr.status){ return; }
  const blob = xhr.response;
  const url = URL.createObjectURL(blob);
  const video = document.createElement("video");
  video.src = url;
  video.onloadeddata" = ()=>{
    video.currentTime = 24*60*60*1000;
    video.seeked = ()=>{
      document.body.appendChild(video);
    };
  };
  
};
```

これで Range Request が発生する場合よりは seek の応答性は早くなる。しかしまだ遅い。なぜなら seek が発生するたびにファイル先頭から逐次時刻を計算していることには変わらないからである。

### 2. 根本的な解決策「webm ファイルを動的に構成する」

時刻計算を省くためには webm ファイルのメタデータにシーク情報を与えておく必要がある。
`MediaRecorder` の `ondataavailable` で送られてくる `BlobEvent` の `ev.blob` の中の EBML を逐次解析し、 `Clustor` 要素の位置を記録しておく。
その後、録画終了時に [`Clustor` 要素への参照を記録した `Segment > SeekHead` 要素を追加](https://www.matroska.org/technical/order/index.html)するか、[MediaSourceAPI](http://qiita.com/tomoyukilabs/items/57ba8a982ab372611669) を使用して動的に構成したヘッダを渡すなりすればよい。

#### webm ファイルの中身をみて `SeekHead` がないことを確認する
　
[ここ](https://permadi.com/2010/06/webm-file-structure/) によると [ebml-viewer](https://code.google.com/archive/p/ebml-viewer/) か [mkvinfo](https://mkvtoolnix.download/doc/mkvinfo.html) を使えばよいとあるので、私は mkvinfo を利用した。 [mkvtoolnix をインストール](https://mkvtoolnix.download/downloads.html) すればよい。

```
$ mkvinfo -g foo.webm
```

とすれば GUI で mkv の中身をみることができる。標準ではメタ情報しか表示しないのでメニューから全ての情報を取得を選択するようにする。
中を覗いてみると `SeekHead` がないことが確認できる。

EBML の詳細については

* https://www.matroska.org/technical/specs/index.html
* https://www.webmproject.org/docs/container/#muxer-guidelines
* http://qiita.com/ryiwamoto/items/0ff451da6ab76b4f4064
* http://www.slideshare.net/mganeko/inside-webm

などが明るい。

#### JS から EBML を読み込む

[themasch/node-ebml](https://github.com/themasch/node-ebml/) と [mathiasvr/ebml-block](https://github.com/mathiasvr/ebml-block/) を使用する。

#### MediaRecorderAPI が生成する webm の EBML 構造

```xml
<EBML>
  ...
</EBML>
<Segment>
  <Info>
    <!--
    ここに duration がないため `ffmpeg -i` などでみると duration が n/a となる
    <Duration>0</Duration>
    -->
    <TimecodeScale>1000000</TimecodeScale>
    <!--
      1000000ns = 1ms
      以後の時刻の単位系を決める
    -->
    ...
  </Info>
  <!--
  ここに SeekHead が欲しい
  <SeekHead>
    <Seek>
      <SeekID>[1F][43][B6][75]</SeekID>// EBML ID
      <SeekPosition>11451489464</SeekPosition> // offset from <Segment />
    </Seek>
    ...
  </SeekHead>
  -->
  <Tracks>
    <TrackEntry>
      <TrackNumber>1</TrackNumber>
      <TrackType>1</TrackType><!-- 1=video, 2=audio -->
      ...
    </TrackEntry>
    ...
  </Tracks>
  <Cluster>
    <Timecode>0</Timecode><!-- このクラスタの開始時刻(TimecodeScale単位) -->
    <SimpleBlock>
      <!--SimpleBlock の中身は ebml-block を使えば読める-->
      <track>1</track>
      <timecode>0</timecode><!-- クラスタ先頭からの時刻(TimecodeScale単位) -->
      <keyframe/>
      ...
    </SimpleBlock>
    <SimpleBlock>
      <timecode>0.033</timecode>
      ...
    </SimpleBlock>
    ...
    <!--
    SimpleBlock があと 98 個続く
    -->
<!--
Cluster, Segment は不定長要素なので閉じタグはない
新たな Clustor は level 0 要素(EBMLとSegment) からやり直される
-->
<EBML>...</EBML>
<Segment>
  ...
  <Cluster>
    <Timecode>0</Timecode>
    <SimpleBlock>...</SimpleBlock>
    ...
...
<!--
以後繰り返し
-->
```

#### 動的解析

node-ebml は node の　stream 標準ライブラリに依存しているため、そのまま MediaRecorder に適用できないが、書き換えるのは容易だった。ソースは公開していないので各自挑戦してみてほしい。
ちなみに buffer の polyfill は [feross/buffer](https://github.com/feross/buffer) が良かった。
あるいは [oeuillot/node-matroska](https://github.com/oeuillot/node-matroska) ならば browserify で動くかもしれない。

## 追記

* [node-ebml](https://github.com/themasch/node-ebml/issues/19) も [node-matrosika](https://github.com/oeuillot/node-matroska/issues/4) も webm streaming のための不定長要素を読み込むことができないバグがあったので報告した
* Matroska の [libebml](https://github.com/Matroska-Org/libebml/blob/347a75d77c9c48d1c20bca44fa8639e0afa0ca38/src/EbmlElement.cpp#L149) の該当コード
* chromium の [libwebm](https://chromium.googlesource.com/webm/libvpx/+/master/third_party/libwebm/mkvparser/mkvparser.cc#787) の該当コード


## 追記

上述の問題を解決した EBML パーサを書いた (https://github.com/legokichi/ts-ebml)

以下のようにして WebM のストリームを動的に解析することができる

```ts

import EBML, {Decoder, Encoder, Refiner, tools} from "./";

const Buffer: typeof global.Buffer = require("buffer/").Buffer;

const decoder = new Decoder();
const encoder = new Encoder();
const refiner = new Refiner();

async function recorder_main() {
	const stream = await navigator.mediaDevices.getUserMedia({video: true, audio: true});
  const rec = new MediaRecorder(stream, { mimeType: 'video/webm; codecs="vp8, opus"' });  
  const tasks: Promise<void>[] = []; 
  
  rec.ondataavailable = (ev: BlobEvent)=>{
    const chunk = ev.data;
    
    const task = readAsArrayBuffer(chunk) // Blob -> Promise<ArrayBuffer>
      .then((buf)=>{
        const chunks = decoder.decode(buf); // Blob -> EBMLElement[]
        refiner.read(chunks); // EBMLElement[] -> (EBMLCluster[], duration, ...)
      });
    
    tasks.push(task);
  };
  rec.start(100);

  await new Promise((resolve)=> setTimeout(resolve, 30 * 1000) );
  
  rec.stop();
  rec.ondataavailable = undefined;
  rec.stream.getTracks().map((track) => { track.stop(); });
  
  await tasks.reduce((o, prm) => o.then(() => prm), Promise.resolve(void 0)); // wait 30 sec

  // metadata: SeekHead と Duration が挿入された新しい metadata
  // clusterStartPos: 最初の Cluster 要素の位置
  const {metadata, clusterStartPos} = refiner.putRefinedMetaData();
  const webmBuf = await readAsArrayBuffer(WebM);
  const clustersBuf = webmBuf.slice(clusterStartPos);
  // metadata の置き換え
  const refined = new Blob([metadata, clustersBuf], {type: "video/webm"});

  const originalVid = await putVideo(WebM, "plain recorded webm");
  const refinedVid = await putVideo(refined, "refined webm");

  console.assert(! Number.isFinite(originalVid.duration)); // Infinity
  console.assert(  Number.isFinite(refinedVid.duration));  // Finite

  originalVid.currentTime = 1000 * 60 * 60 * 24 * 7;
  originalVid.onseeked = ()=>{
    originalVid.onseeked = <any>undefined;
    originalVid.currentTime = 0;
    console.assert(refinedVid.duration === originalVid.duration); // Duration も一致
  }
}

```

