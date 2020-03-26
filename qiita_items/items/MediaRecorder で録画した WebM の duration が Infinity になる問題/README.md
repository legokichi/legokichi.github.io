# MediaRecorder で録画した WebM の duration が Infinity になる問題

MediaRecorder で Stream を録画するとその動画の duration が Infinity になる問題がある。

## サンプルコード

https://jsfiddle.net/zsrchm7d/3/

```js
const ctx = document.createElement("canvas").getContext("2d");
const cnv_stream = ctx.canvas.captureStream(60);
const rec = new MediaRecorder(cnv_stream, {mimeType: 'video/webm; codecs="vp8, opus"'});
const chunks = [];

ctx.canvas.width  = 400;
ctx.canvas.height = 300;

document.body.appendChild(ctx.canvas);

rec.ondataavailable = (ev)=>{ chunks.push(ev.data); };
rec.start();

const tid = setInterval(renderer, 1000/5);

setTimeout(rec_stop, 3000);

function rec_stop(){
	clearInterval(tid);
  rec.stop();
  const webm = new Blob(chunks,  { 'type' : 'video/webm' });
	const video = document.createElement("video");
  video.src = URL.createObjectURL(webm);
  video.controls = true;
  video.autoplay = true;
  video.loop = true;
  document.body.appendChild(video);
  
  alert("before-loadedmetadata: "+video.duration);
  video.onloadeddata = ()=>{
		alert("after-loadedmetadata: "+video.duration);
  };
}

function renderer(){
	ctx.canvas.width = ctx.canvas.width;
  ctx.arc(
  Math.random()*ctx.canvas.width,
  Math.random()*ctx.canvas.height,
  10, 0, 2*Math.PI);
  ctx.fill();
}
```

## 原因

ffmpeg でヘッダを見てみる。

```
$ ffmpeg -i 01eb2a76-072c-4195-8c26-a71646b08d2f.webm
ffmpeg version 3.1.4 Copyright (c) 2000-2016 the FFmpeg developers
  built with Apple LLVM version 7.0.2 (clang-700.1.81)
(中略)
Input #0, matroska,webm, from '01eb2a76-072c-4195-8c26-a71646b08d2f.webm':
  Metadata:
    encoder         : Chrome
  Duration: N/A, start: 0.000000, bitrate: N/A
    Stream #0:0(eng): Audio: opus, 48000 Hz, mono, fltp (default)
    Stream #0:1(eng): Video: vp8, yuv420p, 2592x1944, SAR 1:1 DAR 4:3, 15 fps, 15 tbr, 1k tbn, 1k tbc (default)
At least one output file must be specified
```

1. MediaStream は本質的にいつ停止するか事前にわからない
2. MediaRecorder API が webm のEBMLヘッダ+フレームバッファを逐次送る形式になっておりメタデータは最初に埋め込まれる
3. HTMLVideoElement が　webm の `Duration: N/A` を `Infinity` 扱いにする。

## 対策

`HTMLVideoElement.currentTime` に大きな値を入れて最後までシークさせると値が取得できる。

https://jsfiddle.net/zsrchm7d/4/

```js
video.currentTime = 7*24*60*1000;
video.onseeked = ()=>{
  alert("after-seeked: "+video.duration);
  video.onseeked = undefined;
};
```

ユーザに webm の blob を配布する場合、この情報を元に blob の EBML をパースして書き換えると良い。

