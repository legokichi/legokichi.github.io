# MediaRecorder の ondataavailable の Blob の結合時に動作が重くなる問題

下は [MDNのMediaRecorderの記事](https://developer.mozilla.org/en-US/docs/Web/API/MediaRecorder) の例にならって書いた MediaStream を録画する最小のコードである。

```js
const rec = new MediaRecorder(mediaStream, {mimeType: 'video/webm; codecs="vp8, opus"'});
const chunks: Blob[] = [];
rec.ondataavailable = (ev)=>{ chunks.push(ev.data); };
rec.start();

setTimeout(()=>{
  rec.stop();
  const webm = new Blob(chunks, { 'type' : 'video/webm' }); // 結合
}, 1000*60*1); // 1時間後に
```

実はこのコードには長時間録画すると blob の結合処理の動作が重くなるという問題がある。

## 原因

1. 高 bps の MediaStream （例えば getUserMedia ） を録画すると `MediaRecorder.prototype.ondataavailable` の呼ばれる頻度が高くなる.
2. `BlobEvent.prototype.data` の Blob が 1B に近い小ささなので、chunks に1分で数千ものBlobオブジェクトが溜まってしまう。
3. 1時間では数十万個近い Blob オブジェクトになり、大量の結合と大量のGCが入るため動作が重くなる。


## 対策

数分に一回は結合する。

```js
const rec = new MediaRecorder(mediaStream, {mimeType: 'video/webm; codecs="vp8, opus"'});
const chunks: Blob[] = [];
rec.ondataavailable = (ev)=>{ chunks.push(ev.data); };
rec.start();

let blob = new Blob([], { 'type' : 'video/webm' }); // 空のblobを用意する

function flush(){
  const _chunks = chunks.splice(0, chunks.length); // バッファを空にする
  blob = new Blob([blob].concat(_chunks), { 'type' : 'video/webm' });
}

const tid = setInterval(flush, 1000*60); // 1分おきに

setTimeout(()=>{
  clearInterval(tid);
  rec.stop();
  flush(); // 最後のflush
  const webm = blob; // 結合
}, 1000*60*60*1); // 1時間後に
```

## 追記

[`rec.start(timeslice)`](https://developer.mozilla.org/en-US/docs/Web/API/MediaRecorder/start) で ondataavailable の頻度を変えられたようだ。こちらを使うべきだろう。
