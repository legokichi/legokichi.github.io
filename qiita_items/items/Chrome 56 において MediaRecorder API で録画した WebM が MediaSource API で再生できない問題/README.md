# Chrome 56 において MediaRecorder API で録画した WebM が MediaSource API で再生できない問題

## 問題

MediaRecorder で生成した WebM の Blob を MediaSource に読ませて再生したい。
しかし、以下のコードでは Chrome 56.0.2924.87 (64-bit) では再生できない。

```ts
async function main(){
  const video = document.createElement('video');
  const stream = await navigator.mediaDevices.getUserMedia({video: true, audio: true});
  const rec = new MediaRecorder(stream, {mimeType: 'video/webm; codecs="opus,vp8"'});
  const ms = new MediaSource();

  video.src = URL.createObjectURL(ms);
  video.controls = true;
  video.autoplay = true;
  document.body.appendChild(video);

  await waitEvent(ms, "sourceopen");

  const sb = ms.addSourceBuffer(rec.mimeType);

  const chunks: ArrayBuffer[] = [];

  rec.ondataavailable = async ({data: blob})=>{
    const buf = await readAsArrayBuffer(blob);
    chunks.push(buf);
  };

  console.info("start recorfing");

  rec.start(100);

  (async function recur(){
    if(rec.state === "paused"){ return; }

    if(sb.updating){
      await sleep(10);
      return recur();
    }

    const chunk = chunks.shift();
    if(chunk == null){
      await sleep(10);
      return recur();
    }

    sb.appendBuffer(chunk);
    console.info("appendBuffer:", chunk.byteLength, "B");
    
    await waitEvent(sb, "updateend");

    return recur();
  })();

	
  await sleep(30 * 1000);
	
  rec.stop();
  stream.getTracks().map((track)=>{ track.stop(); });
  if(ms.readyState === "open"){ ms.endOfStream(); }

  console.info("stop recorfing");
  
  await sleep(60 * 1000);

  console.info("garbage collection");

  ms.removeSourceBuffer(sb);
  URL.revokeObjectURL(video.src);
}


function sleep(ms: number): Promise<void> {
  return new Promise<void>((resolve)=>{ setTimeout(resolve, ms); });
}

function waitEvent<EV extends Event>(target: EventTarget, event: string, error?: string): Promise<EV> {
  return new Promise<EV>((resolve, reject)=>{
    target.addEventListener(event, _resolve);
    if(typeof error === "string"){
      target.addEventListener(error, _reject);
    }
    function _removeListener(){
      target.removeEventListener(event, _resolve);
      if(typeof error === "string"){
        target.removeEventListener(error, _reject);
      }
    }
    function _resolve(ev: EV){
      _removeListener();
      resolve(ev);
    }
    function _reject(ev: EV){
      _removeListener();
      reject(ev);
    }
  });
}

function readAsArrayBuffer(blob: Blob): Promise<ArrayBuffer> {
  const reader = new FileReader();
  reader.readAsArrayBuffer(blob);
  return waitEvent(reader, "loadend", "error").then(()=> reader.result);
}

main();
```


## 原因

* https://bugs.chromium.org/p/chromium/issues/detail?id=606000#c22
* Chrome ５６ の WebM はヘッダに [`Segment > Tracks > TrackEntry > DefaultDuration`](https://www.matroska.org/technical/specs/index.html) を含むが、[フレーム間のデルタ時刻が一定でない場合うまく再生できなくなるため](https://bugs.chromium.org/p/chromium/issues/detail?id=606000#c34)。
* Chrome 58.0.3015.0 canary (64-bit) では `DefaultDuration` を含まないようにする [パッチが適用された](https://chromium.googlesource.com/chromium/src.git/+/50252de90c71b7d5d12c30121a82d4e790f74252%5E%21/#F0)。


## Chrome58 を使わない対策

`ondataavailable` で送られてくる WebM の EBML を [ts-ebml](https://github.com/legokichi/ts-ebml) を使ってパースし、取り除く。

```diff
+ import * as EBML from "ts-ebml";

async function main(){
  const video = document.createElement('video');
  const stream = await navigator.mediaDevices.getUserMedia({video: true, audio: true});
```


```diff
+  const dec = new EBML.Decoder();
+  const enc = new EBML.Encoder();
  rec.ondataavailable = async ({data: blob})=>{
    const buf = await readAsArrayBuffer(blob);
+    const elms = dec.decode(buf);
+    const _elms = elms.filter((elm)=> elm.name !== "DefaultDuration");
+    const _buf = enc.encode(_elms);
-    chunks.push(buf);
+    chunks.push(_buf);
  };
```





