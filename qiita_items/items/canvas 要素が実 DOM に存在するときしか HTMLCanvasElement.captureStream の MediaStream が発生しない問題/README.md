# canvas 要素が実 DOM に存在するときしか HTMLCanvasElement.captureStream の MediaStream が発生しない問題

HTMLCanvasElement.captureStream を使えば canvas 上のアニメーションから MediaStream を作ることができる。

しかし、 GoogleChrome 55.0.2883.95 (64-bit) で確認したところ、
対象となる canvas が実 DOM 上に存在しない場合 MediaStream が止まってしまう。
`display: none;` もダメである。

## サンプルコード

https://jsfiddle.net/zsrchm7d/1/

```js
const ctx = document.createElement("canvas").getContext("2d");
const cnv_stream = ctx.canvas.captureStream(60);
const url_cnv = URL.createObjectURL(cnv_stream);
const video_cnv = document.createElement("video");
let flag = true;

ctx.canvas.width  = 400;
ctx.canvas.height = 300;

video_cnv.src = url_cnv;
video_cnv.controls = true;
video_cnv.play();

document.body.appendChild(video_cnv);

setInterval(renderer, 1000/5);
setInterval(toggle, 3000);

function renderer(){
	ctx.canvas.width = ctx.canvas.width;
  ctx.arc(
  Math.random()*ctx.canvas.width,
  Math.random()*ctx.canvas.height,
  10, 0, 2*Math.PI);
  ctx.fill();
}

function toggle(){
  if(flag) document.body.appendChild(ctx.canvas);
  else     document.body.removeChild(ctx.canvas);  
  flag = !flag;
}
```

## 原因

canvas でのオフスクリーンレンダリングをキャプチャできないのは問題である。
おそらくバグだと思うが確認を取っていないため不明である。

## 対処法

`visibility: hidden;` を使う。大きさを持つので `width: 0%;` も併せて使う。

https://jsfiddle.net/zsrchm7d/2/

```js
const ctx = document.createElement("canvas").getContext("2d");
const cnv_stream = ctx.canvas.captureStream(60);
const url_cnv = URL.createObjectURL(cnv_stream);
const video_cnv = document.createElement("video");
let flag = true;

ctx.canvas.width  = 400;
ctx.canvas.height = 300;

video_cnv.src = url_cnv;
video_cnv.controls = true;
video_cnv.play();

setInterval(toggle, 3000);
setInterval(renderer, 1000/5);

document.body.appendChild(video_cnv);
document.body.appendChild(ctx.canvas);

function renderer(){
	ctx.canvas.width = ctx.canvas.width;
  ctx.arc(
  Math.random()*ctx.canvas.width,
  Math.random()*ctx.canvas.height,
  10, 0, 2*Math.PI);
  ctx.fill();
}

function toggle(){
  if(flag){
  	ctx.canvas.style.visibility = "";
    ctx.canvas.style.width = "";
  }else{
  	ctx.canvas.style.visibility = "hidden";
    ctx.canvas.style.width = "0%";
  }
  flag = !flag;
}
```

