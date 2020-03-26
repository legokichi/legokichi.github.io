# 複数枚重なったcanvasで透明領域のマウスイベントを見えている直下の要素に渡したい

## 例

https://jsfiddle.net/oyb4jby8/6/

![canvas layers](https://i.gyazo.com/91fc6ede4087ecd1a3865b57bbc3297c.png)

こういう透明なcanvasが複数枚重なっていたとして、
canvasの不透明領域(図中の丸)ではcanvasにマウスイベントが発生して、
それ以外の透明領域ではマウス直下の要素にマウスイベントを発火させたいとする。

```html
<div id="wrapper">
  <canvas id="cnv1"></canvas>
  <canvas id="cnv2"></canvas>
  <div id="rect"></div>
  <canvas id="cnv3"></canvas>
</div>
```

```css
canvas {
  border: none;
}
#wrapper{
  position: relative;
  top: 20px;
  left: 20px;
  width: 100px;
  height: 100px;
  background-color: gray;
}
#wrapper>*{
  position: absolute;
}
#rect{
  top: 25px;
  left: 75px;
  width: 50px;
  height: 50px;
  background-color: yellow;
  border: 1px solid black;
}
#cnv1{
  top: 10px;
  left: 10px;
}
#cnv2{
  top: 20px;
  left: 20px;
}
#cnv3{
  top: 30px;
  left: 30px;
}
```

## pointer-events
このとき、[pointer-events](https://developer.mozilla.org/en/docs/Web/CSS/pointer-events)
というCSSプロパティを使えばその要素にマウスイベントを発生させず透過させることができるが、canvasの不透明要素へのマウスイベントも当然発生しなくなる。故に今回の場合この方法は使えない。

## 座標の位置の要素を取得する
そこで次に考えるのが[document.elementFromPoint](https://developer.mozilla.org/en/docs/Web/API/Document/elementFromPoint)である。これは、vieportのx,y座標の位置の要素を取得できるAPIである。これと[MouseEvent](https://developer.mozilla.org/en/docs/Web/API/MouseEvent)のclientX,clientYを組み合わせれば、マウスポインタの座標の要素を取得できる。このAPIを使って、ある要素の背後の要素を取得するには次のようにする。

```coffeescript
getUnderElement = (target, clientX, clientY)->
  tmp = target.style.display
  target.style.display = "none"
  under = document.elementFromPoint(clientX, clientY)
  target.style.display = tmp
  under
```

まず、マウスイベントが発生した要素`target`のCSSの[display](https://developer.mozilla.org/en/docs/Web/CSS/display)プロパティを`none`にしてレンダリングツリーから存在を消す。
そうしておいて、`document.elementFromPoint(x, y)`をすることで、その座標における、直下の要素を取得できる。
これは今回の目的に使えそうである。

> 参考:
> "What is the difference between screenX/Y, clientX/Y and pageX/Y?"
> http://stackoverflow.com/questions/6073505/what-is-the-difference-between-screenx-y-clientx-y-and-pagex-y

## canvasのある座標が透明かどうか判定する

次に、canvasのマウスイベントが発生したその座標において、透明であるかそうでないかを判定する方法を考える。これにはcanvas要素のalpha channelを調べればよい。

```coffeescript
isHit = (cnv, x, y)->
  ctx = cnv.getContext("2d")
  imgdata = ctx.getImageData(0, 0, x + 1, y + 1)
  imgdata.data[imgdata.data.length - 1] isnt 0
```

対象ピクセルの1x1のimagedataからalpha channelを読み込み、その領域が透明であるか判定できる。


## 直下の要素にマウスイベントを投げる

`MouseEvent`コンストラクタと[EventTarget#dispatchEvent](https://developer.mozilla.org/en/docs/Web/API/EventTarget)メソッドを使うことで、現在のマウスイベントの値を持った新しいマウスイベントを生成して、直下の要素`underElement`に対してマウスイベントを発火させることができる。

```coffeescript
ev.preventDefault()
ev.stopPropagation()
mev = new MouseEvent ev.type,
  screenX: ev.screenX
  screenY: ev.screenY
  clientX: ev.clientX
  clientY: ev.clientY
  ctrlKey: ev.ctrlKey
  altKey:  ev.altKey
  shiftKey:ev.shiftKey
  metaKey: ev.metaKey
  button:  ev.button
  buttons: ev.buttons
  relatedTarget: ev.relatedTarget
  view:    ev.view
  detail:  ev.detail
  bubbles: false
underElement.dispatchEvent(mev);
```

ここまでが問題解決に必要な技術である。


## 複数枚重なったcanvasに対応する

canvasが複数枚重なっている場合、上の手法をそのまま使うだけでは、見えている要素にはイベントが発生しない。先ほど作った`getUnderElement`関数を思い出してほしい。

```coffeescript
getUnderElement = (target, x, y)->
  tmp = target.style.display
  target.style.display = "none"
  under = document.elementFromPoint(x, y)
  target.style.display = tmp
  under
```

返り値を渡す前に`target.style.display = tmp`をして、レンダリングツリーに再表示している。これでは複数枚canvasが重なっている場合、１枚目と２枚目が互いにイベントを発生しあい、それより下にイベントが通知されなくなる。そのため、一回のイベント内で、再帰的に直下要素がcanvasであるかどうか、また、そのcanvasのその位置が透明であるかを判定する必要がある。そして、直下の要素がcanvasかつ不透明ならその当たりと判定し、透明かつcanvas以外の要素であったらマウスイベントを発火させる。

説明するのが面倒くさくなってきたので、最終的なコードだけ示す。

```coffeescript
ids = ["cnv1", "cnv2", "cnv3"]
cnvs = ids.map (id)-> document.getElementById(id)
cnvs.forEach (cnv)-> cnv.width = cnv.height = 100
ctxs = cnvs.map (cnv)-> cnv.getContext("2d")
colors = ["red", "green", "blue"]

ctxs.forEach (ctx, i)->
  ctx.beginPath()
  ctx.arc(cnvs[i].width/2, cnvs[i].height/2, cnvs[i].width/4, 0, 2*Math.PI, true)
  ctx.closePath()
  ctx.fillStyle = colors[i%colors.length]
  ctx.fill()
  ctx.beginPath()
  ctx.rect(0, 0, 100, 100);
  ctx.closePath()
  ctx.stroke();

cnvs.forEach (cnv)->
  console.log cnv.id, $(cnv).offset()

$("#wrapper").on "click",  (ev)->
  console.log "wrapper" if ev.target is @

$("#wrapper").delegate "canvas", "click", (ev)->
  hit = recursiveElementFromPoint(ev, wrapper, ev.target)
  return unless hit?
  console.log "circle", hit

$("#rect").on "click", (ev)->
  console.log "rect"

recursiveElementFromPoint = (ev, parent, target)->
  {clientX, clientY, pageX, pageY} = ev
  {left, top} = $(target).offset()
  [offsetX, offsetY] = [pageX - left, pageY - top]
  if [].slice.call(wrapper.children).indexOf(target) > -1 and
     target instanceof HTMLCanvasElement and
     isHit(target, offsetX, offsetY)
    ev.preventDefault()
    ev.stopPropagation()
    return target
  tmp = target.style.display
  target.style.display = "none"
  under = document.elementFromPoint(clientX, clientY)
  unless under?
    target.style.display = tmp
    return null 
  if [].slice.call(wrapper.children).indexOf(under) > -1 and
     under instanceof HTMLCanvasElement
    result = recursiveElementFromPoint(ev, wrapper, under) # display:noneしたまま再帰的に直下要素を調べる。ここがミソ
    target.style.display = tmp
    return result
  target.style.display = tmp
  ev.preventDefault()
  ev.stopPropagation()
  eventPropagationSim(under, ev)
  return null

isHit = (cnv, x, y)->
  ctx = cnv.getContext("2d")
  imgdata = ctx.getImageData(0, 0, x + 1, y + 1)
  imgdata.data[imgdata.data.length - 1] isnt 0

eventPropagationSim = (target, ev)->
  if ev.type is "click" or
     /^mouse/.test(ev.type)
    ev.preventDefault()
    ev.stopPropagation()
    mev = new MouseEvent ev.type,
      screenX: ev.screenX
      screenY: ev.screenY
      clientX: ev.clientX
      clientY: ev.clientY
      ctrlKey: ev.ctrlKey
      altKey:  ev.altKey
      shiftKey:ev.shiftKey
      metaKey: ev.metaKey
      button:  ev.button
      buttons: ev["buttons"]
      relatedTarget: ev.relatedTarget
      view:    ev["view"]
      detail:  ev["detail"]
      bubbles: false
    target.dispatchEvent(mev);
  else
   console.log(ev.type)
  return
```


