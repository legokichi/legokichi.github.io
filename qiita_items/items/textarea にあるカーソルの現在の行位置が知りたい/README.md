# textarea にあるカーソルの現在の行位置が知りたい

`HTMLTextAreaElement.prototype.selectionEnd` を使うと text 上のカーソルの文字位置がわかる。
いくつめの改行の区間に入っているのかを調べれば良い。

```js
const $textarea = $("textarea#someone");

document.addEventListener('keyup', function(ev){
  const pos = $textarea.get(0).selectionEnd;
  const text = $textarea.val();
  const [linePos, lineAll] = getCursorLinePos(pos, text);
  console.log(linePos, "行目", (linePos-1) / (lineAll-1) * 100, "%");
});

function getCursorLinePos(pos, text){
  const lines = text.split("\n");
  const charCounts = lines.map((line)=> line.length+1);
  if(charCounts.length>0 && charCounts[0]>0){ charCounts[0]-=1; }
  let cur = 0;
  let sum = 0;
  for(let i=0; i<charCounts.length; i++){
    sum += charCounts[i];
    if(pos <= sum){
      cur = i+1;
      break;
    }
  }
  return [cur, lines.length];
}
```
