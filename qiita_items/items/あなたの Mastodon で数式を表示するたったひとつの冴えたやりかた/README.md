# あなたの Mastodon で数式を表示するたったひとつの冴えたやりかた

react で差分管理されている Mastodon の新着ツイートのみ Mathjax でレンダリングする方法です。

react による DOM の追加を `MutationObserver` を使ってフックします。

## 1.CSP を設定する

```
Content-Security-Policy: script-src https://cdnjs.cloudflare.com;
```

## 2.以下の JavaScript を home のページ末尾に読み込ませる

```html
<script>
(()=>{
  window.MathJax = {
    showProcessingMessages:false,
    messageStyle:"none",
    showMathMenu:false,
    skipStartupTypeset:true,
    texHints:false,
    extensions:["tex2jax.js"],
    jax:["input/TeX","output/CommonHTML"],
    tex2jax:{
      inlineMath:[["$","$"],["\\(","\\)"]],
      displayMath:[["$$","$$"],["\\[","\\]"]],
      processEscapes:true
    },
    "HTML-CSS":{availableFonts:["TeX"]},
    TeX:{extensions:["AMSmath.js","AMSsymbols.js"]}
  };
  const urls = [
    "https://cdnjs.cloudflare.com/ajax/libs/zepto/1.2.0/zepto.min.js",
    "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS_HTML"
  ];
  urls.reduce((next, url)=> ()=> loadScript(url, next), ()=> $(loaded))();
  function loaded(){
    MathJax.Hub.Queue(["Typeset", MathJax.Hub, ".status"]);
    MathJax.Hub.Queue(["Typeset", MathJax.Hub, ".status__content"]);
    MathJax.Hub.Queue([()=> new MutationObserver(changed).observe(document.body,{childList:1,subtree:1}) ]);
  }
  let rendering = false;
  function changed(diffs){
    if(rendering){ return; }
    MathJax.Hub.Queue([()=>{ rendering = true; }]);
    diffs.forEach(diff =>{
      Array.from(diff.addedNodes).forEach(elm =>{
        if($(elm).hasClass("status") || elm instanceof HTMLParagraphElement){
          MathJax.Hub.Queue(["Typeset", MathJax.Hub, elm]);
        }else if(elm instanceof Text){
          MathJax.Hub.Queue(["Typeset", MathJax.Hub, diff.target]);
        }else if($(elm).hasClass("column")){
          $(elm).find(".status__content").each((i, elm)=>{
            MathJax.Hub.Queue(["Typeset", MathJax.Hub, elm]);  
          });
        }
      });
    });
    MathJax.Hub.Queue([()=>{ rendering = false; }]);
  }
  function loadScript(url, callback){
    let script = document.createElement("script");
    script.src = url;
    script.onload = callback;
    document.head.appendChild(script)
  }
})();
</script>
```

Mathjax の設定はお好みで。

## Appendix. Mathjax で xy-pic のような可換図を書く

AmS ではかけない複雑な可換図は https://github.com/sonoisa/XyJax を使ってください。

