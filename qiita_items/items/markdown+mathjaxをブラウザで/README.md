# やりたいこと

```markdown
## 波動方程式

$$
\frac{\partial^2 p}{\partial t^2} = c^2 \nabla^2 p
$$

```
こういうmarkdownファイルを xhr でとってきて

![波動方程式](https://i.gyazo.com/4f6951726e3bdde2ff9641424a04e819.png)

数式とmarkdownを動的にレンダリングしたい。

# 方針
XHRで取ってきた.mdファイルをmarkedでhtmlに変換した後bodyに挿入しmathjax発動

`location.hash`を使ってルーティングも

```html
<!DOCTYPE html>
<html lang="ja">
<head>
  <meta charset="UTF-8" />
  <link rel="stylesheet" type="text/css" href="github-markdown.css" />
  <script src="https://cdnjs.cloudflare.com/ajax/libs/jquery/3.0.0-alpha1/jquery.min.js"></script>
  <script src="https://cdnjs.cloudflare.com/ajax/libs/marked/0.3.5/marked.min.js"></script>
  <script type="text/x-mathjax-config">
    MathJax.Hub.Config({
      skipStartupTypeset: true, // ページロード時のmathjax発動禁止
      extensions: ["tex2jax.js"],
      jax: ["input/TeX", "output/HTML-CSS"],
      tex2jax: {
        inlineMath: [ ['$','$'], ["\\(","\\)"] ],
        displayMath: [ ['$$','$$'], ["\\[","\\]"] ],
        processEscapes: true
      },
      "HTML-CSS": {
        availableFonts: ["TeX"]
      }
    });
  </script>
  <script src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.5.3/MathJax.js?config=TeX-AMS-MML_HTMLorMML&delayStartupUntil=configured"></script>
</head>
<body>
  <div id="content" class="markdown-body"></div>
  <footer>
    <a href="javascript:history.back();">Back</a>
  </footer>
<script>
window.addEventListener("DOMContentLoaded", main);
window.addEventListener("hashchange", main);

var get = $.get;
marked.setOptions({
  renderer: new marked.Renderer(),
  gfm: true,
  tables: true,
  breaks: true,
  pedantic: false,
  sanitize: false,
  smartLists: false,
  smartypants: false
});

function main(){
  // ルーティング
  var name = "";
  if( location.hash.length <= 1 ){
    name = "index.md";
    location.hash = "#" + name;
  }else{
    name = location.hash.slice(1);
  }
  // ページ内リンクなのでhistory.pushStateする必要はない
  get(name).catch(function(){
    return Promise.resolve("# 404 page not found");
  }).then(function(text){
    // markedにlatexタグ食わせると&<>とかがエスケープされるので<pre />で包んで退避
    // ちなみに```mathとかで<pre><code class="lang-math">になったのはエスケープされるので注意
    var PREFIX = "<pre><code class=\"lang-math\">";
    var SUFFIX = "</code></pre>";
    var reg = new RegExp(
      ("(?:" + PREFIX + "([\\s\\S]*?)" + SUFFIX + ")")
        .replace(/\//g, "\/"),
      "gm");
    var wraped = text.split("$$")
      .reduce(function(sum, str, i){
        return i % 2 === 0 ?
               sum + str :
               sum + PREFIX + str + SUFFIX;
      }, "");
    var html = marked(wraped);
    // 退避したlatexタグを$$で包み直す
    var _html = html;
    var tuple = null;
    while(tuple = reg.exec(html)){
      _html = _html.split(tuple[0]).join("$$" + tuple[1] + "$$");
    }
    // mathjaxで処理
    var div = document.getElementById("content");
    div.innerHTML = _html;
    MathJax.Hub.Configured();
    MathJax.Hub.Queue(["Typeset", MathJax.Hub, div]);
  }).catch(function(err){
    // jqueryのpromiseはthenの中でエラー吐いて止まってもconsoleに表示してくれないので表示させる
    console.error(err);
  });
}
</script>
</body>
</html>
```

# まめちしき
* github大好きマンはmarkdownのCSSに[github-markdown-css](https://github.com/sindresorhus/github-markdown-css)を使うと吉。
* [marked](https://github.com/chjj/marked)なら`marked.setOptions({gfm: true});`とすれば[github-flavored-markdown](https://help.github.com/articles/github-flavored-markdown/)が使える。
