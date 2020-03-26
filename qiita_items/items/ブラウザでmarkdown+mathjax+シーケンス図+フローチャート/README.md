[markdown+mathjaxをブラウザで](http://qiita.com/DUxCA/items/27b7b865a0ab28b5d530)の続き。

[Markdownテキストでシーケンス図とフローチャートを描く](http://qiita.com/ka215/items/a709665cb34c505ccf1f)
に触発されたので。

```html
<!DOCTYPE html>
<html lang="ja">
<head>
  <meta charset="UTF-8" />
  <link rel="stylesheet" type="text/css" href="./github-markdown.css" />
  <script src="https://cdnjs.cloudflare.com/ajax/libs/jquery/3.0.0-alpha1/jquery.min.js"></script>
  <script src="https://cdnjs.cloudflare.com/ajax/libs/marked/0.3.5/marked.min.js"></script>
  <script src="https://cdnjs.cloudflare.com/ajax/libs/raphael/2.1.4/raphael-min.js"></script>
  <script src="https://cdnjs.cloudflare.com/ajax/libs/lodash.js/3.10.1/lodash.min.js"></script>
  <script src="https://cdnjs.cloudflare.com/ajax/libs/js-sequence-diagrams/1.0.6/sequence-diagram-min.js"></script>
  <script src="./flowchart.min.js"></script>
  <script type="text/x-mathjax-config">
    MathJax.Hub.Config({
      skipStartupTypeset: true,
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
  <hr />
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
MathJax.Hub.Configured();

function main(){
  var name = "";
  if( location.hash.length <= 1 ){
    name = "index.md";
    location.hash = "#" + name;
  }else{
    name = location.hash.slice(1);
  }
  get(name).catch(function(){
    return "# 404 page not found";
  }).then(function(text){
    var PREFIX = "\n<pre><code class=\"lang-math\">";
    var SUFFIX = "</code></pre>\n";
    var reg = /\$\$([\s\S]+?)\$\$/gm;
    var tuple = null;
    var _text = text;
    while(tuple = reg.exec(text)){
      _text = _text.replace(tuple[0], PREFIX + tuple[1] + SUFFIX);
    }
    return _text;
  }).then(function(text){
    var PREFIX = "<span class=\"lang-math-inline\">```";
    var SUFFIX = "```</span>";
    var reg = /\$([^\r\n]+?)\$/g;
    var _text = text;
    var tuple = null;
    while(tuple = reg.exec(text)){
      _text = _text.replace(tuple[0], PREFIX + tuple[1] + SUFFIX);
    }
    return _text;
  }).then(function(text){
    var html = marked(text);
    $("#content").html(html);
  }).then(function(){
    $(".lang-math-inline").map(function(i, elm){
      console.log(elm);
      var code = $(elm).children().html()
        .replace(/\&amp;/g, "&")
        .replace(/\&lt;/g, "<")
        .replace(/\&gt;/g, ">");
      $(elm).after("$" + code + "$").remove();
    });
    $(".lang-math").map(function(i, elm){
      var code = $(elm).html()
        .replace(/\&amp;/g, "&")
        .replace(/\&lt;/g, "<")
        .replace(/\&gt;/g, ">");
      $(elm).parent().after("$$" + code + "$$").remove();
    });
    MathJax.Hub.Queue(["Typeset", MathJax.Hub, document.body]);
  }).then(function(){
    $(".lang-sequence").map(function(i, elm){
      var code = $(elm).html()
        .replace(/\&amp;/g, "&")
        .replace(/\&lt;/g, "<")
        .replace(/\&gt;/g, ">");
      var div = document.createElement("div");
      $(elm).parent().after(div).remove();
      Diagram.parse(code).drawSVG(div, {theme: "simple", scale: 1});
    });
  }).then(function(){
    $(".lang-flow").map(function(i, elm){
      var code = $(elm).html()
        .replace(/\&amp;/g, "&")
        .replace(/\&lt;/g, "<")
        .replace(/\&gt;/g, ">");
      var div = document.createElement("div");
      $(elm).parent().after(div).remove();
      flowchart.parse(code).drawSVG(div);
    });
  }).catch(function(err){
    console.error(err, err.stack);
  });
}
</script>
</body>
</html>
```
