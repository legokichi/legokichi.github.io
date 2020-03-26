https://github.com/twada/power-assert-demo の通りに
qunitとpower-assertを組み合わせると表示が崩れてしまいます。

![](http://i.gyazo.com/aa3b42b2811ed99653f780a7305f7a54.png)

そこで以下のようにスタイルを設定します。

```html
<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8" />
  <title>QUnit Test Suite</title>
  <link  href="../bower_components/qunit/qunit/qunit.css" rel="stylesheet" />
  <script src="../bower_components/qunit/qunit/qunit.js"></script>
  <script src="../bower_components/qunit-tap/lib/qunit-tap.js"></script>
  <script>qunitTap(QUnit, function() { console.log.apply(console, arguments); }, {showSourceOnFailure: false});</script>

  <script src="../bower_components/empower/build/empower.js"></script>
  <script src="../bower_components/power-assert-formatter/build/power-assert-formatter.js"></script>
  <script>empower(QUnit.assert, powerAssertFormatter({lineSeparator:"\n"}), {destructive: true});</script>
  <style>
    .test-message{
      white-space: pre;
      font-family: Osaka-mono, "Osaka-等幅", "ＭＳ ゴシック", monospace;
      font-size: 14px;
    }
  </style>

  <script src="../src/mylibrary.js"></script>

  <script src="../test/testMylibrary.espowered.js"></script>
</head>
<body>
  <h1 id="qunit-header">QUnit Test Suite</h1>
  <h2 id="qunit-banner"></h2>
  <div id="qunit-testrunner-toolbar"></div>
  <h2 id="qunit-userAgent"></h2>
  <ol id="qunit-tests"></ol>
  <div id="qunit-fixture">test markup</div>
</body>
</html>
```

![](http://i.gyazo.com/98fabb452b1c6bfc6fce7456fb2c1bff.png)

うまくいきました。

