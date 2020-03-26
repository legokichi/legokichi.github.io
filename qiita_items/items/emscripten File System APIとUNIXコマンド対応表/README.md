# emscripten File System APIとUNIXコマンド対応表

## pwd

[FS.cwd](https://kripken.github.io/emscripten-site/docs/api_reference/Filesystem-API.html#FS.cwd)

```js
FS.cwd(); // -> "/"
```

## ls

[FS.lookupPath](https://kripken.github.io/emscripten-site/docs/api_reference/Filesystem-API.html#FS.lookupPath)

```js
Object.keys(FS.lookupPath("/").node.contents); // -> ["tmp", "home", "dev", "proc"]
```

### ファイルの詳しい情報

```js
var contents = FS.lookupPath("/").node.contents;
Object.keys(contents).map(function(name){
  var o = contents[name];
  var attr = FS.stat(o);
  var mode = attr.mode;
  var size = attr.size;
  var ctime = attr.ctime;
  var uid = attr.uid;
  var gid = attr.gid;
  return [
    FS.isFile(mode),
    FS.isDir(mode),
    FS.isLink(mode),
    FS.isFIFO(mode),
    FS.isChrdev(mode),
    FS.isBlkdev(mode),
    FS.isSocket(mode),
    new Date(ctime).toJSON()
  ];
});
```

## cd

[FS.chdir](https://github.com/kripken/emscripten/blob/incoming/src/library_fs.js#L1213)

```js
FS.chdir("/home");
```

## mkdir

[FS.mkdir](https://kripken.github.io/emscripten-site/docs/api_reference/Filesystem-API.html#FS.mkdir)

```js
FS.mkdir("web_user");
```

## rmdir

[FS.rmdir](https://kripken.github.io/emscripten-site/docs/api_reference/Filesystem-API.html#FS.rmdir)

```js
FS.rmdir("web_user");
```

## mv

[FS.rename](https://kripken.github.io/emscripten-site/docs/api_reference/Filesystem-API.html#FS.rename)

```js
FS.rename(oldpath, newpath)
```

## rm

[FS.unlink](https://kripken.github.io/emscripten-site/docs/api_reference/Filesystem-API.html#FS.unlink)

```js
FS.unlink("hoge.txt");
```

## ファイル書き込み

[FS.writeFile](https://kripken.github.io/emscripten-site/docs/api_reference/Filesystem-API.html#FS.writeFile)

### バイナリ書き込み

```js
var somedata = new Uint8Array(something);
FS.writeFile("./hoge.png", somedata, {encoding: "binary"});
```

### テキスト書き込み

```js
FS.writeFile("./hoge.txt", "hello world");
```

### 追加書き込み

```js
FS.writeFile("./hoge.txt", "hello world!", {flags: "a"});
```

## ファイル読み込み

[FS.readFile](https://kripken.github.io/emscripten-site/docs/api_reference/Filesystem-API.html#FS.readFile)

### バイナリ読み込み

```js
FS.readFile("hoge.png"); // -> Uint8Array
```

### テキスト読み込み

```js
FS.readFile("hoge.txt", {encoding: "utf8"}); // -> "hello wolrd"
```
