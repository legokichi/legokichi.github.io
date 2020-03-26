# Emscripten File System API でディレクトリツリーを表示する

[大本営](https://kripken.github.io/emscripten-site/docs/api_reference/Filesystem-API.html) にはごく一部のAPIしか載っていないので[ソースコードを読む](https://github.com/kripken/emscripten/blob/incoming/src/library_fs.js)かコンソールで `FS` に生えているメソッドを確認する必要がある。

## コード

```js
function ls(pathname){
  var names = FS.readdir(pathname);
  return names.map(function(name){
    var stat = FS.lstat(pathname +"/"+ name);
    return {name: name, stat: stat};
  });
}

function getFileType({mode}){
  return FS.isFile(mode) ? "file"
       : FS.isDir(mode) ? "dir"
       : FS.isBlkdev(mode) ? "blcdev"
       : FS.isChrdev(mode) ? "chardev"
       : FS.isLink(mode) ? "symlink"
       : FS.isFIFO(mode) ? "fifo"
       : FS.isSocket(mode) ? "socket"
       : "unkown";
}

function tree(pathname){
  var elms = ls(pathname).map(function(elm){
    if(elm.name !== "." && elm.name !== ".." && getFileType(elm.stat) === "dir"){
      var dir = tree(pathname +"/"+ elm.name);
      return {name: elm.name, type: dir};
    }
    return {name: elm.name, type: getFileType(elm.stat)};
  });
  return elms.reduce(function(o, elm){
    o[elm.name] = elm.type;
    return o;
  }, {});
}


FS.chdir("/home/web_user");
FS.mkdir('working');
FS.mount(MEMFS, { root: '.' }, 'working');
FS.chdir("working");

console.log(JSON.stringify(tree("/home"), null, "  "));
```

## 結果

```json
{
  ".": "dir",
  "..": "dir",
  "web_user": {
    ".": "dir",
    "..": "dir",
    "working": {
      ".": "dir",
      "..": "dir"
    }
  }
}
```
## 参考
* https://kripken.github.io/emscripten-site/docs/api_reference/Filesystem-API.html
* http://qiita.com/DUxCA/items/10efc3b74ede2f91f3c7
* http://qiita.com/DUxCA/items/5b725cb6359003c53171
