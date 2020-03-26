node.js で[treeコマンド](http://qiita.com/satomyumi/items/8891117ed4df42d1cdcc)のように再帰的にディレクトリを表示したいとする。
標準ライブラリである[fs](https://nodejs.org/api/fs.html)では[POSIX API](https://ja.wikipedia.org/wiki/POSIX)しか呼べないため、fsだけで一から作るとなると意外に面倒である。

鍵となるのは [fs.lstat](https://nodejs.org/api/fs.html#fs_fs_lstat_path_callback) と [fs.readdir](https://nodejs.org/api/fs.html#fs_fs_readdir_path_options_callback)、そして[Class: fs.Stats](https://nodejs.org/api/fs.html#fs_class_fs_stats)である。

`fs.stat`と`fs.lstat`と`fs.fstat`の違いについては[Man page of STAT](https://linuxjm.osdn.jp/html/LDP_man-pages/man2/stat.2.html)に次のようにある。


> stat() と fstatat() は pathname が指すファイルに関する情報を取得する。 fstatat() の違いについては後で説明する。
> lstat() は stat() と同じであるが、 pathnames がシンボリックリンクの場合、
リンクが参照しているファイルではなく、 リンク自身の状態を返す点が異なる。
> fstat() は stat() と同じだが、 状態を取得するファイルをファイルディスクリプター fd で指定する点が異なる。

今回はシンボリックリンクを返す`lstat`を使うことにする。

## サンプルコード

```js
var fs = require("fs");
var path = require("path");

// fs APIはコールバックを取るので現代的にPromiseに変換する
// function asynchronous<T, U, V>(fn: (...args: T)=> U, ctx: V): (...args: T)=> Promise<U>
function asynchronous(fn, ctx){
  return function _asyncFn(){
    var args = Array.prototype.slice.call(arguments);
    return new Promise(function(resolve, reject){
      fn.apply(ctx, args.concat(function(err, val){
        if(err){
          reject(err);
        }else{
          resolve(val);
        }
      }));
    });
  };
}

// function readdirAsync(path: string|Buffer, opt?: "utf8"|{encoding: "utf8"}): Promise<[string]>
fs.readdirAsync = asynchronous(fs.readdir, fs);
// function lstatAsync(path: string|Buffer): Promise<fs.Stats>
fs.lstatAsync = asynchronous(fs.lstat, fs);


// function ls(pathname: string): Promise<[{name: string, stat: fs.Stats}]>
function ls(pathname){
  return fs.readdirAsync(pathname)
  .then(function(names){
    return Promise.all(
      names.map(function(name){
        return fs.lstatAsync(path.join(pathname, name))
        .then(function(stat){
          return {name: name, stat: stat};
        });
      })
    )
  });
}

// fs.Statクラスの判別
// type FileType = "file" | "dir" | "blcdev" | "chardev" | "symlink" | "fifo" | "socket" | "unkown"
// function getFileType(stat: fs.Stats): FileType
function getFileType(stat){
  return stat.isFile() ? "file"
       : stat.isDirectory() ? "dir"
       : stat.isBlockDevice() ? "blcdev"
       : stat.isCharacterDevice() ? "chardev"
       : stat.isSymbolicLink() ? "symlink"
       : stat.isFIFO() ? "fifo"
       : stat.isSocket() ? "socket"
       : "unkown";
}

// type Dir = {[name: string]: FileType | Dir }
// tree(pathname: string): Promise<Dir>
function tree(pathname){
  return ls(pathname)
  .then(function(elms){
    return Promise.all(
      elms.map(function(elm){
        if(elm.stat.isDirectory()){
          return tree(path.join(pathname, elm.name))
          .then(function(dir){
            return {name: elm.name, type: dir};
          });
        }
        return {name: elm.name, type: getFileType(elm.stat)};
      })
    )
  })
  .then(function(elms){
    return elms.reduce(function(o, elm){
      o[elm.name] = elm.type;
      return o;
    }, {});
  })
}

tree(".")
.then(function(o){return JSON.stringify(o, null, "  ");})
.then(console.log.bind(console))
.catch(console.error.bind(console));
```

## 感想

ろくに型の書いてない公式ドキュメントは万死に値する。

