## あらかじめ読んでおくもの

* [JavaScriptでGitを実装するKickstarterプロジェクト、28時間で資金調達](http://www.infoq.com/jp/news/2013/04/git-in-javascript)
* [Git の仕組み (1)](http://koseki.hatenablog.com/entry/2014/04/22/inside-git-1)
* [Git の仕組み (2)](http://koseki.hatenablog.com/entry/2014/06/11/inside-git-2)
* [Gitの内側](http://git-scm.com/book/ja/Git%E3%81%AE%E5%86%85%E5%81%B4)

## js-gitとは

gitをjavascriptで実装しちゃった[js-git](https://github.com/creationix/js-git)。

わざわざgitをjavascriptで実装したからにはブラウザで使うでしょう？

js-gitは「なんらかの形で表現された.gitディレクトリ」の中身をあれこれするためのAPI集です。

この「なんらかの形で表現された.gitディレクトリ」には、[オンメモリ](https://github.com/creationix/js-git/blob/master/mixins/mem-db.js)とか[nodejsのfs標準ライブラリのラッパ](https://github.com/creationix/js-git/blob/master/mixins/fs-db.js)とか[indexedDB](https://github.com/creationix/js-git/blob/master/mixins/indexed-db.js)とか[WebSQL](https://github.com/creationix/js-git/blob/master/mixins/websql-db.js)とかいろいろあります。[DOMStorage](https://developer.mozilla.org/ja/docs/DOM/Storage)系がないのが気になりますが[あんな容量](http://dev-test.nemikor.com/web-storage/support-test/)でローカルリポジトリとして使うのは難しいでしょう。localStorage使いたかったらlocalStorage-dbをご自身で書いてみるのも面白いでしょう。また、どこかのクラウドストレージのAPI叩いて保存するものも作れそうですね。

これだけあればブラウザからでも使えそうです。


## browserifyでブラウザから使う

requireで依存関係処理しているのでbrowserifyでサクっとまとめましょう。

### main.js
```js

// This provides symbolic names for the octal modes used by git trees.
window.modes = require('./lib/modes');

// Create a repo by creating a plain object.
window.repo = {};

// This provides an in-memory storage backend that provides the following APIs:
// - saveAs(type, value) => hash
// - loadAs(type, hash) => hash
// - saveRaw(hash, binary) =>
// - loadRaw(hash) => binary
require('./mixins/mem-db')(repo);

// This adds a high-level API for creating multiple git objects by path.
// - createTree(entries) => hash
require('./mixins/create-tree')(repo);

// This provides extra methods for dealing with packfile streams.
// It depends on
// - unpack(packStream, opts) => hashes
// - pack(hashes, opts) => packStream
require('./mixins/pack-ops')(repo);

// This adds in walker algorithms for quickly walking history or a tree.
// - logWalk(ref|hash) => stream<commit>
// - treeWalk(hash) => stream<object>
require('./mixins/walkers')(repo);

// This combines parallel requests for the same resource for effeciency under load.
require('./mixins/read-combiner')(repo);

// This makes the object interface less strict.  See it's docs for details
require('./mixins/formats')(repo);

```

### index.html
```html
<script src="bundle.js"></script>
```

### とりあえず動かす
```bash
$ browserify main.js -o bundle.js
$ open index.html
```

Chromeが開いたと思うのでWebInspectorのコンソールからrepoの中身をのぞいてみてください。

### コミットしてみる

[このへん](https://github.com/creationix/js-git#basic-object-creation)にコミットの仕方載ってますね。

yieldとか恐ろしげなワードを使ってますがcallbackパターンも問題なく使えるようです。
以下はcallbackパターンでのコミットの例です

```js

// First we create a blob from a string.  The `formats` mixin allows us to
// use a string directly instead of having to pass in a binary buffer.
repo.saveAs("blob", "Hello World\n", function(err, blobHash){
  console.log(err, blobHash);

  // Now we create a tree that is a folder containing the blob as `greeting.txt`
  repo.saveAs("tree", {
    "greeting.txt": { mode: modes.file, hash: blobHash }
  }, function(err, treeHash){
    console.log(err, treeHash);
    
    // With that tree, we can create a commit.
    // Again the `formats` mixin allows us to omit details like committer, date,
    // and parents.  It assumes sane defaults for these.
    repo.saveAs("commit", {
      author: {
        name: "Tim Caswell",
        email: "tim@creationix.com"
      },
      tree: treeHash,
      message: "Test commit\n"
    }, function(err, commitHash){
      console.log(err, commitHash);
    });
  });
});
```

ネストがえらいこっちゃ。


### コミット読み込む

[このへん](https://github.com/creationix/js-git#basic-object-loading)に載ってます。

yield使ってますが先述の通りcallback(err, result)パターンが使えます。

気をつけるところはblobなgitオブジェクトを読み込むとbyte arrayで返ってくるところでしょうか。

```js
repo.saveAs("blob", "Hello World\n", function(err, blobHash){
  console.dir(blobHash);
  repo.loadAs("blob", blobHash, function(err, byteArray){
    console.log(byteArray);// -> [72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100, 10]
  });
});
```

treeなgitオブジェクトの拡張子みてバイナリの読み方判断しないといけませんね。

### 


## 開発状況
2014-09-09現在、ごらんのありさまだよ！

[![js-git milestone](http://i.gyazo.com/c2f6bf78ea17e36d7a02bc31fec7d0bf.png)](https://github.com/creationix/js-git/milestones)

２週間ほど前からようやく書き込みAPIの整備がされてきたようです。

また、リモートリポジトリとのやりとりも実装される見込みのようです。

熱いですね！


