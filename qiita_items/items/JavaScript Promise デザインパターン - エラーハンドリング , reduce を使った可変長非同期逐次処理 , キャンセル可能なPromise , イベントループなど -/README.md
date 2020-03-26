
# JavaScript Promise デザインパターン - reduce を使った可変長非同期逐次処理 , キャンセル可能なPromise など -

## Promise の中では `throw` してはならない
このコードは何が表示されるだろうか？

```ts
new Promise((resolve)=>{
  setTimeout(()=>{
    throw new Error("foo");
    resolve("bar");
  });
})
.then((a)=>{ alert(a); })
.catch((err)=>{ alert(err); });
```

答えは __何も起きない__ である。

setTimeout の中の関数のような非同期実行コンテキストで throw したエラーはグローバルエラーとなるため
`window.onerror` や `process.on('uncaughtException', (err) => {})` でしか拾うことができない。

代わりに `reject` を使って

```ts
new Promise((resolve, reject)=>{
  setTimeout(()=>{
    return reject("foo");
    resolve("bar");
  });
})
.then((a)=>{ alert(a); })
.catch((err)=>{ alert(err); });
```

のように書かなければならない。

ちなみに __Promise の引数関数 `(resolve, reject)=>{}` は同期的に実行される__ ため、

```ts
console.log("a");
new Promise((resolve)=>{
  console.log("b");
  throw new Error("foo");
  resolve("bar");
})
.then((a)=>{ alert(a); })
.catch((err)=>{ alert(err); });
```

は `a`, `b` が表示されてから `alert(err)` が発生し、問題なくエラーハンドリングできる。

しかし、ややこしいため __Promise の中では throw は使わず必ず reject する__ ことを心がけるべきである。

## `new Promise(resolve =>{ ... })`　というコードは書かない。

__`new Promise(resolve =>{ ... })` というコードを見かけたら要注意である__ 。
そのコードはエラーハンドリングがされていない可能性が濃厚だからである。
reject されない Promise はエラーを catch することはできない。デバッグなどほとんど不可能なコードになってしまう。

あなたのリポジトリも `git grep "new Promise"` で resolve だけPromise がないかチェックしてみよう。

例外的に失敗がありえない非同期処理(例: setTimeoutなど)は resolve だけでも良い。

## 投げっぱなしの非同期処理はコメントに書いておく

処理の失敗を気にせず投げっぱなしの非同期処理を書きたいときもある(例: サーバにテレメトリを定期的に投げる)。

その場合はコメントで __この非同期処理は投げっぱなしであることを明記__ すべきである

```ts
setInterval(()=>{
  // 定期的にサーバにアクセスして生存報告するがフロント側では特になにもしない
  post("http://api.foo.com/post")
    .then(()=>{/* 特に何もしない */})
    .catch((err)=>{/* うっとうしい */});
}, 10 * 1000);
```

## catch した 例外をさらに reject したい
次のように reject された Promise を catch してエラー理由を表示したい時があります。

```ts
Promise.reject(new Error("foo"))
  .catch((err)=>{ console.error(err); })
  .then((bar)=>{ console.log(bar); });
```

この `.catch((err)=>{ console.error(err); })` はエラーを表示した後、 `Promise<void>` を返すので
`.then((bar)=>{ console.log(bar); });` では `undefined` と表示してしまいます。

```ts
Promise.reject(new Error("foo"))
  .catch((err)=>{ console.error(err); return Promise.reject(err); })
  .then((bar)=>{ console.log(bar); });
```

こういう場合は `.catch((err)=>{ console.error(err); return Promise.reject(err); })` として catch の中であらためて reject 値を作りましょう


## 可変長の非同期タスクを逐次実行
```ts
function runAsyncSerialTasks(tasks: (()=> Promise<void>)[]): Promise<void> {
    return tasks.reduce((prm: Promise<void>, task: () => Promise<void>) =>
        prm.then(task), Promise.resolve(void 0));
}
```

reduce で `Promise.resolve(void 0).then(tasks[0]()).then(tasks[1]())...` という promise chain を作る。

### 例： `x-www-form-urlencoded` で順番にPOST
```ts
function toURLEncoded(o: {[key: string]: string}): string {
    return Object.keys(o).map((key)=> key+"="+encodeURIComponent(o[key])).join("&"); 
}
const urlAndDatas = [
  { url: "http://api.example.com/user/create", data: {hello: "world"}}
];
const urlAndDataStrs = urlAndDatas.map(({url, data})=> ({url, data: toURLEncoded(data)})) 
const tasks = urlAndDataStrs.map(({ url, data }) =>
    () =>
        fetch(url, {
            method: "POST",
            headers: {
                'Accept': 'application/json',
                'Content-Type': 'application/x-www-form-urlencoded; charset=utf-8'
            },
            body: data }).then(() => void 0));

runAsyncSerialTasks(tasks)
    .then(()=>{ console.log("finish"); })
    .catch((err) => { console.error(err); });
```

### 例：node.js で順番にファイルIO

```ts
function runAsyncSerialTasks(prmTasks: (()=> Promise<void>)[]): Promise<void> {
    return prmTasks.reduce((prm: Promise<void>, task: () => Promise<void>) =>
        prm.then(task), Promise.resolve(void 0));
}
const fs = require("fs");
const filenameAndContents = [
    { name: "foo.txt", content: "foo" },
    { name: "bar.txt", content: "bar" }
];
const tasks = filenameAndContents.map(({ name, content }) =>
    () =>
        new Promise<void>((resolve, reject) => {
            fs.writeFile(name, "utf8", content, (err) => {
                if (err != null) { return reject(err); }
                else { return resolve(void 0); }
            });
        })
);
runAsyncSerialTasks(tasks)
    .then(() => { console.log("finish"); })
    .catch((err) => { console.error(err); });
```

### 可変長の非同期タスクを逐次実行して結果も取得
Promise の結果も取得した場合。逐次GETや逐次ファイル読み込みなどの場合につかう。

```ts
function runAsyncSerialTasks<T>(tasks: (()=> Promise<T>)[]): Promise<T[]> {
    return tasks.reduce((prm, task) =>
        prm.then((lst)=> task().then((ret)=> lst.concat(ret))), Promise.resolve<T[]>([]));
}
```

reduce の中で `task().then((ret)=> lst.concat(ret))` として結果のリストを構成している。

```ts
runAsyncSerialTasks([
    () => new Promise((resolve) => resolve(0)),
    () => new Promise((resolve) => resolve(1)),
    () => new Promise((resolve) => resolve(2))
]).then((lst)=> console.log(lst))
```
結果 `[0,1,2]` と表示される。

### 問題点：リトライがしたい
`Promise.resolve(void 0).then(tasks[0]()).then(tasks[1]())...` という promise chain ではどれかひとつエラーが起きたときに残りのタスクが強制終了されてしまう。
せめてリトライがしたくなる。


## リトライ付きの可変長の非同期タスクを逐次実行

```ts
function runAsyncSerialTasksWithRetry(tasks: (() => Promise<void>)[], count = 0): Promise<void> {
    let i = 1;
    return tasks.reduce((prm: Promise<void>, task: () => Promise<void>) =>
        prm.then(function recur(){
          return task().catch((err)=>{
            if(i >= count){ return Promise.reject(err); }
            else {
                i += 1;
                return recur();
            }
          });
        }), Promise.resolve(void 0));
}
```

`runAsyncSerialTasks` 関数の引数にリトライ回数をつけ、reduce の中身を再帰的にリトライするようにした。

## 逐次実行中の Promise Chain をキャンセルしたい
JS の Promise は C# の Task とは違ってキャンセルできない。
そこで `reduce` の中で task がひとつ終わるごとに次を続けるかどうかを確認する処理を入れる。

```ts
function runCancelableAsyncSerialTasks(tasks: (() => Promise<void>)[]): { cancel: () => void, promise: Promise<void> } {
    const ret = {
        promise: Promise.resolve(void 0),
        cancel: () => { ret.canceled = true; },
        canceled: false
    };
    ret.promise = tasks.reduce((prm, task) =>
        prm
            .then(task)
            .then(() =>
                ret.canceled ? Promise.reject(new Error("canceled"))
                             : Promise.resolve(void 0) )
    , Promise.resolve(void 0));
    return ret;
}

const cancelable = runCancelableAsyncSerialTasks([
    () => new Promise((resolve) => setTimeout(() => { console.log("a"); resolve(); }, 1000)),
    () => new Promise((resolve) => setTimeout(() => { console.log("b"); resolve(); }, 1000)),
    () => new Promise((resolve) => setTimeout(() => { console.log("c"); resolve(); }, 1000)),
]);

cancelable.promise
    .then(() => { console.log("finish"); })
    .catch((err) => { console.error(err); });

setTimeout(() => { cancelable.cancel(); console.log("cancel"); }, 1100);
```

実行結果

```
a
cancel
b
Error: canceled
    at <anonymous>:11:50
    at <anonymous>
```

この場合 `b` の出力待ち中にキャンセルが入るので `b` が表示されてからキャンセルされる。
すでに実行中の `b` を止めることはできない。

こうなると非同期 task にもキャンセルを入れたくなってくる。


## キャンセル可能な Promise を作る

setTimeout を Promise 化する例はよく見られるが、やはり clearTimeout にも対応しつつ Promise で包みたい、ということは多々ある。

```ts
type CancelablePromise<T> = { promise: Promise<T>, cancel: () => void };

function cancelableSleep(ms: number): CancelablePromise<void> {
    let tid = 0;
    let _reject: (err: Error)=> void;
    const ret = {
        cancel: () => {
            clearInterval(tid);
            _reject(new Error("canceled"));
        },
        promise: new Promise<void>((resolve, reject) => {
            tid = setTimeout(resolve, 1000);
            _reject = reject;
        })
    };
    return ret;
}

const cancelable = cancelableSleep(1000)
cancelable.promise
    .then(() => { console.log("finish"); })
    .catch((err) => { console.error(err); });
setTimeout(() => { cancelable.cancel(); console.log("cancel"); }, 1500);
```

このように `{ promise: Promise<T>, cancel: () => void }` という型を `CancelablePromise<T>` と名付ける。
そして `cancel()` を呼ぶと `reject(new Error("canceled"))` されるように関数を作れば良い。

JSではキャンセル可能な非同期APIは比較的珍しいのであまり使うことはないかもしれない( [XMLHttpRequest.abort()](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/abort) とか？ )

## 逐次実行中の Cancelable Promise をキャンセルする

`CancelablePromise<T>` を定義したので `reduce` でこれを逐次実行してみよう。

```ts
function runCancelableAsyncSerialTasks(tasks: (() => CancelablePromise<void>)[]): CancelablePromise<void> {
    const ret = {
        cancel: () => void 0,
        promise: tasks.reduce((prm, task) =>
            prm
                .then(task)
                .then(({ promise, cancel }) => {
                    ret.cancel = cancel;
                    return promise;
                }), Promise.resolve(void 0)),
        
    }; 
    return ret;
}

const cancelable = runCancelableAsyncSerialTasks([
    () => (console.log("a"), cancelableSleep(1000)),
    () => (console.log("b"), cancelableSleep(1000)),
    () => (console.log("c"), cancelableSleep(1000)),
]);

cancelable.promise
    .then(() => { console.log("finish"); })
    .catch((err) => { console.error(err); });
setTimeout(() => { cancelable.cancel(); console.log("cancel"); }, 1500);
```

出力

```
a
b
cancel
Error: canceled
    at Object.cancel (<anonymous>:7:21)
    at <anonymous>:39:37
```

今度は文字を表示してから sleep するようにしたので `b` が表示された後の sleep がキャンセルされている。

## その他 Tips

### ECMAScript ではイベントループや setTimeout は定義されていない
* そのため setTimeout は ECMAScript 仕様外である
* 実際 Java の JS 処理系である Rhino はイベントループはなく当然 setTimeout もない
* イベントループおよび setTimeout の仕様は [HTML](https://www.w3.org/TR/html5/webappapis.html#event-loops) と [node.js](https://nodejs.org/en/docs/guides/event-loop-timers-and-nexttick/) でそれぞれ別に定義されている
* にも関わらず Promise は ECMAScript2015 で定義されている。わけがわからないよ

### Promise の引数関数は同期的に実行される
上述の通り Promise とイベントループは仕様上は直接関係がないので

```js
console.log("a");
new Promise(()=>{
  console.log("b");
});
console.log("c");
```

は `a`, `b`, `c` の順番に同期的に実行される

## async-await と yield と Promise と callback の違い
* [ES2016におけるasyncとgenerator、Promise、CPSの関係](http://qiita.com/DUxCA/items/77a36b7d2b75d8278f9d) を参照

## Promise の all と race の使いかた
* [Promise の all と race だけで書ける待ち合わせ、書けない待ち合わせ](http://qiita.com/Kuniwak/items/9ea447598b7652db78b9) を参照

## まとめ
* Promise の中で throw しない
* Promise には必ず reject する条件も書く
* 可変長の非同期逐次処理は `reduce` を使うことでうまく制御できる
* reduce の中の処理を文脈の必要に応じて変えることでリトライやキャンセル可能な非同期逐次処理が作れる
* ブラウザ JavaScript や node.js のイベントループは ECMAScript 仕様外だが Promise は仕様内である

