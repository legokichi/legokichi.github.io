# ES2016におけるasyncとgenerator、Promise、CPSの関係

次の非同期sleepをES2016ではどう書けるかを比較する。

```js
// sleep: number -> Promise<number>
function sleep(ms){
  return new Promise(resolve =>
    setTimeout((()=>resolve(ms)), ms));
}
```

## async-await版

```js
async function main(){
  let a = await sleep(1000);
  alert(`${a}ms passed`); // 1000ms passed
  let b = await sleep(2000);
  alert(`${b}ms passed`); // 2000ms passed
  let [c, d] = await Promise.all([
     sleep(3000),
     sleep(4000)
  ]);
  alert(`${Math.max(c, d)}ms passed`); // 4000ms passed
  alert('done');
}

main(); // return Promise
```

async-await記法を使うとスッキリ書ける。
非同期処理の同期待ちには `Promise.all` を `await` すればよい。
このコードはgeneratorを使うと次のように書ける。

## generator版

```js
// main: void -> Promise<void>
let main = async(function* _main(){
  let a = yield sleep(1000);
  alert(`${a}ms passed`);
  let b = yield sleep(2000);
  alert(`${b}ms passed`);
  let [c, d] = yield Promise.all([
     sleep(3000),
     sleep(4000)
  ]);
  alert(`${Math.max(c, d)}ms passed`);
  alert('done');
});

// async: (void -> Generator) -> (void -> Promise)
function async(generatorFunc) {
  let generator = generatorFunc();
  let onResolved = arg =>{
    let result = generator.next(arg);
    if (result.done) {
      return result.value;
    } else {
      return Promise
      .resolve(result.value)
      .then(onResolved);
    }
  }
  return onResolved;
}

main(); // return Promise<void>
```

generator関数である `_main` が `yeild sleep(3000)` で `Promise<number>` 返す。
その `Promise<number>` を `async` 関数が成功か失敗かを判断し、成功ならば次の計算を呼び出すことで、async-awaitとおなじようにフラットに書ける。

つまり `async` 関数は 内部で Promise のチェーンをつないでいるのだ。

## Promise版

```js
// main: void -> Promise<void>
function main(){
  return sleep(1000).then((a)=>{
    alert(`${a}ms passed`);
    return sleep(2000).then((b)=>{
      alert(`${b}ms passed`);
      return Promise.all([sleep(3000), sleep(4000)]).then(([c, d])=>{
        alert(`${Math.max(c, d)}ms passed`);
        alert('done');
        return;
      });
    });
  });
});

main(); // return Promise<void>
```

これが generator 版で `async` 関数が作っていた Promise のチェーンである。
重要な事実だが __Promise では コールバック地獄を防ぐことはできない__ 。

```ts
async function main(){
  const a = await getA();
  const b = await getB();
  return a + b;
}
```
のような処理を Promise で書こうとすると

```ts
function main(){
  return getA().then((a)=>
    getB().then((b)=>
      a + b));
}
```

というように以前の前の Promise の返り値を利用するためにはクロージャを利用する必要があるため
ネストを深くせざるを得ないからである。
ではなぜ Promise をつかうのかというと、 then メソッドや Promise.all, Promise.race を使うことで非同期処理の演算ができるようになり、コールバックスタイルよりも表現力が上がるからである。

* 参考: [Promise の all と race だけで書ける待ち合わせ、書けない待ち合わせ](http://qiita.com/Kuniwak/items/9ea447598b7652db78b9)

最後に `sleep` を古き良きコールバックスタイルにし、これを継続渡し形式(CPS)で書いてみる。

## CPS版

```js
// sleep: number -> (number -> void) -> void
function sleep(ms, cb){
  setTimeout((()=> cb(ms)), ms);
  return;
}
```

```js
// main: void -> Promise<void>
function main(){
  sleep(1000, (a)=>{
    alert(`${a}ms passed`);
    sleep(2000, (b)=>{
      alert(`${b}ms passed`);
      let waitAll = genWaitAll(next);
      sleep(3000, waitAll());
      sleep(4000, waitAll());
      function next([c, d]){
        alert(`${Math.max(c, d)}ms passed`);
        alert('done');
        return;
      }
    });
  });
};

// genWaitAll: ([a] -> void) -> (void -> (a -> void))
function genWaitAll(next){
  let results = [];
  let counter = 0
  return ()=>{
    let i = counter;
    counter++;
    return (ms)=>{
      results[i] = ms;
      counter--;
      if (counter === 0){
        next(results);
      }
    };
  }
};

main(); // return void;
```
callback hell が起きている。
また、 `Promise.all` のやっていた同期待ち処理を `genWaitAll` のように手書きする必要がある。

## モナドとか

[JavaScript + generator で Maybe、 Either、 Promise モナドと do 構文を実装し async-await と比べてみる](https://qiita.com/DUxCA/items/0582e71f4e6984548933)
