## モナドを定義する

モナドという性質をもつ型は

* 二項演算 `>>=` (bind) 
* 一項演算 `pure`

を持ちます。

```js
class Monad{
  // pure :: a -> m a
  static pure(a){ throw new Error("abstruct"); }
  // (>>=) :: m a -> (a -> m b) -> m b
  bind(f){ throw new Error("abstruct"); }
  // bind(f){ return this.constructor.join(this.constructor.map(f(this))); }

  // join :: m m a -> m a
  //static join(mma){ return mma.bind((ma)=> ma); }
  // (<$>) ::  m a -> (a -> b) -> m b
  //map(f){ return this.bind((a)=> this.constructor.pure(f(a))); }
}
```

* 余談
    * 数学的に厳密なモナドは pure と map と join を持つ。
    * bind は map と join と pure から導出できる
    * 詳しい話は https://ja.wikibooks.org/wiki/Haskell/%E5%9C%8F%E8%AB%96 や https://pursuit.purescript.org/packages/purescript-prelude/ を見てくだし

## do 構文を定義する

```js
function _do(M, genfn){
  const gen = genfn();
  return recur(gen, null);
  function recur(gen, prev){
    const {value, done} = gen.next(prev);
    const ma = value instanceof M ? value : M.pure(value);
    return ma.bind((a)=> !done ? recur(gen, a) : M.pure(a) );
  }
}
```

* JS の `do` は予約語なので `_do` にしてます
* do 構文は、モナディックな型の値 m1, m2, m3 を逐次返す generator を引数にとる
* そして `ma.bind((a)=> mb.bind((b)=> mc.bind((c)=> Monad.pure(c) )))` のように計算をつなげる
* _do の第一引数にモナド型のコンストラクタを渡しているのは
    * `const ma = value instanceof M ? value : M.pure(value);` のために使っている
    *  `return` と `yield` の違いを吸収するために必要
    * 本当は `return pure(a)` か `yield pure(a)` が正しい
    * ~~haskell の return にシャレて return a と書けるようにしている~~
    * [co](https://github.com/tj/co) や async-await は `await` や `return` で `Promise` に強制的に包む仕様なので（非同期実行のため）それにあわせた
    * while ではなく recur してるのも非同期実行されることを意識しているから

## Maybe モナドを定義する
Maybe の `>>=` (bind) の性質は、計算結果が nothing だと計算を打ち切って nothing を返します。

```js
class Maybe extends Monad {
  constructor(a){ super(); this.a = a; }
  isJust(){ return this.a != null; }
  isNothing(){ return this.a == null; }
  bind(cont){ return this.isJust() ? cont(this.a) : Maybe.nothing(); }
  static pure(a){ return Maybe.just(a); }
  static just(a){ return new Maybe(a); }
  static nothing(){ return new Maybe(null); }
  toString(){ return this.isJust() ? `Just ${this.a}` : `Nothing`; }
}
```

## Maybe モナドを使ってみる
すべて just なら成功。

```js
function main(){
  const opt = _do(Maybe, function* calc(){
    const a = yield Maybe.just(1);
    console.assert(a === 1);
    const b = yield Maybe.just(1);
    console.assert(b === 1);
    const c = yield Maybe.just(a + b);
    console.assert(c ===  1 + 1);
    return c;
  });
  console.assert(opt.toString() === "Just 2");
}
main();
```

ひとつでも nothing があれば nothing。

```js
function main(){
  const opt = _do(Maybe, function* calc(){
    const a = yield Maybe.just(1);
    console.assert(a === 1);
    const b = yield Maybe.nothing(); // なんか処理がうまくいかなかった
    console.assert(b === 1);
    const c = yield Maybe.just(a + b);
    console.assert(c ===  1 + 1);
    return c;
  });
  console.assert(opt2.toString() === "Nothing");
}
main();
```

null 安全なコードが書けた。

## Either モナドを定義する

Either モナドの `>>=` (bind) の性質は、計算結果が left のときは計算を打ち切って left 値を返します。

```js
class Either extends Monad {
  constructor(l, r){ super(); this.l = l; this.r = r;  }
  isLeft(){ return this.l != null; }
  isRight(){ return this.r != null; }
  bind(cont){ return this.isRight() ? cont(this.r) : Either.left(this.l, null); }
  static pure(a){ return Either.right(a); }
  static right(a){ return new Either(null, a); }
  static left(a){ return new Either(a, null); }
  toString(){ return this.isLeft() ? `Left ${this.l}` : `Right ${this.r}`; }
}
```

## Either モナドを使ってみる

すべて right なら計算が最後まで続く

```js
function main(){
  const opt = _do(Either, function*(){
    const a = yield Either.right(1);
    console.assert(a === 1);
    const b = yield Either.right(1);
    console.assert(b === 1);
    const c = yield Either.right(a + b);
    console.assert(c ===  1 + 1);
    return c;
  });
  console.assert(opt.toString() === "Right 2");
}
main();
```

途中で処理が失敗(=left)したらそこで計算を打ち切って left を返す

```js
function main(){
  const opt = _do(Either, function*(){
    const a = yield Either.right(1);
    console.assert(a === 1);
    const b = yield Either.left("fail"); // なんか処理が失敗した
    console.assert(b === 1);
    const c = yield Either.right(a + b);
    console.assert(c ===  1 + 1);
    return c;
  });
  console.assert(opt.toString() === "Left fail");
}
main();
```

失敗つき計算を表現できた。うれしい！

## Promise モナド

Promise はJSの実装そのまま使ってみます。

```js
Promise.prototype.bind = Promise.prototype.then;
Promise.pure = Promise.resolve;
Promise.prototype.map = Monad.prototype.map;
Promise.join = Monad.join;
```

* JS の Promiseはネストできないので `map` を定義しても `then` と同じ
* JS の Promiseはネストできないので `join` を定義するのはあまり意味がない 
    * `Promise.resolve(Promise.resolve(1))` は `Promise.resolve(1)` として扱われる

## Promise モナドを使ってみる 

```js
function main(){
  const prm = _do(Promise, function*(){
    const a = yield Promise.resolve(1);
    console.assert(a === 1);
    const b = yield Promise.resolve(1);
    console.assert(b === 1);
    const c = yield Promise.resolve(a + b);
    console.assert(c ===  1 + 1);
    return c;
  });
  // JS の Promise はそのイベントループの中作られた Promise 値は次のイベントループまで pending になるため中を見るには then する必要ある
  prm
    .then((c)=> console.assert(c === 2))
    .catch((c)=> console.assert(false));
}
main();
```

これが、 async-await の実装の基礎です (実際は try-catch も使えるようになっているので少し違う)

### try-catch できる async-await の実装

```js
function async(genfn) {
  return (...args)=>{
    const gen = genfn.apply(this, args);
    return recur(gen, null, null);
    function recur(gen, prev, preverr){
      try{
        const {value, done} = preverr == null ? gen.next(prev) : gen.throw(preverr);
        const ma = value instanceof Promise ? value : Promise.resolve(value);
        return ma
          .then((a)=> !done ? recur(gen, a, null) : Promise.resolve(a) )
          .catch((err)=> !done ? recur(gen, null, err) : Promise.reject(err) );
      }catch(err){
        recur(gen, null, err);
      }
    }
  };
}
```

do との違いとして

* generator 関数を promise 値関数に変換するために `return (...args)=>{` している
* generator の実行を try-catch で囲んでいる
* err が扱えるように recur が 3 引数になった
* `generatir.throw` で generator 関数へ `throw` している

などがあります。

### async-await を使う

```js
const main = async(function*(init){
  const a = yield Promise.resolve(init);
  console.assert(a === init);
  const b = yield Promise.resolve(1);
  console.assert(b === 1);
  const c = yield a + b;
  console.assert(c ===  1 + init);
  return c;
});

main(1)
  .then((c)=> console.assert(2 === c))
  .catch((c)=> console.assert(false))
```

main に引数が付きました。

もちろんエラー対策も万全なので

```js
const main = async(function*(init){
  let a;
  try{
    throw new Error("some error"); 
    a = yield Promise.resolve(init);
  }catch(err){
    // catch できる
    a = yield init; // resolve なくてもいける
  }
  console.assert(a === init);
  let b;
  try{
    b = yield Promise.reject(new Error("some AIO erorr"));
  }catch(err){
    // reject も catch できる
    b = yield Promise.resolve(1);
  }
  console.assert(b === 1);
  const c = yield a + b;
  console.assert(c ===  1 + init);
  return c;
});

main(1)
  .then((c)=> console.assert(2 === c))
  .catch((c)=> console.assert(false))
```

のような処理も実行できます

### do と async の diff

![](https://i.gyazo.com/0cdb32310ccc94bc962721232bdc2dc3.png)

## 継続モナドを実装する

[こうなってくるとわけがわからない](http://www.sampou.org/haskell/a-a-monads/html/contmonad.html)

```js
class Cont extends Monad {
  // runCont :: ((a -> r) -> r)
  constructor(runCont){ super(); this.runCont = runCont; }
  //  (Cont c) >>= f = Cont $ \k -> c (\a -> runCont (f a) k)
  bind(f){ return new Cont((k)=> this.runCont((a)=> f(a).runCont(k) ) ); }
  // return a       = Cont $ \k -> k a
  static pure(a){ return new Cont((k)=> k(a)); }
  // callCC :: ((a -> m b) -> m a) -> m a 
  static callCC(f){ return new Cont((k)=> (f((a)=> new Cont((_)=> k(a)) )).runCont(k) ); }
}
```

## 継続モナドを使ってみる

緊急脱出ができる。

```js
function main(){
  return ((c)=> c.runCont((a)=>a))( _do(Cont, function*(){
    const ret = yield Cont.callCC((exit)=> _do(Cont, function*(){
      const a = yield Cont.pure(1);
      const b = yield Cont.pure(1 + a);
      console.assert(b === 2);
      if(b === 2){
        yield exit(100); // escape!
      }
      return Cont.pure(-100); // never run
    }));
    console.assert(ret === 100);
    return Cont.pure(ret);
  }));
}
console.assert(main() === 100);
```

`exit` で抜けた場合と `return pure` で抜けた場合で型が同じなので例外というよりは goto のような使い方になりそう。

## 後記

* xstreamモナドとかjQueryモナドとかリストモナドも書きたかった。あとで書くかも。
* 複雑になると haskell より醜い


元ネタ https://qiita.com/DUxCA/items/77a36b7d2b75d8278f9d


