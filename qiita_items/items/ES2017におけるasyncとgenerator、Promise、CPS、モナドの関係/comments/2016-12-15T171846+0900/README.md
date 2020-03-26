xstream をモナドとして do 記法を使う。

```ts
export function xsasync<RET, S, T>(generatorFunc: (arg?: S)=> Iterator<Stream<T>> ): (arg?: S)=> Stream<RET> {
  return function (arg?: S): Stream<RET> {
    const generator = generatorFunc(arg);
    return <Stream<RET>>next(null);
    function next(arg: T|null): Stream<T|RET>{
      const result = generator.next(arg);
      if (result.done) {
        if (result.value instanceof Stream) {
          return result.value;
        }else{
          return xs.of(result.value); // return で done されたときは async に習って モナド で包む
        }
      }else{
        return result.value.map(next).flatten(); }
    }
  }
}



const hoge = xsasync(function * _hoge(a: string): Iterator<Stream<string|number>> {
  console.log("a", a);
  const b = yield xs.of("b");
  console.log("b", b);
  const c = yield xs.of(0);
  console.log("c", c);
  return xs.of("fin");
});

hoge("a").addListener({next:console.log});
```

結果

```
a a
b b
c 0
fin
```

`const b = yield xs.of("b");` の `b` に型がつかない問題がある。
`const b: string = yield xs.of("b");` と書けば ok
