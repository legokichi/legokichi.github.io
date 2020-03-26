上記の async 化関数は初期値が与えられないため不十分です。

```ts
export function async<S, T>(generatorFunc: (arg: S)=> Iterator<Promise<T>> ): (arg: S)=> Promise<T> {
  return function (arg: S): Promise<T> {
    const generator = generatorFunc(arg);
    return next(null);
    function next(arg: T|null): Promise<T>{
      const result = generator.next(arg);
      if(result.done){ return result.value; }
      else if(result.value instanceof Promise){ return result.value.then(next); }
      else{ return Promise.resolve(result.value); }
    }
  }
}

const main = async(function* main(period: number){
  let a = yield sleep(init);
  alert(`${a}ms passed`);
  let b = yield sleep(init);
  alert(`${b}ms passed`);
  let [c, d] = yield Promise.all([
     sleep(3000),
     sleep(4000)
  ]);
  alert(`${Math.max(c, d)}ms passed`);
  alert('done');
});

main(1000);
```

のほうが TS の async の挙動に近いと思います。

あと `_async` はスペルミスではないでしょうか？


