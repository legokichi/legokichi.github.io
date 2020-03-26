## createWithFunc

リソースを開く関数 `function openSomeResource(openOpt: OpenOption): Promise<SomeResource>` と
リソースを閉じる関数 `function closeSomeResource(closeOpt: CloseOption): Promise<void>` から
スコープ付きでリソースを管理する関数 `function withSomeResource<T>(openOpt: OpenOption, withBlock: (r: SomeResource)=> Promise<T>): Promise<T>` を合成する関数。

### 使い方

```ts
function openSomeResource(openOpt: OpenOption): Promise<SomeResource>;
function closeSomeResource(closeOpt: CloseOption): Promise<void>;

export const withSomeResource = createWithFunc(
  openSomething,
  closeSomething,
  (r: SomeResource, openOpt: OpenOption): CloseOption => ({...})
);
```

のようにして使う。
似たようなものとして haskell には [bracket](https://www.stackage.org/haddock/lts-14.5/base-prelude-1.3/BasePrelude.html#v:bracket) がある
([継続モナドによるリソース管理#bracket関数](https://qiita.com/tanakh/items/81fc1a0d9ae0af3865cb#bracket関数) に説明がある)。
しかし、ファイルを開いてしまわずに with 関数を合成する点、 
close のために必要なオプションを生成する `(r: SomeResource, openOpt: OpenOption)=> CloseOption` が必要な点
eが違う。


### 実装

```ts
export function createWithFunc<
  T,
  U,
  V,
  W,
>(
  create: (o: T) => Promise<U>,
  delete_: (o: V) => Promise<W>,
  compose_delete_arg: (
    o: T,
    p: U
  ) => V
): <RET>(
  o: T,
  callback: (o: U) => Promise<RET>
) => Promise<RET> {
  // callback と delete_ の両方のエラーを返しうる
  return async (o, callback) => {
    const resource = (await create(o));
    const delopt = compose_delete_arg(o, resource);
    try {
      const ret = await callback(resource);
      return ret;
    } catch (err) {
      throw err;
    } finally {
      await delete_(delopt);
    }
  };
}
```

## withMany

haskell の素朴な [withMany](https://www.stackage.org/haddock/lts-14.5/base-4.12.0.0/src/Foreign-Marshal-Utils.html#withMany)

### 使い方

```ts
await withMany(withSomeResource, [o1,o2,o3], async ([r1,r2,r3])=>{
  ...
});
```

は 

```ts
await withSomeResource(o1, (r1)=>{
  return withSomeResource(o2, (r2)=>{
    return withSomeResource(o3, (r3)=>{
      return (([r1,r2,r3])=>{
        ...
      }))([r1,r2,r3);
    });
  });
});
```

のように展開されるため、非同期並列でリソースを取得することができず、時間がかかる。

### 実装

```ts
export function withMany<
  A,
  B,
  RES,
  WITH_FUNC extends (x: A, g: (y: B) => RES) => RES
>(withFoo: WITH_FUNC, xs: Array<A>, f: (ys: Array<B>) => RES): RES {
  if (xs.length === 0) {
    return f([]);
  }
  const [head, ...rest] = xs;
  return withFoo(head, (y) =>
    withMany<A, B, RES, WITH_FUNC>(withFoo, rest, (ys) => f([y].concat(ys)))
  );
}
```


## withManyParallel

withMany の非同期並列版。
Promise を使った疑似的な継続 call/cc を使うことで実現する。

### 実装

```ts
export async function withManyParallel<
  A,
  B,
  RES,
  WITH_FUNC extends (x: A, f: (y: B) => Promise<RES>) => Promise<RES>
>(
  xs: Array<A>,
  withFoo: WITH_FUNC,
  f: (
    xs: Array<Parameters<WITH_FUNC>[0]>,
    ys: Array<Parameters<Parameters<WITH_FUNC>[1]>[0]>
  ) => Promise<RES>
): Promise<RES> {
  const ys = new Map<number, Parameters<Parameters<WITH_FUNC>[1]>[0]>();
  let result: RES;
  let ok: Function;
  const allok = new Promise((resolve) => {
    ok = resolve;
  });
  const prm = withParallel<A, B, RES, WITH_FUNC>(
    xs,
    withFoo,
    async (i, x, y) => {
      ys.set(i, y);
      if (ys.size === xs.length) {
        const _ys = xs.map((x, i) => ys.get(i) as B);
        result = await f(xs, _ys);
        ok(); // ok は results を得てから
      }
      // ok を呼ぶと他のタスクが↓の await を通過する
      await new Promise((resolve, reject) => {
        prm.catch(reject);
        allok.then(resolve);
      });
      return result;
    }
  );
  await prm;
  return result!;
}
import lodash = require("lodash");
// withManyParallel の補助関数
export async function withParallel<
  A,
  B,
  RES,
  WITH_FUNC extends (x: A, f: (y: B) => Promise<RES>) => Promise<RES>
>(
  xs: Array<A>,
  withFoo: WITH_FUNC,
  f: (index: number, x: A, y: B) => Promise<RES>
): Promise<Array<RES>> {
  const parallelism = 5;
  const chunks = lodash.chunk(
    xs.map<[number, A]>((x, index) => [index, x]),
    parallelism
  );
  return lodash.flatten(
    await Promise.all(
      chunks.map((xs: [number, A][]) =>
        Promise.all(
          xs.map(async ([index, x]) => withFoo(x, (y) => f(index, x, y)))
        )
      )
    )
  );
}
```
