例: サーバが 502 を返したときに sleep(setTimeout) を入れつつ 10 回までリトライしたい。


## コピペ用

### 最後のエラーだけ返す版

reject 時に最後のエラーが返ってきます

```ts
function retry<T>(asyncFunc: ()=> Promise<T>, retry = 3): Promise<T> {
    const uniqueObj = {};
    const nums = Array.from(Array(retry));
    return nums.reduce((prm, _, i)=>
        prm.catch((err)=>
            err !== uniqueObj
                ? Promise.reject(err)
                : asyncFunc()
                    .catch((err)=>
                        sleep(i*1000)
                            .then(()=> Promise.reject(uniqueObj)) )
        )
    , Promise.reject(uniqueObj));
    function sleep(ms: number): Promise<void> {
        return new Promise((r)=> setTimeout(r, ms))
    }
}
```



### 全てのエラーを配列で返す版

reject 時に過去の失敗すべてのエラーが配列で返ってきます

```ts
function retry<T>(asyncFunc: ()=> Promise<T>, retry = 3): Promise<T> {
    const errs = [];
    const nums = Array.from(Array(retry));
    return nums.reduce((prm, _, i)=>
        prm.catch((err)=>
            err !== errs
                ? Promise.reject(err)
                : asyncFunc()
                    .catch((err)=>{
                        errs.push(err);
                        return sleep(i*1000)
                            .then(()=> Promise.reject(errs))
                    })
        )
    , Promise.reject(errs));
    function sleep(ms: number): Promise<void> {
        return new Promise((r)=> setTimeout(r, ms))
    }
}
```




## usage

リトライしたい非同期関数をクロージャで囲むだけで使える

```ts
const body = await retry(()=> fetch("https://example.com").then((res)=> res.text()));
```

あるいは async 関数を渡す

```ts
const body = await retry(async ()=>{
   const res = await fetch("https://example.com")
   const body = await res.text();
   return body;
});
```


