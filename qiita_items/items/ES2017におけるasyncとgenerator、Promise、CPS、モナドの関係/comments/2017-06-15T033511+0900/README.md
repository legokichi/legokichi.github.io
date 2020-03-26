co のような try-catch できる async



```js
function async(generatorFunc) {
  return function () {
    return new Promise((resolve, reject)=>{
      const generator = generatorFunc.apply(this, arguments);
      if(!generator){ return resolve(generator); }
      if(typeof generator.next !== 'function'){ return resolve(generator) }
      return onFulfilled();
      function next(result) {
        if (result.done) return resolve(result.value);
        if (result.value instanceof Promise){ return result.value.then(onFulfilled, onRejected); }
        return Promise.resolve(result.value);
      }
      function onFulfilled(res) {
        try{ next(generator.next(res)); }
        catch(e){ reject(e); }
      }
      function onRejected(err) {
        try{ next(generator.throw(err)); }
        catch(e){ reject(e); }
      }
    });
  };
}
```
