# Python の generator で Monad と do 記法を実装する

## Monad を定義する

```py
from abc import ABCMeta, abstractmethod

class Monad(metaclass=ABCMeta):
    @classmethod
    def pure(cls):
        raise NotImplementedError()
    @classmethod
    def bind(cls, f):
        raise NotImplementedError()
```

## do 構文を定義する

```py
def do(genfn):
    import functools
    @functools.wraps(genfn)
    def doimpl():
        gen = genfn()
        def recur(gen, prev):
            try:
                ma = gen.send(prev)
            except StopIteration as last:
                return last.value
            done = True if ma == None else False
            def cb(a):
                if not done:
                    return recur(gen, a)
                else:
                    return ma.__class__.pure(a)
            return ma.bind(cb)
        return recur(gen, None)
    return doimpl
```

## Maybe
### Maybe モナドを定義する

```py
class Maybe(Monad):
    def __init__(self, a): 
        self.a = a
    def is_just(self):
        return self.a != None
    def is_nothing(self):
        return self.a == None
    def just(a):
        return Maybe(a)
    def nothing(cls):
        return Maybe(None)
    def bind(self, cont):
        if self.is_just():
            return cont(self.a)
        else:
            return self.__class__.nothing()
    @classmethod
    def pure(cls, a):
        return cls.just(a)
```

### Maybe モナドを使ってみる

```python
def main():
    @do
    def genfn():
        a = yield Maybe.just(1)
        assert a == 1
        b = yield Maybe.just(2)
        assert b == 2
        c = yield Maybe.just(a + b)
        assert c == 3
        return Maybe.pure(c + 1)
    opt = genfn()
    if opt.is_just():
        assert opt.a == 4
    else:
        assert False
main()
```

## asyncio の Future でモナドを作る
### asyncio の実装について

* `@asyncio.coroutine` の実装は本質的には `@do` と同じ
* ただし python3 の future は JavaScript の `Promise.prototype.then` のようなコールバックではなくコルーチンとして実装されている
* そのため `ret = yield from fut` のように使う
* あるいは `ensure_future` でコルーチンから Future へ変換できる
* `bind` の実装には `add_done_callback` が使えそう
    * ただし `add_done_callback` は JS の `Promise.prototype.then` のように新しい Future 値を返さない(Noneを返す)のでチェーンできない
* 同様に `Promise.resolve` は `fut.set_result` が対応するがこれも None を返すなど面倒。
* 加えて lambda が構文を持てないので可読性が悪い
* python は JS のように動的にメソッドを追加できない (2.x のころは MethodType でできた) ので Future をそのまま使おうとすると do を書き換える必要があり

```py
from asyncio import Future, ensure_future, sleep, get_event_loop

def do(genfn):
    import functools
    @functools.wraps(genfn)
    def doimpl():
        gen = genfn()
        final = Future()
        def recur(gen, prev):
            try:
                ma = gen.send(prev)
            except StopIteration as last:
                final.set_result(last.value.result())
                return final
            _fut = Future()
            def _cb(fut):
                _fut.set_result(recur(gen, (fut)))
                return _fut
            ma.add_done_callback(_cb)
            return _fut
        recur(gen, None)
        return final
    return doimpl

def main():
    @do
    def genfn():
        yield ensure_future(sleep(1))
        print(1)
        yield ensure_future(sleep(2))
        print(2)
        yield ensure_future(sleep(3))
        print(3)
        fut = Future()
        fut.set_result(4)
        return fut
    fut = genfn()
    loop = get_event_loop()
    ensure_future(fut)
    loop.run_until_complete(fut)
    assert fut.result() == 4

main()
```


## 感想
* python のことがますます嫌いになりました

## reference

* https://qiita.com/DUxCA/items/0582e71f4e6984548933
* http://postd.cc/python-generators-coroutines-native-coroutines-and-async-await/
* https://qiita.com/mtb_beta/items/d257519b018b8cd0cc2e
* https://docs.python.jp/3/library/asyncio-task.html#example-chain-coroutines
* https://docs.python.jp/3/library/asyncio-task.html#asyncio.Task
* https://github.com/python/cpython/blob/789e359f51d2b27bea01b8c6c3bf090aaedf8839/Lib/asyncio/coroutines.py#L104
