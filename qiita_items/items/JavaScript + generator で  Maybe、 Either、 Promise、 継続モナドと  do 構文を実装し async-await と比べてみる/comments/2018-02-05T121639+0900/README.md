```js
class Monad{
  bind(f){ return this.constructor.join(this.constructor.map(f(this))); }
  static pure(a){ throw new Error("abstruct"); }
  static join(mma){ return mma.bind((ma)=> ma); }
  map(f){ return this.bind((a)=> this.constructor.pure(f(a))); }
}

class MonadTrans {
  // class MonadTrans t where
  // lift :: (Monad m) => m a -> t m a
  static lift(ma){ throw new Error("abstruct"); }
}

function _do(M, genfn){
  const gen = genfn();
  return recur(gen, null);
  function recur(gen, prev){
    const {value, done} = gen.next(prev);
    const ma = value instanceof M ? value : M.pure(value);
    console.log(value, value instanceof M, ma);
    return ma.bind((a)=> !done ? recur(gen, a) : M.pure(a) );
  }
}

class ReaderT extends MonadTrans {
  static lift(ma/*:IO a*/){ return new MonadReaderT((_)=> ma); }
}
class MonadReaderT extends Monad {
  constructor(runReaderT){ super(); this.runReaderT = runReaderT; }
  static pure(M, a){ return ReaderT.lift(M.pure(a)); }
  bind(k){ return new MonadReaderT((r)=> this.runReaderT(r).bind((a)=> k(a).runReaderT(r) ) ); }
  static withReaderT(f){ return (m)=> new MonadReaderT(m(f(a)).runReaderT); }
  static local(ma){ return MonadReaderT.withReaderT(ma); }
  static ask(M, a){ return new MonadReaderT(M.pure(a)); }
  static asks(M, f){ return (a)=> new MonadReaderT(M.pure(f(a))); }
  runReader(a){ return this.runReaderT(a).runIdentity; }
}
class Identity extends Monad {
  constructor(a){ super(); this.runIdentity = a; }
  static pure(a){ return new Identity(a); }
  bind(f){ return f(this.runIdentity); }
}
class Reader extends Monad {
  static pure(a){ return MonadReaderT.pure(Identity, a); }
  static local(ma){ return MonadReaderT.local(ma); }
  static ask(a){ return new MonadReaderT(Identity.pure(a)); }
  static asks(f){ return (a)=> new MonadReaderT(Identity.pure(f(a))); }
}
//debugger
console.group("A")
console.log("A",
  Reader.pure(0)
  .runReader({})
)
console.groupEnd()
console.group("B")
console.log("B",
  _do(Reader, function*(){
      //const x = yield Reader.asks((o)=> o.envX);
      //const y = yield Reader.asks((o)=> o.envY);
      return Reader.pure(0);
  })
  .runReader({envX:1, envY:1})
)
console.groupEnd()
console.group("C")
console.log("C",
  _do(Reader, function*(){
      //const x = yield Reader.asks((o)=> o.envX);
      //const y = yield Reader.asks((o)=> o.envY);
      return 0//Reader.pure(0);
  })
  .runReader({envX:1, envY:1})
)
console.groupEnd()
```

JSでモナド変換子しようとしたけど動的な型の判定が難しくてやめた
