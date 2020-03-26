[Schemeでジェネレーター - Qiita](http://qiita.com/hotoku/items/a36974d2ecfec9ace930)を拝見したところ、ジェネレータ関数の内部でyieldが呼ばれなかったときに無限ループに入るバグを見つけたので修正した。


```scm
;; make-generator: ((a → ⊥) → a) → (() → a|undefined)
(define (make-generator proc)

  ;; nop: () → undefined
  (define (nop) (if #f 0))

  ;; return: a → ⊥
  (define (return val) ()) ;; 一回目のリターンポイントは未定義

  ;; next: () → a|undefined
  (define (next) (proc yield));; 一回目のジェネレータ関数内部のセーブポイントはproc最初から、ジェネレータ関数の最後まで到達したらundefinedを返す

  ;; yield a → ⊥
  (define (yield value)
    (call/cc (lambda (next*)
      (set! next next*)
      (return value) )) )

  ;; generator*: () → a
  (define (generator*)
    (call/cc (lambda (return*);; generatorが呼ばれた時のリターンポイント、gen=make_generator(fn);gen()のとき
      (set! return return*) 
      (let ((val (next)))
        ;; ここに到達した -> nextでyieldは使われなかった -> return は使われなかった
        (set! generator* nop) ;; 以後nop
        (return val) ) )) )

  ;; generator: () → a|undefined
  (define (generator) (generator*))

  generator)

;; gen-n: () → a|undefined
(define (gen-n)
  (make-generator
   (lambda (yield)
     (yield 1) ;;ここの各行の継続がnextに保持されるセーブポイント
     (yield 2)
     (yield 3)
     -1 )) )

(let ((an (gen-n)))
  (print (an));; 1 ここの各行の継続がreturnに保持されるリターンポイント
  (print (an));; 2
  (print (an));; 3
  (print (an));; -1
  (print (an)));; #<undef>
```

## 感想

人類には第一級継続は早すぎるし型のない言語はつらい
