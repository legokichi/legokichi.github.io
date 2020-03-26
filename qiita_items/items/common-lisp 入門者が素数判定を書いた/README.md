Lispの練習で書いたのでここでは[エラトステネスの篩](https://ja.wikipedia.org/wiki/%E3%82%A8%E3%83%A9%E3%83%88%E3%82%B9%E3%83%86%E3%83%8D%E3%82%B9%E3%81%AE%E7%AF%A9)の話はしない。

## 素直な実装

```cl
(defpackage naive-prime-search)
(in-package naive-prime-search)

;; prime-number?:: Num -> List Num -> Bool
(defun prime-number? (n primes)
  (cond
    ((atom primes)               t)
    ((eq 0 (mod n (car primes))) nil)
    (t                           (prime-number? n (cdr primes)))))

;; search-prime:: Num -> List Num -> Num -> List Num
(defun search-prime (n primes max)
  (cond
    ((> n max)                primes)
    ((prime-number? n primes) (search-prime (1+ n) (append primes (list n)) max))
                              ;; appendを使う事でリスト完全コピー。どうみても遅い
                              ;; 末尾再帰最適化されなさそうな書き方
    (t                        (search-prime (1+ n) primes max))))

(time (search-prime 3 '(2) 100000))
```

副作用のない `append` とか `cond` で末尾再帰でない書き方とか、もう見るからに遅そう。

```sh
$ clisp -c naive-prime-search.lisp
$ clisp naive-prime-search.fas
Real time: 11.719404 sec.
Run time: 11.668685 sec.
Space: 736128576 Bytes
GC: 906, GC time: 4.034319 sec.
```

GCが重たい。

## 高速化後

```cl
(defpackage optimised-naive-prime-search)
(in-package optimised-naive-prime-search)

;; リストを探索したついでに素数を追加すべき末尾ポインタを返す
;; (prime-number? n primes (cons 0 primes)) のような形で pre-primes には適当なリストを用意する
;; prime-number?:: Num -> List Num -> List Num -> (Bool, List Num)
(defun prime-number? (n primes pre-primes)
  (cond
    ((atom primes)               (cons t pre-primes))
    ((eq 0 (mod n (car primes))) (cons nil pre-primes))
    (t                           (prime-number? n (cdr primes) primes))))

;; search-prime:: Num -> List Num -> Num -> List Num
(defun search-prime (n primes max)
  (if (> n max)
    primes ;; 探索範囲を超えたら終了
    (let* ((tmp (prime-number? n primes (cons 0 primes)))
           (prime? (car tmp)) ;; prime?: boolean
           (ptr (cdr tmp)))   ;; ptr: もし素数ならcdrを書き換えるべきコンスセル
      (if prime?
        (setf (cdr ptr) (cons n nil)))    ;; setfによるコンスセル書き換え。コピーは発生しない
      (search-prime (1+ n) primes max)))) ;; clispでそのまま実行しても末尾再帰最適化されない。clisp -c hoge.lisp してバイトコードにコンパイルしておいてから clisp hoge.fas しよう

(time (search-prime 3 '(2) 100000))
```

末尾再帰と、破壊的なリストへの追加とリスト末尾ポインタを使い回すような高速化を試みる。

```sh
$ clisp -c naive-prime-search.lisp
$ clisp naive-prime-search.fas
Real time: 7.283802 sec.
Run time: 7.255682 sec.
Space: 3353536 Bytes
GC: 5, GC time: 0.022078 sec.
```

先ほどよりGC時間が少ない。

## 所感

* GC時間を考えなければループ速度はそれほど変わっていないようだ？
* clisp(GNU CLISP 2.49)の場合、 `(defun a () (cond (flag1 nil) (flag2 (a)) (t (a))))` のような複数の再帰がある場合でもgoto的な末尾再帰最適化がされているのかもしれない
