# できたもの

とりあえず最低限ライフゲームと呼べるものが出来上がったので載せておきます

```cl
;; update-cell :: [[Bit]] -> Num -> Num -> Bit
(defun update-cell (field x y)
  (destructuring-bind (width height) (array-dimensions field) ;; destructuring-bindが長い
  (let* ((live? (eq (aref field x y) 1))
         ;; 境界処理
         (l (mod (1- x) width))
         (r (mod (1+ x) width))
         (u (mod (1- y) height))
         (d (mod (1+ y) height))
         ;; 近傍セルの状態
         (near (list (aref field l u)
                     (aref field x u)
                     (aref field r u)
                     (aref field l y)
                     (aref field r y)
                     (aref field l d)
                     (aref field x d)
                     (aref field r d)))
         ;; 近傍セルの生存数カウント
         (lifes (reduce #'+ near)))
    (cond (live? (case lifes
                       ((2 3)     1)  ;; living
                       (otherwise 0)));; die
          (T     (case lifes
                       (3         1)  ;; born
                       (otherwise 0)))))));;dying

;; update-field :: [[Bit]] -> [[Bit]] -> IO ()
(defun update-field (field1 field2)
  (destructuring-bind (width height) (array-dimensions field1)
    (loop for i from 0 to (1- width) do ;; 二重ループなんか汚い
      (loop for j from 0 to (1- height) do
        (setf (aref field2 i j) (update-cell field1 i j)))))) ;; バッファ1からバッファ2へ写す

;; view-field :: [[Bit]] -> IO ()
(defun view-field (field)
  (destructuring-bind (width height) (array-dimensions field) ;; destructuring-bindが長い
    (loop for i from 0 to (1- width) do ;; 二重ループなんか汚い
      (loop for j from 0 to (1- height) do
        (prin1 (aref field i j))) ;; ヤケクソじみたprint。CRで更新とかしたかった
      (format t "~%")))) ;; 改行するにはこうするしかないのか？

;; 画面サイズ
(defconstant +width+ 30)
(defconstant +height+ 30)

;; 画面バッファ 二次元ビット配列
(defparameter *field-1* (make-array (list +width+ +height+)
  :element-type 'bit
  :initial-element 0))
(defparameter *field-2* (make-array (list +width+ +height+)
  :element-type 'bit
  :initial-element 0))

;; 初期状態としてグライダーを配置
(setf (aref *field-1* 1 1) 1) (setf (aref *field-1* 1 2) 1) (setf (aref *field-1* 1 3) 1)
                                                            (setf (aref *field-1* 2 3) 1)
                              (setf (aref *field-1* 3 2) 1)

;; 100ステップ実行
(let ((lst (list *field-1* *field-2*)))
  (loop for i to 30 do
    (destructuring-bind (a b) lst
      (view-field a)
      (format t "~%") ;; 改行するにはこうするしかないのか？
      (update-field a b)
      (rotatef (nth 0 lst) (nth 1 lst))))) ;; バッファの交換に変数swapしたかったけどrotatefしかわからなかったので
```

## 所感

* slimeやemacsのコマンドを覚えるの大変
* CLtL2やグレアム本はあまり参考にならない
* ググる以外に目当ての情報を得る方法はない
* ググった情報でも型については書かれていなくてつらい
* emacs-slimeの接続が切れたり繋がったり
* atom-slimeも切れたり繋がったり
* イライラしてatomで一気に書いてコンソール `clisp lifegame.lisp` で実行するバッドノウハウ最高

