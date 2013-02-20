;; http://bach.istc.kobe-u.ac.jp/lect/ProLang/org/lisp.html
(setq menu '(tea coffee milk))
(cons (car menu) (cdr (cdr menu)))
(cons (car (cdr menu)) (cons (car menu) (cdr (cdr menu))))
(cons (car (cdr menu)) (cons (car menu) (cdr (cdr menu))))

(defun nibanme (x) (car (cdr x)))
(defun del2 (x) (cons (car x) (cdr (cdr x))))
(defun ex12 (x) (cons (nibanme x) (del2 x)))
(defun ex23 (x) (cons (car x) (ex12 (cdr x))))
(assert (equal (ex23 menu) '(tea milk coffee)))

;; リストを左へ1つ回転させたリストを求める関数 rotate を定義せよ． たとえば (rotate '(a b c d)) の結果は (b c d a) である． 与えられるリストの長さは1以上としてよい．
(setq menu '(a b c d))
(defun rotate (x) (append (cdr x) (cons (car x) ())))
(assert (equal (rotate '(a b c d)) '(b c d a)))

(defun ookiihou (x y) (if (> x y) x y))
(assert (equal (ookiihou 3 7) 7))

;; 2つの数値を要素とする長さ2のリストを昇順に並べ変えたリストを 求める関数 sort2 を定義せよ． たとえば (sort2 '(3 2)) の結果は (2 3) ， (sort2 '(2 3)) の結果は (2 3) である．
(defun sort2 (x) (if (> (car x) (car (cdr x))) (cons (car (cdr x)) (cons (car x) ())) x))
(assert (equal (sort2 '(3 2)) '(2 3)))
(assert (equal (sort2 '(2 3)) '(2 3)))

;; 与えられた西暦が グレゴリオ暦 でうるう年になるかどうかを判定する関数 leap_year を定義せよ．
(defun leap_year (x)
  (if (or (= (% x 400) 0) (and (= (% x 4) 0) (not (= (% x 100) 0))))
          t nil))
(assert (equal (leap_year 2000) t))
(assert (equal (leap_year 2100) nil))
(assert (equal (and) t))
(assert (equal (or) nil))

;; copy
(defun fact (n)
  (if (= n 0) 1 (* n (fact (- n 1)))))
(assert (equal (fact 10) 3628800))

(defun sum (l)
  (if (null l) 0 (+ (car l) (sum (cdr l)))))
(assert (equal (sum '(1 9 8 9)) 27))

(defun app (x y)
  (if (null x)
     y
     (cons (car x) (app (cdr x) y))))
(assert (equal (app '(a b) '(c d)) '(a b c d)))

;; 関数 fact を変更して，1から n の和 1+2+…+n を 求める関数 sigma を定義せよ
(defun sigma (n)
  (if (= n 0) 0 (+ n (sigma (- n 1)))))
(assert (equal (sigma 10) 55))

;; 関数 fact を変更して，1から n の平方和 12+22+…+n2 を 求める関数 sigma2 を定義せよ．
(defun sigma2 (n)
  (if (= n 1) 1 (+ (* n n) (sigma2 (- n 1)))))
(assert (equal (sigma2 10) 385))

;; 次の漸化式で定義されるフィボナッチ数を計算する関数 fib を定義せよ．
(defun fib (n)
  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(assert (equal (fib 4) 3))
(assert (equal (fib 5) 5))

;; リストの全要素の積を求める関数 prod を定義せよ．
(defun prod (n)
  (if (null n)
      1
      (* (car n) (prod (cdr n)))))
(assert (equal (prod '(1 2 3 4 5)) 120))

;;リストの中で一番大きい要素を求める関数 maxelem を定義せよ．
(defun maxelem(x)
  (if (= (length x) 1)
      (car x)
      (if (> (car x) (maxelem (cdr x)))
          (car x)
          (maxelem (cdr x))
        )))
(assert (equal (maxelem '(1 2 3 4 5)) 5))
(assert (equal (maxelem '(10 2 3 4 5)) 10))

;; リストを逆順にしたリストを求める関数 rev を定義せよ．
(defun rev(x)
  (if (null x)
      ()
      (append (rev (cdr x)) (list (car x)))))
(assert (equal (rev '(1 2 3)) '(3 2 1)))
(append '(3) (append '(2) (append '(1) ())))

;; 第2引数のリストの要素として第1引数と同じ記号が現れるかどうか調べる 述語 mem を定義せよ． たとえば (mem 'a '(b a c)) の結果は t ， (mem 'a '(b (a) c)) の結果は nil である．
;; ヒント：第2引数が空リストなら結果は nil である． 第2引数が空リストでないときは， 第2引数の car が第1引数と equal なら結果は t ， equal でないなら第2引数の cdr について調べる．

(defun mem(x y)
  (if (null y)
      ()
      (if (equal x (car y))
          t
          (mem(x '(cdr y))))))
(assert (equal (mem 'a '(b a c)) t))
(assert (equal (mem 'a '(b (a) c)) nil))
