;;; macros1.arc.t - test some macros features.
;;;
;;; This file is licensed under the MIT X11 License:
;;; http://www.opensource.org/licenses/mit-license.php
;;;
;;; (C) Copyright by Shlomi Fish, 2008

(load "arctap.arc")

(plan 14)

(mac += (var . values)
    `(= ,var (+ ,var ,@values)))

(with (a 5)
    (+= a 10)
    ; TEST
    (ok (is a 15) "+= works")
    (+= a 100 200 300)
    ; TEST
    (ok (is a 615) "+= (list) works"))

(mac op2assign (op)
    (with (op= (sym (+ (string op) "=")))
        `(mac ,op= (var . values)
            `(= ,var (,',op ,var ,@values)))))

; (each op '(- * /) (eval (list 'op2assign op)))

(op2assign -)
(op2assign *)
(op2assign /)

(with (a 100 b 2)
    (-= a 50 20)
    ; TEST
    (ok (is a 30) "-= (and ergo op2assign) works")
    (*= b 3 4 5)
    ; TEST
    (ok (is b 120) "*= (list) works"))

(with (i 100)
      (+= i (* 5 4) (- 10 3 2))
      ; TEST
      (ok (is i 125) "+= evaluates nested sub-expressions"))

(with (i 100)
      (-= i (* 2 3) (- 5 1))
      ; TEST
      (ok (is i 90) "-= evaluates nested sub-expressions"))

; A rather contrived example, but let's see if it works.
(op2assign list)

(with (mylist (list 5 6 7))
      (list= mylist 8 9 10)
      ; TEST
      (ok (is (car (car mylist)) 5) "list= #1")
      ; TEST
      (ok (is (car (cdr mylist)) 8) "list= #2")
      ; TEST
      (ok (is (car (cddr mylist)) 9) "list= #3"))

;;; Let's test recursive macro calls to the ${op}= macros.

(def caddr (l) (car (cddr l)))
(def cadddr (l) (car (cdr (cddr l))))

(with (l 5 n 100)
      (list= l 10 (-= n 4))
      ; TEST
      (ok (is (car l) 5) "-= inside list= No. 1")
      ; TEST
      (ok (is (cadr l) 10) "-= inside list= No. 2")
      ; TEST
      (ok (is (caddr l) 96) "-= inside list= No. 3")
      ; TEST
      (ok (is n 96) "-= inside list= No. 4")
      ; TEST
      (ok (not (cadddr l)) "-= inside list= No. 5"))

