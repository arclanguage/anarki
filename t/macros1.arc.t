;;; macros1.arc.t - test some macros features.
;;;
;;; This file is licensed under the MIT X11 License:
;;; http://www.opensource.org/licenses/mit-license.php
;;;
;;; (C) Copyright by Shlomi Fish, 2008

(load "arctap.arc")

(plan 4)

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
    (ok (is a 123) "*= (list) works"))

