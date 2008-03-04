;;; basic.arc.t - test some basic arc features.
;;;
;;; This file is licensed under the MIT X11 License:
;;; http://www.opensource.org/licenses/mit-license.php
;;;
;;; (C) Copyright by Shlomi Fish, 2008

(load "arctap.arc")

(plan 18)
; TEST
(ok 1 "1 is a true value")
; TEST
(ok (is 3 3) "3 is equal to 3")
; TEST
(ok (is (+ 20 4) 24) "20+4 is equal to 24")

(with (x 20)
    (= x (+ x 4))
    ; TEST
    (ok (is x 24) "Adding 4 to x == 20 yields x == 24")
    (= x (- x 15))
    ; TEST
    (ok (is x 9) "Subtracting 15 to get 9"))

; TEST
(ok (is (* 3 6) 18) "3*6 == 18")

; TEST
(ok (> 5 3) "5 > 3")

; TEST
(ok (>= 5 3) "5 >= 3")

; TEST
(ok (>= 5 5) "5 >= 5")

; TEST
(ok (< 3 5) "3 < 5")

; TEST
(ok (<= 3 5) "3 <= 5")

; TEST
(ok (<= 3 3) "3 <= 3")

; TEST
(ok (not nil) "nil is false")

; TEST
(ok (not (not 1)) "!!1 is true")

; TEST
(ok (not (> 3 5)) "3 is not > 5")

; TEST
(ok (not (is 3 5)) "3 is not 5")

; TEST
(ok (not (< 10 0)) "10 is not less than 0")

; TEST
(ok (not (<= 5 4)) "5 is not leq than 4")

