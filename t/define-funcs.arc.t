(load "arctap.arc")

(plan 4)

(def func1 (a b) (+ a (* 2 b)))

; TEST
(ok (is (func1 2 100) 202) "func1 #1")

; TEST
(ok (is (func1 100 2) 104) "func1 #2")

; TEST
(ok (is (func1 3 3) 9) "func1 #3 - same args")

; TEST
(ok (is (func1 10 1) 12) "func1 #4")

