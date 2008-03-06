(load "arctap.arc")

(plan 11)

(def func1 (a b) 
     (+ a (* 2 b)))

; TEST
(ok (is (func1 2 100) 202) "func1 #1")

; TEST
(ok (is (func1 100 2) 104) "func1 #2")

; TEST
(ok (is (func1 3 3) 9) "func1 #3 - same args")

; TEST
(ok (is (func1 10 1) 12) "func1 #4")

(def 1- (n) (- n 1))

; TEST
(ok (is (1- 5) 4) "Testing 1- - #1")

; TEST
(ok (is (1- 1) 0) "Testing 1- - #2")

; TEST
(ok (is (1- 0) -1) "Testing 1- - #3")

(def factorial1 (n)
     (prn n)
     (if (is n 0) 
       1 
       (* n (factorial1 (1- n)))))

; TEST
(ok (is (factorial1 4) 24) "Simple recursion - #1")

; TEST
(ok (is (factorial1 0) 1) "Simple recursion - #2")

; TEST
(ok (is (factorial1 1) 1) "Simple recursion - #3")

; TEST
(ok (is (factorial1 3) 6) "Simple recursion - #4")

