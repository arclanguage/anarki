(load "arctap.arc")

(plan 2)

(with (pair `(5 . 120))
    ; TEST
    (ok (is (car pair) 5) "Testing `(a . b) - car")

    ; TEST
    (ok (is (cdr pair) 120) "Testing `(a . b) - cdr"))
