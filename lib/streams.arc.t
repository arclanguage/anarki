(suite lazy-stream
       (test car
             (assert-same 1 (car (lazy-cons 1 2))))
       (test cdr
             (assert-same 2 (cdr (lazy-cons 1 2))))
       (test to-list
             (assert-same '(1 2 3)
                          (as 
                            cons
                            (lazy-cons 
                              1
                              (lazy-cons 2 (lazy-cons 3 nil))))))
       (test iso
             (assert-same (lazy-cons 
                            1
                            (lazy-cons 2 (lazy-cons 3 nil)))
                          (lazy-cons 
                            1
                            (lazy-cons 2 (lazy-cons 3 nil)))))
       (test lazy-gen
             (assert-same '(1 2 3)
                          (firstn 3 (lazy-gen (let x 0 (fn () ++.x))))))
       (test from-fn
             (assert-same '(1 2 3)
                          (firstn 3
                                  (as lazy-stream (let x 0 (fn () ++.x))))))
       (test lazy-range
             (assert-same '(5 6 7 8)
                          (as cons (lazy-range 5 8))))
       (test integers-from
             (assert-same '(5 6 7 8)
                          (firstn 4 (integers-from 5))))
       (test map
             (assert-same '(10 12 14 16 18 20)
                          (map [* _ 2] (lazy-range 5 10))))
       (test lazy-map
             (assert-same '(10 12 14 16 18 20)
                          (as 
                            cons
                            (lazy-map [* _ 2] (lazy-range 5 10)))))
       (test rem
             (assert-same '(5 7 9)
                          (rem even (lazy-range 5 10))))
       (test lazy-rem
             (assert-same '(5 7 9)
                          (as cons (lazy-rem even (lazy-range 5 10)))))
       (test lazy-keep
             (assert-same '(5 7 9)
                          (as cons (lazy-keep odd (lazy-range 5 10)))))
       (test find
             (assert-same 5 (find odd (lazy-range 5 10)))))

