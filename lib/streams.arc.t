;; tests in the style of https://bitbucket.org/zck/unit-test.arc
;; need to first hg clone it into lib/unit-test
(load "lib/unit-test/unit-test.arc")

(suite lazy-stream
  car (assert-same 1 (car (lazy-cons 1 2)))
  cdr (assert-same 2 (cdr (lazy-cons 1 2)))
  to-list (assert-same '(1 2 3)
                       (as cons (lazy-cons 1 (lazy-cons 2 (lazy-cons 3 nil)))))
  lazy-gen (assert-same '(1 2 3)
                        (firstn 3 (lazy-gen (let x 0
                                              (fn () ++.x)))))
  from-fn (assert-same '(1 2 3)
                       (firstn 3
                          (as lazy-stream
                              (let x 0 (fn () ++.x)))))
  lazy-range (assert-same '(5 6 7 8)
                          (as cons (lazy-range 5 8)))
  integers-from (assert-same '(5 6 7 8)
                             (firstn 4 (integers-from 5)))
  map (assert-same '(10 12 14 16 18 20)
                   (map [* _ 2] (lazy-range 5 10)))
  lazy-map (assert-same '(10 12 14 16 18 20)
                        (as cons
                            (lazy-map [* _ 2] (lazy-range 5 10))))
  rem (assert-same '(5 7 9)
                   (rem even (lazy-range 5 10)))
  lazy-rem (assert-same '(5 7 9)
                        (as cons
                            (lazy-rem even (lazy-range 5 10))))
  lazy-keep (assert-same '(5 7 9)
                         (as cons
                             (lazy-keep odd (lazy-range 5 10))))
  find (assert-same 5
                    (find odd (lazy-range 5 10))))
