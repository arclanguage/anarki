(def translate (expr)
  (accum acc
    (translate-2 expr acc)))

(def translate-2 (expr acc)
  (if (atom expr)
        (acc expr)
      (is car.expr 'suite)
        (do (acc 'suite)
            (let (suite-name . suite-body)  cdr.expr
              (acc suite-name)
              (translate-suite-body suite-body acc)))
      (is car.expr 'suite-w/setup)
        (do (acc 'suite)
            (let (suite-name suite-setup . suite-body)  cdr.expr
              (acc suite-name)
              (acc (cons 'setup suite-setup))
              (translate-suite-body suite-body acc)))
      'else
        (map acc expr)))

(def translate-suite-body (suite-body acc)
  (if suite-body
    (if (acons car.suite-body)
      ; nested suite
      (let (nested-suite . rest) suite-body
        (acc (accum acc2
                (translate-2 nested-suite acc2)))
        (translate-suite-body rest acc))
      ; test name must be atomic
      (let (test-name test-body . rest)  suite-body
        (acc `(test ,test-name ,test-body))
        (translate-suite-body rest acc)))))

; suite with tests
(assert:iso '(suite a (test t1 b1) (test t2 b2))
            (translate '(suite a t1 b1 t2 b2)))

; suite with tests and nested suites
(assert:iso '(suite a (test t1 b1) (suite s2 (test t3 b3)) (test t2 b2))
            (translate '(suite a t1 b1 (suite s2 t3 b3) t2 b2)))

; suite with setup and tests
(assert:iso '(suite a (setup x 1 y 2) (test t1 b1) (test t2 b2))
            (translate '(suite-w/setup a (x 1 y 2) t1 b1 t2 b2)))

; suite with setup and tests and nested suites
(assert:iso '(suite a (setup x 1 y 2) (test t1 b1) (suite s2 (test t3 b3)) (test t2 b2))
            (translate '(suite-w/setup a (x 1 y 2) t1 b1 (suite s2 t3 b3) t2 b2)))

(each f cdr.argv
  (prn f)
  (fromfile string.f
    (tofile (+ string.f ".2")
      (each expr (drain (read) eof)
        (let out translate.expr
          (ppr out)
          (prn))))))
