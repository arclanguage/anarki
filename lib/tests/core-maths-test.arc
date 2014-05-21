(suite math
       (suite addition
              zero-args-is-0 (assert-same 0
                                          (+))
              is-variadic (assert-same 6
                                       (+ 1 2 3))
              adds-floats (assert-same 10.2
                                       (+ 1.2 3.4 5.6))
              concatenates-lists (assert-same '(a b c d e f)
                                              (+ '(a b) '(c d) '(e f)))
              concatenates-lists-ignoring-nil (assert-same '(a b c d e f)
                                                           (+ '(a b) nil '(c d) nil '(e f)))
              converts-integer-fraction-to-integer (assert-same 17
                                                                (+ 12 20/4))
              doesnt-convert-non-integer-fraction (assert-same 104/7
                                                               (+ 12 20/7))
              adding-doubles-results-in-double (assert-same 15.0
                                                            (+ 13 2.0))
              simplifies-fractions (assert-same 1/6
                                                (+ 1/10 1/15))
              concatenates-strings (assert-same "foobar21"
                                                (+ "foo" 'bar 21))
              string-concatenation-ignores-nil (assert-same "abc"
                                                            (+ "a" "b" "c" nil))
              cant-add-a-string-to-a-number (assert-error (+ 10 "11"))
              cant-add-a-number-to-a-list (assert-error (+ '(a b c) 4)))

       (suite subtraction
              basic-subtraction (assert-same 18
                                             (- 25 7))
              is-variadic (assert-same -4
                                       (- 1 2 3))
              subtracts-floats (assert-same -7.8
                                            (- 1.2 3.4 5.6))
              converts-integer-fraction-to-integer (assert-same 7
                                                                (- 12 20/4))
              simplifies-fractions (assert-same 1/30
                                                (- 1/10 1/15)))

       (suite multiplication
              zero-args-is-1 (assert-same 1
                                          (*))
              apply-to-nil-is-1 (assert-same 1
                                             (apply * nil))
              basic-multiplication (assert-same 161
                                                (* 23 7))
              multiplies-floats (assert-same 8.4
                                             (* 2.4 3.5))
              multiplies-fractions (assert-same 5/2
                                                (* 5 1/2))
              multiplies-fractions-converts-to-double (assert-same 8.75
                                                                   (* 2.5 7/2))
              multiplies-fractions-converts-to-double-when-double-is-seconds (assert-same 8.75
                                                                                          (* 7/2 2.5))
              converts-integer-fraction-to-integer (assert-same 3
                                                                (* 9/4 4/3))
              reads-exponents (assert-same 40.0
                                           (* 1000000 4e-5))
              reads-exponents-with-decimal-places (assert-same 40.1
                                                               (* 1000000 4.01e-5)))

       (suite division
              divides-integers (assert-same 32
                                            (/ 192 6))
              divides-floats (assert-t (< 3.19999
                                          (/ 11.2
                                             3.5)
                                          3.20001))
              divides-first-arg-by-all-remaining-args (assert-same 32
                                                                   (/ 192 3 2))
              divides-first-arg-by-all-remaining-args-including-float (assert-same 6.0
                                                                                   (/ 30 2 2.5)))

       (suite infinities-and-NaN
              dividing-by-zero-is-infinity (assert-same +inf.0
                                                        (/ 1.0 0.0))
              dividing-by-zero-is-negative-infinity (assert-same -inf.0
                                                                 (/ -1.0 0.0))
              subtracting-infinities-is-NaN (assert-same +nan.0
                                                         (- +inf.0 +inf.0)))

       (suite trunc
              truncates-integer-valued-floats (assert-same 13
                                                           (trunc 13.0))
              truncates-floats (assert-same 3
                                            (trunc 3.1415)))
       (suite expt
              integer-exponents (assert-same 15.625
                                             (expt 2.5 3))
              double-exponents (assert-same 10.0
                                            (expt 100 0.5))
              double-exponenets-again (assert-same 31.622776601683793
                                                   (expt 1000 0.5)))
       (suite sqrt
              simple (assert-same 4
                                  (sqrt 16))
              non-integer-roots (assert-same 1.4142135623730951
                                             (sqrt 2))
              root-of-double (assert-same 1.772004514666935
                                          (sqrt 3.14)))
       (suite mod
              positive-integers (assert-same 1
                                             (mod 10 3))
              negative-integers (assert-same 2
                                             (mod -10 3))
              negative-integers-2 (assert-same 4
                                               (mod -21 5))
              fractions (assert-same 4
                                     (mod -77/7 5))))
