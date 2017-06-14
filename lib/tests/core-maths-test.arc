(suite math
       (suite addition
              (test zero-args-is-0 (assert-same 0 (+)))
              (test is-variadic (assert-same 6 (+ 1 2 3)))
              (test adds-floats
                    (assert-same 10.2 (+ 1.2 3.4 5.6)))
              (test concatenates-lists
                    (assert-same '(a b c d e f)
                                 (+ '(a b) '(c d) '(e f))))
              (test concatenates-lists-ignoring-nil
                    (assert-same '(a b c d e f)
                                 (+ '(a b) nil '(c d) nil '(e f))))
              (test converts-integer-fraction-to-integer
                    (assert-same 17 (+ 12 5)))
              (test doesnt-convert-non-integer-fraction
                    (assert-same 104/7 (+ 12 20/7)))
              (test adding-doubles-results-in-double
                    (assert-same 15.0 (+ 13 2.0)))
              (test simplifies-fractions
                    (assert-same 1/6 (+ 1/10 1/15)))
              (test concatenates-strings
                    (assert-same "foobar21" (+ "foo" 'bar 21)))
              (test string-concatenation-ignores-nil
                    (assert-same "abc" (+ "a" "b" "c" nil)))
              (test cant-add-a-string-to-a-number
                    (assert-error (+ 10 "11")))
              (test cant-add-a-number-to-a-list
                    (assert-error (+ '(a b c) 4))))
       (suite subtraction
              (test basic-subtraction
                    (assert-same 18 (- 25 7)))
              (test is-variadic
                    (assert-same -4 (- 1 2 3)))
              (test subtracts-floats
                    (assert-same -7.8 (- 1.2 3.4 5.6)))
              (test converts-integer-fraction-to-integer
                    (assert-same 7 (- 12 5)))
              (test simplifies-fractions
                    (assert-same 1/30 (- 1/10 1/15))))
       (suite multiplication
              (test zero-args-is-1 (assert-same 1 (*)))
              (test apply-to-nil-is-1
                    (assert-same 1 (apply * nil)))
              (test basic-multiplication
                    (assert-same 161 (* 23 7)))
              (test multiplies-floats
                    (assert-same 8.4 (* 2.4 3.5)))
              (test multiplies-fractions
                    (assert-same 5/2 (* 5 1/2)))
              (test multiplies-fractions-converts-to-double
                    (assert-same 8.75 (* 2.5 7/2)))
              (test multiplies-fractions-converts-to-double-when-double-is-seconds
                    (assert-same 8.75 (* 7/2 2.5)))
              (test converts-integer-fraction-to-integer
                    (assert-same 3 (* 9/4 4/3)))
              (test reads-exponents
                    (assert-same 40.0 (* 1000000 4e-05)))
              (test reads-exponents-with-decimal-places
                    (assert-same 40.1 (* 1000000 4.01e-05))))
       (suite division
              (test divides-integers
                    (assert-same 32 (/ 192 6)))
              (test divides-floats
                    (assert-t (< 3.19999 (/ 11.2 3.5) 3.20001)))
              (test divides-first-arg-by-all-remaining-args
                    (assert-same 32 (/ 192 3 2)))
              (test divides-first-arg-by-all-remaining-args-including-float
                    (assert-same 6.0 (/ 30 2 2.5))))
       (suite infinities-and-NaN
              (test dividing-by-zero-is-infinity
                    (assert-same +inf.0 (/ 1.0 0.0)))
              (test dividing-by-zero-is-negative-infinity
                    (assert-same -inf.0 (/ -1.0 0.0)))
              (test subtracting-infinities-is-NaN
                    (assert-same +nan.0 (- +inf.0 +inf.0))))
       (suite trunc
              (test truncates-integer-valued-floats
                    (assert-same 13 (trunc 13.0)))
              (test truncates-floats
                    (assert-same 3 (trunc 3.1415))))
       (suite expt
              (test integer-exponents
                    (assert-same 15.625 (expt 2.5 3)))
              (test double-exponents
                    (assert-same 10.0 (expt 100 0.5)))
              (test double-exponenets-again
                    (assert-same 31.622776601683793
                                 (expt 1000 0.5))))
       (suite sqrt
              (test simple (assert-same 4 (sqrt 16)))
              (test non-integer-roots
                    (assert-same 1.4142135623730951 (sqrt 2)))
              (test root-of-double
                    (assert-same 1.772004514666935 (sqrt 3.14))))
       (suite mod
              (test positive-integers
                    (assert-same 1 (mod 10 3)))
              (test negative-integers
                    (assert-same 2 (mod -10 3)))
              (test negative-integers-2
                    (assert-same 4 (mod -21 5)))
              (test fractions (assert-same 4 (mod -11 5)))))

