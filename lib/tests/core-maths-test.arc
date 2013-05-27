(register-test '(suite "Foundation Tests"
  (suite "Maths"
    (suite "+"
      ("sums all args"
        (+ 1 2 3)
        6)

      ("sums all args"
        (+ 1.2 3.4 5.6)
        10.2)

      ("concatenates lists"
        (+ '(a b) '(c d) '(e f))
        (a b c d e f))

      ("concatenates lists ignoring nil"
        (+ '(a b) nil '(c d) nil '(e f))
        (a b c d e f))

      ("converts fraction to int if no loss"
        (+ 12 20/4)
        17 )

      ("retains fraction if not integer"
        (+ 12 20/7)
        104/7 )

      ("retains double even for integer result"
        (+ 13 2.0)
        15.0 )

      ("simplifies fractions"
        (+ 1/10 1/15)
        1/6 )

      ("concatenates strings"
        (+ "foo" 'bar 21)
        "foobar21")

      ("string concatenation ignores nil"
        (+ "a" "b" "c" nil)
        "abc")

      ("can't add a string to a number"
        (on-err (fn (ex) "impossible") (fn () (+ 10 "11")))
        "impossible")

      ("can't add a number to a list"
        (on-err (fn (ex) "impossible") (fn () (+ '(a b c) 4)))
        "impossible")
    )

    (suite "-"
      ("subtracts second arg from first"
        (- 25 7)
        18)

      ("subtracts all subsequent integer args from first"
        (- 1 2 3)
        -4)

      ("subtracts all subsequent double args from first"
        (- 1.2 3.4 5.6)
        -7.8)

      ("converts fraction to int if no loss"
        (- 12 20/4)
        7 )

      ("simplifies fractions"
        (- 1/10 1/15)
        1/30 ))

    (suite "*"
      ("returns 1 for zero args"
        (*)
        1)

      ("returns 1 for zero args via apply"
        (apply * nil)
        1)

      ("multiplies integers"
        (* 23 7)
        161)

      ("multiplies floats"
        (* 2.4 3.5)
        8.4)

      ("multiplies fractions"
         (* 5 1/2)
         5/2 )

      ("multiplies fractions and converts to double"
         (* 2.5 7/2)
         8.75 )

      ("multiplies fractions and converts to double regardless of parameter order"
         (* 7/2 2.5)
         8.75 )

      ("converts fraction to int if no loss"
        (* 9/4 4/3)
        3 )

      ("reads exponents"
        (* 1000000 4e-5)
        40.0)

      ("reads exponents"
        (* 1000000 4.01e-5)
        40.1))

    (suite "/"
      ("divides integers"
        (/ 192 6)
        32)

      ("divides floats"
        (< 3.19999 (/ 11.2 3.5) 3.20001)
        t)

      ("divides first arg by product of remaining args"
        (/ 192 3 2)
        32)

      ("divides first arg by product of remaining args"
        (/ 30 2 2.5)
        6.0))

    (suite "inifities and NaN"
      ("division by zero returns inifinity"     (/ 1.0 0.0)        +inf.0)
      ("division by zero returns neg infinity"  (/ -1.0 0.0)       -inf.0)
      ("subtraction of infinities returns NaN"  (coerce (- +inf.0 +inf.0) 'string) "+nan.0"))

    (suite "trunc"
      ("truncates integers with no effect"
        (trunc 13.0)
        13)

      ("truncates floats"
        (trunc 3.1415)
        3))

    (suite "expt"
      ("simple integer exponents"
        (expt 2.5 3)
        15.625 )

      ("double exponents"
        (expt 100 0.5)
        10.0 )

      ("more double exponents"
        (expt 1000 0.5)
        31.622776601683793 ))

    (suite "sqrt"
      ("simple square root"
        (sqrt 6.25)
        2.5 )

      ("non-integer roots"
        (sqrt 2)
        1.4142135623730951)

      ("root of double"
        (sqrt 3.14)
        1.772004514666935 ))

    (suite "mod"
      ("for positive integers"
        (mod 10 3)
        1)

      ("for negative integers"
        (mod -10 3)
        2)

      ("for negative integers"
        (mod -21 5)
        4)

      ("for fractions"
        (mod -77/7 5)
        4))
)))
