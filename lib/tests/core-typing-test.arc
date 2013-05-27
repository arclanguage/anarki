(register-test '(suite "Foundation Tests"
  (suite "Typing"
    (suite "type"
      ("of nil is symbol"
        (type nil)
        sym)

      ("of empty list is also symbol"
        (type ())
        sym)

      ("of t is symbol"
        (type t)
        sym)

      ("of symbol"
        (type 'foo)
        sym)

      ("of string"
        (type "foo")
        string)

      ("of integer rational is int"
        (type 12)
        int)

      ("of integer rational is int"
        (type 12/3)
        int)

      ("of fractional rational is num"
        (type 1/4)
        num)

      ("of double is num"
        (type 3.1415)
        num)

      ("of integer double is int"
        (type 3.0)
        num)

      ("of pair is cons"
        (type '(a b c))
        cons)

      ("of procedure is fn"
        (type (fn (x) x))
        fn)

      ("of character"
        (type #\a)
        char)

      ("of character"
        (type #\newline)
        char)

      ("of table"
        (type (table))
        table)
    )

    (suite "coerce"
      ("nil" ; news url posting requires this somewhere
        (coerce nil 'string)
        "")

      (suite "strings"
        ("list"
          (coerce "abc" 'cons)
          (#\a #\b #\c))

        ("sym"
          (coerce "foo" 'sym)
          foo)

        ("empty string is ||"
          (coerce "" 'sym)
          ||)

        (suite "string->int"
          ("plain int"
            (coerce "256" 'int)
            256)

          ("integer rational"
            (coerce "12/4" 'int)
            3)

          ("string->int rounds"
            (coerce "3/4" 'int)
            1)

          ("int rounds from rational in another base"
            (coerce "101/100" 'int 2)
            1)

          ("rounds floating-point numbers"
            (coerce "14.2857" 'int)
            14)

          ("coerces floats with exponent"
            (coerce "1.3E10" 'int)
            13000000000)

          ("exponent is case-insensitive"
            (coerce "1.3e10" 'int)
            13000000000)

          ("string->int rounds from floating-point in another base"
            (coerce "10.11" 'int 2)
            3)

          ("string->int from float in another base with exponent (simpler case)"
            (coerce "101E100" 'int 2)
            80)

          ("string of given base to int"
            (coerce "FF" 'int 16)
            255)

          ("hexadecimal string containing 'E' to int"
            (coerce "3E" 'int 16)
            62)

          ("positive infinity is not an integer"
            (on-err (fn (ex) "error: can't coerce")
                    (fn ()   (coerce "+inf.0" 'int)))
            "error: can't coerce")

          ("negative infinity is not an integer"
            (on-err (fn (ex) "error: can't coerce")
                    (fn ()   (coerce "-inf.0" 'int)))
            "error: can't coerce"))

        (suite "string->num"
          ("num for non-integers"
            (coerce "3/4" 'num)
            3/4)

          ("type of non-integer rational is num"
            (type (coerce "3/4" 'num))
            num)

          ("num for integer rationals"
            (coerce "12/4" 'num)
            3)

          ("type of integer rational is int even if coerced to num"
            (type (coerce "12/4" 'num))
            int)

          ("type of integer float is num"
            (type (coerce "3.0" 'num))
            num)

          ("string->num for non-integer in a given base"
            (coerce "101/100" 'num 2)
            5/4)

          ("plain floating-point number"
            (coerce "14.2857" 'num)
            14.2857)

          ("double with an exponent"
            (coerce "1.3E10" 'num)
            13000000000.0)

          ("exponent may have + sign"
            (coerce "1.3e+10" 'num)
            13000000000.0)

          ("exponent may have - sign"
            (< 0.012999 (coerce "1.3e-2" 'num) 0.013001)
            t)

          ("string->num in another base"
            (coerce "10.11" 'num 2)
            2.75)

          ("string->num from float in base 2 with exponent"
            (coerce "10.11E1010" 'num 2)
            2816.0)

          ("string->num from float in base 3 with exponent"
            (coerce "10.11E1010" 'num 3)
            709180566103791.0)

          ("string->num from float in another base with exponent (simpler case)"
            (coerce "101E100" 'num 2)
            80.0)

          ("type of 80.0 is num"
            (type (coerce "80.0" 'num))
            num)

          ("positive infinity is a 'num"
            (coerce "+inf.0" 'num)
            +inf.0)

          ("negative infinity is a 'num"
            (coerce "-inf.0" 'num)
            -inf.0)

          ("not a number (note: can't compare nan to nan; nan is not nan!)"
            (coerce (coerce "+nan.0" 'num) 'string)
            "+nan.0")))

      (suite "iso"
        ("sym"
          (coerce 'foo 'sym)
          foo)

        ("string"
          (coerce "montmartre" 'string)
          "montmartre")

        ("int"
          (coerce 2079 'int)
          2079)
      )

      ("sym to string"
        (coerce 'foo 'string)
        "foo" )

      ("empty sym to string"
        (coerce '|| 'string)
        "")

      ("list to string"
        (coerce '(#\z #\o #\o #\, #\space #\n #\o #\?) 'string)
        "zoo, no?" )

      ("list to string"
        (coerce '(#\a #\b #\c #\@ #\e #\x #\a #\m #\p #\l #\e #\. #\c #\o #\m) 'string)
        "abc@example.com" )

      ("list to string including non-char items"
        (coerce '(12 #\x 34 #\y 56/17 #\z 3.1415) 'string)
        "12x34y56/17z3.1415")

      ("list of 1 empty string to string" ; thanks rocketnia, http://arclanguage.org/item?id=12269
        (coerce '("") 'string)
        "")

      (suite "characters"
        ("char to int"
          (coerce #\A 'int)
          65)

        ("char to int with unused base arg"
          (coerce #\n 'int 16)
          110)

        ("char to string"
          (coerce #\A 'string)
          "A")

        ("char to sym"
          (coerce #\g 'sym)
          g))

      (suite "numbers"
        ("int to string"
          (coerce 66 'string)
          "66")

        ("int to num"
          (coerce 0 'num)
          0)

        ("int to char"
          (coerce 67 'char)
          #\C)

        ("int to string of given base"
          (coerce 63 'string 8)
          "77")

        ("double to string"
          (coerce 45.23e-5 'string)
          "0.0004523")

        ("fraction to string in another base"
          (coerce 41/13 'string 8)
          "51/15")

        ("num->int: round to even"
          (list (coerce 1.5 'int)
                (coerce 2.5 'int)
                (coerce 3.5 'int)
                (coerce 4.5 'int)
                (coerce 5.5 'int)
                (coerce 6.5 'int))
          (2 2 4 4 6 6)
        )

        ("num->int scheme rounds to even if decimal fraction == 0.5"
          (list (coerce 3/2 'int)
                (coerce 5/2 'int)
                (coerce 7/2 'int)
                (coerce 9/2 'int)
                (coerce 11/2 'int)
                (coerce 13/2 'int))
          (2 2 4 4 6 6)
        )

        ("num->int with unused base arg"
          (list (coerce 3/2 'int 16)
                (coerce 5/2 'int 16)
                (coerce 7/2 'int 16)
                (coerce 9/2 'int 16)
                (coerce 11/2 'int 16)
                (coerce 13/2 'int 16))
          (2 2 4 4 6 6)
        )

        ("double to int"
          (coerce 3.14 'int)
          3 )))

    (suite "Annotation"
      ("(rep x) == x for builtin types"
        (rep 'foo)
        foo)

      ("don't wrap objects already of the given type"
        (rep (annotate 'a (annotate 'a 'foo)))
        foo)

      ("get the type of an annotation"
        (type (annotate 'foo 'bar))
        foo )

      ("get the value of an annotation"
        (rep (annotate 'foo 'bar))
        bar )))))
