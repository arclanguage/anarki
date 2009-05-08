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
        int)

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

        ("int"
          (coerce "256" 'int)
          256)

        ("int"
          (coerce "12/4" 'int)
          3)

        ("int even if it's not an integer"
          (coerce "3/4" 'int)
          3/4)

        ("int even if it's not an integer in a given base"
          (coerce "101/100" 'int 2)
          5/4)

        ("int even if it's a double"
          (coerce "14.2857" 'int)
          14.2857)

        ("int even if it's a double with an exponent"
          (coerce "1.3E10" 'int)
          13000000000.0)

        ("exponent is case-insensitive"
          (coerce "1.3e10" 'int)
          13000000000.0)

        ("exponent may have + sign"
          (coerce "1.3e+10" 'int)
          13000000000.0)

        ("exponent may have - sign"
          (< 0.012999 (coerce "1.3e-2" 'int) 0.013001)
          t)

        ("string to int even if it's a double in another base"
          (coerce "10.11" 'int 2)
          2.75)

        ("string to int even if it's a double in another base with an exponent"
          (coerce "10.11E1010" 'int 2)
          2816.0)

        ("string to int even if it's a double in another base with an exponent"
          (coerce "10.11E1010" 'int 3)
          709180566103791.0)

        ("string to int even if it's a double in another base with an exponent (simpler case)"
          (coerce "101E100" 'int 2)
          80.0)

        ("string of given base to int"
          (coerce "FF" 'int 16)
          255)

        ("hexadecimal string containing 'E' to int"
          (coerce "3E" 'int 16)
          62)

       ("positive infinity"
         (coerce "+inf.0" 'int)
         +inf.0)

       ("negative infinity"
         (coerce "-inf.0" 'int)
         -inf.0)

        ("not a number (note: can't compare nan to nan; nan is not nan!)"
          (coerce (coerce "+nan.0" 'int) 'string)
          "+nan.0")
      )

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
        (coerce '(#\z #\o #\o #\, #\?) 'string)
        "zoo,?" )

      ("list to string"
        (coerce '(#\a #\b #\c #\@ #\e #\x #\a #\m #\p #\l #\e #\. #\c #\o #\m) 'string)
        "abc@example.com" )

      (suite "characters"
        ("char to int"
          (coerce #\A 'int)
          65)

        ("char to string"
          (coerce #\A 'string)
          "A")

        ("char to sym"
          (coerce #\g 'sym)
          g)
      )

      (suite "numbers"
        ("int to string"
          (coerce 66 'string)
          "66")

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

        ("double to int"
          (coerce 3.14 'int)
          3.0 )
      )
    )

    (suite "Annotation"
      ("get the type of an annotation"
        (type (annotate 'foo 'bar))
        foo )

      ("get the value of an annotation"
        (rep (annotate 'foo 'bar))
        bar )
    )
  )))
