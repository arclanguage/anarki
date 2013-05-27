(register-test '(suite "Foundation Tests"
  (suite "Predicates"
    (suite "bound"
      ("true for items assign with assign"
        ( (fn () (assign foo 'bar) (bound 'foo)) )
        t )

      ("false for unset items"
        (bound 'toto-yadda)
        nil )

      ("false for local parameters"
        ( (fn (yadda-toto) (bound 'yadda-toto)) "blah" )
        nil )
    )

    (suite "<"
      ("t if first number argument is less than second"
        (< 1 2)
        t)

      ("nil if first number argument is greater than second"
        (< 20 5)
        nil)

      ("nil if first number argument is equal to second"
        (< 20 20)
        nil)

      ("t if first string argument is less than second"
        (< "bar" "foo")
        t)

      ("nil if first string argument is greater than second"
        (< "zizi" "alpha")
        nil)

      ("nil if first string argument is equal to second"
        (< "foo" "foo")
        nil)

      ("t if first symbol argument is less than second"
        (< 'bar 'foo)
        t)

      ("nil if first symbol argument is greater than second"
        (< 'zizi 'alpha)
        nil)

      ("nil if first symbol argument is equal to second"
        (< 'foo 'foo)
        nil)

      ("is variadic"
        (< 1 2 3 4)
        t)

      ("is variadic"
        (< 1 2 10 4)
        nil)

      ("works on characters too"
        (< #\a #\c #\z)
        t)

      ("works on characters too"
        (< #\z #\c #\a)
        nil)
    )

    (suite ">"
      ("nil if first number argument is less than second"
        (> 1 2)
        nil)

      ("t if first number argument is greater than second"
        (> 20 5)
        t)

      ("nil if first number argument is equal to second"
        (> 20 20)
        nil)

      ("nil if first string argument is less than second"
        (> "bar" "foo")
        nil)

      ("t if first string argument is greater than second"
        (> "zizi" "alpha")
        t)

      ("nil if first string argument is equal to second"
        (> "foo" "foo")
        nil)

      ("nil if first symbol argument is less than second"
        (> 'bar 'foo)
        nil)

      ("t if first symbol argument is greater than second"
        (> 'zizi 'alpha)
        t)

      ("nil if first symbol argument is equal to second"
        (> 'foo 'foo)
        nil)

      ("is variadic"
        (> 4 3 2 1)
        t)

      ("is variadic"
        (> 100 20 40 10)
        nil)

      ("works on characters too"
        (> #\a #\c #\z)
        nil)

      ("works on characters too"
        (> #\z #\c #\a)
        t)
    )

    (suite "exact"
      ("t for integers"
        (exact 12)
        t)

      ("nil for reals even if they are integral"
        (exact 12.0)
        nil)

      ("t for fractions evaluating to integer"
        (exact 12/4)
        t)

      ("nil for fractions evaluating to non-integer"
        (exact 12/5)
        nil)

      ("nil for non-integers"
        (exact 3.14)
        nil)
    )

    (suite "is"
      ("t for two nils"
        (is nil nil)
        t)

      ("t for two t's"
        (is t t)
        t)

      ("t is not nil"
        (is t nil)
        nil)

      ("nil is not t"
        (is nil t)
        nil)

      ("t for the same characters"
        ((fn ()
          (assign ss (coerce "  " 'cons))
          (is (car ss) #\space)
        ))
        t)

      ("t for the same characters"
        ((fn ()
          (assign ss (coerce "
  " 'cons))
          (is (car ss) #\newline)
        ))
        t)

      ("t for the same numbers"
        (is 2 2)
        t)

      ("t for the same numbers"
        (is 22/7 22/7)
        t)

      ("t for the same numbers"
        (is 1.618 1.618)
        t)

      ("t for the same strings"
        (is "foobar" "foobar")
        t)

      ("nil for different integers"
        (is 2 3)
        nil)

      ("nil for same numbers but different types"
        (is 3/4 0.75)
        nil)

      ("nil for different reals"
        (is 1.1234567 1.1234569)
        nil)

      ("nil for different strings"
        (is "foobar" "toto")
        nil)

      ("variadic for numbers"
        (is 1.23 1.23 1.23 1.23)
        t)

      ("variadic for different numbers"
        (is 1.23 1.23 1.23 3.14159265)
        nil)

      ("variadic for strings"
        (is "f" "f" "f" "f" "f" "f")
        t)

      ("variadic for different strings"
        (is "f" "f" "f" "g" "f" "f")
        nil)

      ("variadic for symbols"
        (is 'f 'f 'f 'f 'f 'f)
        t)

      ("variadic for different symbols"
        (is 'f 'f 'f 'g 'f 'f)
        nil)

      ("nil for different lists, even with identical content" ; see http://arclanguage.org/item?id=6985
        ((fn (a b) (is a b)) (list 'cons) (list 'cons))
        nil)

      ("t for empty lists, however"
        (is () ())
        t)
    )
  )))
