(register-test '(suite "Foundation Tests"
  (suite "Lists"
    (suite "cons"
      ("cons creates a list"
        (cons 'a '(b c))
        (a b c))

      ("cons conses two strings"
        (cons "a" "b")
        ("a" . "b"))
    )

    (suite "car"
      ("car of nil is nil"
        (car nil)
        nil)

      ("car of empty list is nil"
        (car '())
        nil)

      ("car - no need to quote empty list"
        (car ())
        nil)

      ("car returns car of argument"
        (car '(foo 12.34 "bar"))
        foo)
    )

    (suite "cdr"
      ("cdr returns cdr of argument"
        (cdr '("foo" bar 123.45))
        (bar 123.45))

      ("cdr of empty list is nil"
        (cdr ())
        nil)

      ("'each' macro relies on cdr of empty list returning nil"
        ((fn stuff
             ((fn (outer)
                  (((fn (self)
                         (assign self
                                 (fn (inner)
                                     (if (is (type inner) 'cons)
                                         ((fn ()
                                              ((fn (x)
                                                   (disp x))
                                               (car inner))
                                     (self (cdr inner))))) )))
                     nil)
                    outer))
              stuff)))
        nil)
    )

    (suite "scar"
      ("sets the first element of a list"
        ( (fn (x) (scar x 99) x ) '(1 2 3))
        (99 2 3) )

      ("sets the first character of a string"
        ( (fn (x) (scar x #\b) x ) "foo" )
        "boo" )
    )

    (suite "scdr"
      ("scdr sets the remainder of a list"
        ( (fn (x) (scdr x '(a a)) x ) '(a b c) )
        (a a a) )

      ("scdr sets the remainder of any list"
        ( (fn (x) (scdr (cdr (cdr x)) '(d e f)) x ) '(a b c) )
        (a b c d e f) )
    )

    ("get the size of a list"
      (len '(a b c d e f g))
      7 )

    ("set an element of a list"
      ( (fn (lst) (sref lst 'b 0) lst) '(a b c) )
      (b b c) )

    ("get an element of a list"
      ( '(a b c d) 2 )
      c )
  )))
