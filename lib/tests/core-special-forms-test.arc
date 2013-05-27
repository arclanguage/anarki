(mac plusall args `(+ ,@args))
(mac act-of-god () 'earthquake)
(mac overwrite (after) `(+ ,after 3))

(register-test '(suite "Foundation Tests"
  (suite "Special Forms"
    (suite "Quotation"
      (suite "quote"
        ("quotation"
          'a
          a)

        ("quotes its argument"
          (quote a)
          a)

        ("quotes a list argument"
          (quote (1 2 3))
          (1 2 3))

        ("quotes a list of symbols"
          (quote (fooble bar))
          (fooble bar))

        ("quote char as shortcut"
          'foogle
          foogle )
      )

      (suite "quasiquote"
        ("quasi-quotation"
          '`qqfoo
          (quasiquote qqfoo) )

        ("unquote"
          ',uqfoo
          (unquote uqfoo)
        )

        ("unquote-splicing"
          ',@uqsfoo
          (unquote-splicing uqsfoo) )

        ("quasi-quotation is just like quotation"
          `qqqfoo
          qqqfoo )

        ("quasi-quote quasi-quote"
          ``double-qq
          `double-qq )

        ("quasiquotation unquote is identity"
          ( (fn (x) `,x) "foo" )
          "foo" )

        ("quasiquotation of list with unquote"
          `(1 2 ,(+ 1 2))
          (1 2 3) )

        ("quasiquotation of list with unquote"
          `(a b c (+ 1 2 ,(+ 1 2)))
          (a b c (+ 1 2 3)) )

        ("quasiquotation of list with unquote in first position"
          `(,(+ 1 2) 2 1)
          (3 2 1) )

        ("quasiquotation of list with unquote-splicing"
          `(a b c (+ 1 2 ,@(cons 3 (cons 4 nil))))
          (a b c (+ 1 2 3 4)) )

        ("quasiquotation of list with unquote-splicing in first position"
          `(,@(cons 3 (cons 4 nil)) a b c ,(+ 0 5))
          (3 4 a b c 5) )

        ("don't expand nested quasiquotes"
          `(* 17 ,(plusall 1 2 3) `(+ 1 ,(plusall 1 2 3)))
          (* 17 6 `(+ 1 ,(plusall 1 2 3))))

        ("quasiquote uses local namespace"
          ((fn (x) (overwrite (+ x x))) 7)
          17
        )

        ("nested quasiquote with double unquote"
          ((fn (qqq) `(a b qqq ,qqq `(a b qqq ,qqq ,,qqq))) 'qoo)
          (a b qqq qoo `(a b qqq ,qqq ,qoo)))

        ("more nested quasiquote"
          ((fn (x) ``,,x) 'y)
          `,y)
      )
    )

    (suite "If"
      ("returns first argument if only one argument"
        (if 12)
        12 )

      ("any non-nil atom is t"
        (if 'toto 'titi)
        titi )

      ("returns nil if condition is false"
        (if (is 12 13) 'foo)
        nil )

      ("returns value corresponding to first true condition"
        (if (is 12 13) 'foo
            (is 'foo 'bar) 'toto
            (is "a" "a") "yadda"
            'doh
        )
        "yadda" )

      ("returns default value corresponding if no true condition"
        (if (is 12 13) 'foo
            (is 'foo 'bar) 'toto
            'doh
        )
        doh )
    )

    (suite "Function Definition"
      ("a simple no-args function that returns a string"
        ( (fn () "foobar") )
        "foobar")

      ("single rest arg"
        ( (fn args cdr.args) 'a 'b 'c )
        (b c))

      ("a simple addition function"
        ( (fn (x y) (+ x y)) 17 13)
        30)

      ("a simple addition function with varargs"
        ( (fn args (apply + args)) 17 13 14 16)
        60)

      ("a simple function with a rest parameter"
        ( (fn (a b . c) (* (- a b) (apply + c))) 20 15 19 20 21)
        300)

      ("set a value inside a function"
        ( (fn () (assign foo "bar") foo) )
        "bar")

      ("nested variables are lexically scoped"
        ( (fn ()
          ( (fn (x)
              (assign toto (fn () x))) 99)
          (toto)) )
        99)

      ("nested variables are lexically scoped - really"
        ((fn ()
          ((fn (a)
            ((fn (b)
              ((fn (c)
                (assign abcfun (fn (x) (+ a b c x)))
              ) "toto")
            ) "bar")
          ) "foo")
          (abcfun "-foobartoto")
        ))
        "foobartoto-foobartoto")

      ("optional arguments fill in missing arguments"
        ((fn (x y (o z 2))
          (+ x y z)
        ) 31 41)
        74)

      ("optional values ignored if value provided"
        ((fn (x y (o z 2))
          (+ x y z)
        ) 31 41 3)
        75)

      ("defaults for optionals may be other args"
        ((fn (x y (o z x))
          (+ x y z)
        ) 16 33)
        65)

      ("optional args evaluated at invocation time, but in lexical scope of fn definition"
        ((fn (z)
          (assign test-opt-arg (fn (x (o y z)) (cons x y)))
          ( (fn (z) (test-opt-arg 2)) 'zoo)
        ) 'goo)
        (2 . goo))

      ("optional arg may be invocation"
        ((fn (z)
          (assign test-opt-arg (fn (x (o y (+ z z))) (cons x y)))
          ( (fn (z) (test-opt-arg 2)) 101)
        ) 25)
        (2 . 50))

      ("o is not always an optional arg"
        ((fn ()
          (assign fioip (fn ((i o ip)) o))
          (fioip '(ifoo obar "iptoto"))
        ))
        obar)

      ("special case: 2nd arg is optional and refers to outer lexical scope"
        ((fn (x)
          ( (fn (a (o b x)) nil b) "ignored")
        ) "expected")
        "expected")

      ("destructuring-bind params"
        ((fn (a b (c d e (f g)) h) `(,a ,b ,c ,d ,e ,f ,g ,h)) 1 2 '(3 4 5 (6 7)) 8)
         (1 2 3 4 5 6 7 8))

      ("destructuring-bind params with nested optional argument"
        ((fn (a b (c d e (f (o g 22))) h) `(,a ,b ,c ,d ,e ,f ,g ,h)) 1 2 '(3 4 5 (6)) 8)
         (1 2 3 4 5 6 22 8))

      ("destructuring-bind in first place"
          ((fn (out)
            (assign foo (fn ((opt val) . rest) (disp opt) (disp "=") (disp val) (disp " ")  (if (is (car rest) nil) '() (foo (car rest) (cdr rest)))))
            (call-w/stdout out (fn () (foo '(id 4) '(class "myclass"))))
            (inside out)
          ) (outstring))
        "id=4 class=myclass ")

      ("destructured args are implicitly optional"
        ( (fn (a (b c d)) (+ a b c d)) "foo" '("bar") )
        "foobar")

      ("extra destructured args are ignored"
        ( (fn (a (b c d)) (+ a b c d)) "foo" '("bar" "baz" "toto" "extra" "and some more") )
        "foobarbaztoto")

      ("empty body returns nil"
        ((fn ()))
        nil)

      ("rainbow optimises inline idfn"
        ( (fn (x) (if x x nil)) "en vacances au sud de france")
        "en vacances au sud de france")

      ("allow nil as param name"
        ( (fn (nil) nil) 3)
        nil)
    )

    (suite "assign"
      ("sets a value in the top namespace"
        ( (fn () (assign earthquake 10.3) earthquake))
        10.3)

      ("macro-expands first argument"
        ((fn ()
            (assign (act-of-god) 8.9)
            earthquake))
        8.9)

      ("sets several values at once"
        ( (fn ()
              (assign volcano 2.4 (act-of-god) 10.3 tsunami 15.3 avalanche 4.1)
              `(,tsunami ,volcano ,avalanche ,earthquake)))
        (15.3 2.4 4.1 10.3))
    )
  )))
