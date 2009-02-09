(mac test-double (x) `(+ ,x ,x))

(register-test '(suite "Foundation Tests"
  (suite "Evaluation"
    (suite "_ shortcut"
      ("call directly"
        ([* _ _] 15)
        225
      )

      ("useful in apply"
        (apply [* _ _] '(16))
        256
      )
    )

    (suite "apply"
      ("a simple sum function"
        (apply + '(1 2))
        3)

      ("an inline function"
        (apply (fn (x y) (* x y)) '(17 3))
        51)

      ("passes all args to function"
        (apply + "a" "b" '("c" "d"))
        "abcd" )

      ("passes all args to function"
        (apply + '(a b) '(c d) '((e f) (g h)))
        (a b c d e f g h) )
    )

    (suite "eval"
      ("a simple sum function"
        (eval '(+ 21 4))
        25)

      ("an inline function invocation"
        (eval '( (fn (x y) (* x y)) 16 4))
        64)
    )

    (suite "ssexpand"
      ("expand compose"
        (ssexpand 'x:y)
        (compose x y))

      ("expand complement"
        (ssexpand '~p)
        (complement p))

      ("expand compose/complement"
        (ssexpand 'p:~q:r)
        (compose p (complement q) r) )

      ("expand compose/complement"
        (ssexpand '~p:q:r)
        (compose (complement p) q r) )

      ("expand list"
        (ssexpand '+.a.b)
        (+ a b))

      ("expand quoted list"
        (ssexpand 'cons!a!b)
        (cons (quote a) (quote b)) )
    )

    (suite "ssyntax"
      ("recognises compose"
        (ssyntax 'a:b)
        t )

      ("recognises complement and compose"
        (ssyntax '~a:b)
        t )

      ("recognises complement alone"
        (ssyntax '~a)
        t )

      ("recognises list"
        (ssyntax 'a.b)
        t )

      ("recognises list-quoted"
        (ssyntax 'a!b)
        t )
    )

    (suite "special syntax invocation (compose is implemented in Arc)"
      ("direct invocation"
        ((fn ()
          (sqrt:+ 40 2.25)
        ))
        6.5 )

      ("compose macro invocation"
        (coerce (test-double:sqrt 256) 'int)
        32)

      ("invoke as parameter"
        ((fn ()
          (set addand (fn (op x y z) (+ z (op x y))))
          (addand sqrt:* 5 20 1.0)
        ))
        11.0 )
    )
  )))
