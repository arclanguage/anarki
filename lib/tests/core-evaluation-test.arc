(mac test-double (x) `(+ ,x ,x))

(register-test '(suite "Foundation Tests"
  (suite "Evaluation"
    (suite "[ _ ] shortcut"
      ("call directly"
        ([* _ _] 15)
        225)

      ("useful in apply"
        (apply [* _ _] '(16))
        256)

      ("handles empty bracket-fn"
        ([] 21)
        nil))

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
        (a b c d e f g h)))

    (suite "eval"
      ("a simple sum function"
        (eval '(+ 21 4))
        25)

      ("an inline function invocation"
        (eval '( (fn (x y) (* x y)) 16 4))
        64))

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

      ("andf"
        (ssyntax 'a&b)
        t)

      ("andf"
        (ssyntax '&a&b&)
        t)
    )

    (suite "special syntax invocation (compose is implemented in Arc)"
      ("direct invocation"
        ((fn ()
          (sqrt:+ 40 2.25)))
        6.5 )

      ("compose macro invocation"
        (coerce (test-double:sqrt 256) 'int)
        32)

      ("invoke as parameter"
        ((fn (addand)
          (addand sqrt:* 5 20 1.0)) (fn (op x y z) (+ z (op x y))))
        11.0 )))))

(register-test '(suite "ssexpand"
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

  ("expand compose with numbers"
    (ssexpand 'x:1.2)
    (compose x 1.2))              ; bizarre but true

  ("expand compose with numbers"
    (type ((ssexpand 'x:1.2) 2))
    num)                          ; bizarre but true

  ("expand list"
    (ssexpand '*.a.b)
    ((* a) b))

  ("expand quoted list"
    (ssexpand 'cons!a!b)
    ((cons (quote a)) (quote b)) )

  ("expand chained dots and bangs"
    (ssexpand 'a.b!c.d)
    (((a b) (quote c)) d))

  ("ssexpand with initial dot"
    (ssexpand '.a.b.c)
    (((get a) b) c))

  ("ssexpand with initial quote"
    (ssexpand '!a.b.c)
    (((get (quote a)) b) c))

  ("andf"
    (ssexpand 'a&b)
    (andf a b))
))

(register-test '(suite "using special syntax"
  ("ssyntax copes with embedded nil"
    list.nil
    (nil))

  ("ssyntax expands numbers too"
    ((fn (s) s.1) "foo")
    #\o)

  ("everything at once, in functional position"
    (let x (fn(n) (fn(p) (is (mod n p) 0)))
      (~odd&x.9 3))
    nil)

  ("everything at once, as argument"
    (let x (fn(n) (fn(p) (is (mod n p) 0)))
      (map ~odd&x.9 '(3 4 5)))  ; contrived 'not a factor of 9' function
    (nil t t))

    ))
