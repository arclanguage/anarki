(mac my-mac (x y) `(+ ,x ,y))
(mac my-plus (x y) `(+ ,x ,y))
(mac the-mac args (if (is nil args) 1 `(* ,(car args) (the-mac ,@(cdr args)))))
(mac make-pair (a b) `(cons ,a ,b))
(mac new-repeat-mac (x n) (if (is n 0) nil `(make-pair ,x (new-repeat-mac ,x ,(- n 1)))))

(register-test '(suite "Foundation Tests"
  (suite "Macros"
    (suite "expansion"
      ("identity for non-macros"
        ((fn ()
          (set my-fn (fn (x y) `(+ ,x ,y)))
          (macex '(my-fn 19 71))
        ))
        (my-fn 19 71))

      ("identity for atoms"
        ((fn ()
          (macex 'foo)
        ))
        foo)

      ("identity for atoms, really"
        ((fn ()
          (macex 13)
        ))
        13)

      ("identity for undefined symbols"
        (macex '(i-do-not-exist 'blah))
        (i-do-not-exist 'blah) )

      ("expand a simple macro"
        ((fn ()
          (set do (annotate 'mac
                    (fn args `((fn () ,@args)))))
          (macex '(do a b c))
        ))
        ((fn () a b c)))

      ("expand a simple macro"
        ((fn ()
          (macex '(my-plus 19 71))
        ))
        (+ 19 71))

      ("expand a recursive macro"
        ((fn ()
          (set my-mac (annotate 'mac (fn (x) (if (is x 0) 0 `(+ ,x (my-mac ,(- x 1)))))))
          (macex '(my-mac 3))
        ))
        (+ 3 (my-mac 2)))

      ("expand a repeat macro"
        ((fn ()
          (set repeat-mac (annotate 'mac (fn (x n) (if (is n 0) nil `(cons ,x (repeat-mac ,x ,(- n 1)))))))
          (macex '(repeat-mac 'foo 14))
        ))
        (cons 'foo (repeat-mac 'foo 13)))

      ("nested expansion"
        ((fn ()
          (set make-pair (annotate 'mac (fn (a b) `(cons ,a ,b))))
          (set repeat-mac (annotate 'mac (fn (x n) (if (is n 0) nil `(make-pair ,x (repeat-mac ,x ,(- n 1)))))))
          (macex '(repeat-mac 'foo 14))
        ))
        (cons (quote foo) (repeat-mac (quote foo) 13)))

      ("more expansion"
        ((fn ()
          (set do (annotate 'mac
                    (fn args `((fn () ,@args)))))
          (set mac (annotate 'mac
                     (fn (name parms . body)
                       `(do (sref sig ',parms ',name)
                            (safeset ,name (annotate 'mac (fn ,parms ,@body)))))))
          (macex '(mac foo (x y) (prn "foo" x y) `(,x y ,y)))
        ))
        ((fn () (sref sig (quote (x y)) (quote foo)) (safeset foo (annotate (quote mac) (fn (x y) (prn "foo" x y) (quasiquote ((unquote x) y (unquote y))))))))
      )
    )

    (suite "invocation"
      ("do"
        ((fn ()
          (set do (annotate 'mac
                    (fn args `((fn () ,@args)))))
          (do (set x 3.14) (set y 2) (* x y))
        ))
        6.28 )

      ("invoke a simple macro"
        (my-plus 19 71)
        90 )

      ("invoke a recursive macro"
        ((fn ()
          (set my-mac-caller (fn () (the-mac 5 6 7)))
          (my-mac-caller)
        ))
        210 )

      ("repeat macro"
        ((fn ()
          (set repeat-mac (annotate 'mac (fn (x n) (if (is n 0) nil `(cons ,x (repeat-mac ,x ,(- n 1)))))))
          (repeat-mac 'foo 3)
        ))
        (foo foo foo) )

      ("nested macro"
        (new-repeat-mac 'foo 4)
        (foo foo foo foo))
    )
  )))
