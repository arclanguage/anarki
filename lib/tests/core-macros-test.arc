(mac my-mac (x y) `(+ ,x ,y))
(mac my-plus (x y) `(+ ,x ,y))
(mac the-mac args (if (is nil args) 1 `(* ,(car args) (the-mac ,@(cdr args)))))
(mac make-pair (a b) `(cons ,a ,b))
(mac new-repeat-mac (x n) (if (is n 0) nil `(make-pair ,x (new-repeat-mac ,x ,(- n 1)))))

(assign test-repeat-mac
        (annotate 'mac
                  (fn (x n)
                      (if (is n 0)
                          nil
                          `(cons ,x (test-repeat-mac ,x ,(- n 1)))))))

(assign test-do
       (annotate 'mac
                 (fn args `((fn () ,@args)))))

(assign test-mymac
       (annotate 'mac
                 (fn (name parms . body)
                    `(test-do (sref sig ',parms ',name)
                              (safeset ,name (annotate 'mac (fn ,parms ,@body)))))))

(assign test-recursive-mac
        (annotate 'mac
                  (fn (x)
                      (if (is x 0)
                          0
                          `(+ ,x (test-recursive-mac ,(- x 1)))))))

(assign test-make-pair
        (annotate 'mac
                  (fn (a b)
                      `(cons ,a ,b))))

(assign test-alternative-repeat-mac
        (annotate 'mac
                  (fn (x n)
                      (if (is n 0)
                          nil
                          `(test-make-pair ,x (test-alternative-repeat-mac ,x ,(- n 1)))))))

(register-test '(suite "Foundation Tests"
  (suite "Macros"
    (suite "expansion"
      ("identity for non-macros"
        ((fn (my-fn)
          (macex '(my-fn 19 71))) (fn (x y) `(+ ,x ,y)))
        (my-fn 19 71))

      ("identity for atoms"
        (macex 'foo)
        foo)

      ("identity for atoms, really"
        (macex 13)
        13)

      ("identity for undefined symbols"
        (macex '(i-do-not-exist 'blah))
        (i-do-not-exist 'blah) )

      ("expand a simple macro"
        (macex '(test-do a b c))
        ((fn () a b c)))

      ("expand a simple macro"
        (macex '(my-plus 19 71))
        (+ 19 71))

      ("expand a recursive macro"
        (macex '(test-recursive-mac 3))
        (+ 3 (test-recursive-mac 2)))

      ("expand a repeat macro"
        (macex '(test-repeat-mac 'foo 14))
        (cons 'foo (test-repeat-mac 'foo 13)))

      ("nested expansion"
        (macex '(test-alternative-repeat-mac 'foo 14))
        (cons 'foo (test-alternative-repeat-mac 'foo 13)))

      ("more expansion"
        (macex '(test-mymac foo (x y) (prn "foo" x y) `(,x y ,y)))
        ((fn ()
             (sref sig (quote (x y)) (quote foo))
             (safeset foo
                      (annotate (quote mac)
                                (fn (x y)
                                    (prn "foo" x y)
                                    (quasiquote ((unquote x) y (unquote y))))))))))

    (suite "invocation"
      ("do"
        ((fn (x y) (test-do (assign x 3.14)
                            (assign y 2)
                            (* x y))) nil nil)
        6.28)

      ("invoke a simple macro"
        (my-plus 19 71)
        90)

      ("invoke a recursive macro"
        ((fn (my-mac-caller)
          (assign my-mac-caller (fn () (the-mac 5 6 7)))
          (my-mac-caller)) nil)
        210)

      ("repeat macro"
        (test-repeat-mac 'foo 3)
        (foo foo foo))

      ("nested macro"
        (new-repeat-mac 'foo 4)
        (foo foo foo foo))))))
