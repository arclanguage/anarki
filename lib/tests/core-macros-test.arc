(mac my-mac (x y) `(+ ,x ,y))

(mac my-plus (x y) `(+ ,x ,y))

(mac the-mac args
  (if (is nil args)
    1
    `(* ,(car args) (the-mac ,@(cdr args)))))

(mac make-pair (a b) `(cons ,a ,b))

(mac new-repeat-mac (x n)
  (if (is n 0)
    nil
    `(make-pair ,x (new-repeat-mac ,x ,(- n 1)))))

(assign test-repeat-mac
        (annotate 'mac
                  (fn (x n)
                    (if (is n 0)
                      nil
                      `(cons ,x (test-repeat-mac ,x ,(- n 1)))))))

(assign test-do
        (annotate 'mac (fn args `((fn () ,@args)))))

(assign test-mymac
        (annotate 'mac
                  (fn (name parms . body)
                    `(test-do (sref sig ',parms ',name)
                              (safeset ,name
                                       (annotate 'mac (fn ,parms ,@body)))))))

(assign test-recursive-mac
        (annotate 'mac
                  (fn (x)
                    (if (is x 0)
                      0
                      `(+ ,x (test-recursive-mac ,(- x 1)))))))

(assign test-make-pair
        (annotate 'mac (fn (a b) `(cons ,a ,b))))

(assign test-alternative-repeat-mac
        (annotate 'mac
                  (fn (x n)
                    (if (is n 0)
                      nil
                      `(test-make-pair ,x
                                       (test-alternative-repeat-mac ,x ,(- n 1)))))))

(suite macros
       (suite identity
              (test identity-non-macros
                    (assert-same '(my-fn 19 71)
                                 ((fn (my-fn) (macex '(my-fn 19 71)))
                                  (fn (x y) `(+ ,x ,y)))))
              (test identity-symbols
                    (assert-same 'foo (macex 'foo)))
              (test identity-atoms
                    (assert-same 13 (macex 13)))
              (test identity-undefined-symbols
                    (assert-same '(i-do-not-exist 'blah)
                                 (macex '(i-do-not-exist 'blah)))))
       (suite expansion
              (test expand-simple-do-macro
                    (assert-same '((fn () a b c))
                                 (macex '(test-do a b c))))
              (test expand-simple-plus-macro
                    (assert-same '(+ 19 71)
                                 (macex '(my-plus 19 71))))
              (test expand-recursive-macro
                    (assert-same (macex '(test-recursive-mac 3))
                                 '(+ 3 (test-recursive-mac 2))))
              (test expand-repeat-macro
                    (assert-same '(cons 'foo (test-repeat-mac 'foo 13))
                                 (macex '(test-repeat-mac 'foo 14))))
              (test nested-expansion
                    (assert-same '(cons 'foo
                                        (test-alternative-repeat-mac 'foo 13))
                                 (macex '(test-alternative-repeat-mac 'foo 14))))
              (test lots-of-expansion
                    (assert-same '((fn ()
                                     (sref sig '(x y) 'foo)
                                     (safeset foo
                                              (annotate 'mac
                                                        (fn (x y) (prn "foo" x y) `(,x y ,y))))))
                                 (macex '(test-mymac foo
                                                     (x y)
                                                     (prn "foo" x y)
                                                     `(,x y ,y))))))
       (suite invocation
              (test do
                    (assert-same 6.28
                                 ((fn (x y)
                                    (test-do (assign x 3.14)
                                             (assign y 2)
                                             (* x y)))
                                  nil
                                  nil)))
              (test simple-macro
                    (assert-same 90 (my-plus 19 71)))
              (test recursive-macro
                    (assert-same 210
                                 ((fn (my-mac-caller)
                                    (assign my-mac-caller
                                            (fn () (the-mac 5 6 7)))
                                    (my-mac-caller))
                                  nil)))
              (test repeat-macro
                    (assert-same '(foo foo foo)
                                 (test-repeat-mac 'foo 3)))
              (test nested-macro
                    (assert-same '(foo foo foo foo)
                                 (new-repeat-mac 'foo 4)))))

