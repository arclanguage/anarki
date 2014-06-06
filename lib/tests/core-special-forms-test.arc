(mac plusall args `(+ ,@args))
(mac act-of-god () 'earthquake)
(mac overwrite (after) `(+ ,after 3))

(suite special-forms
       (suite quote
              quotes-symbol (assert-same 'a
                                         (quote a))
              quotes-list-of-numbers (assert-same '(1 2 3)
                                                  (quote (1 2 3)))
              quotes-list-of-symbols (assert-same '(fooble bar)
                                                  (quote (fooble bar))))
       (suite quasiquote
              backquote-is-quasiquote (assert-same '(quasiquote qqfoo)
                                                   '`qqfoo)
              comma-is-unquote (assert-same '(unquote uqfoo)
                                 ',uqfoo)
              comma-at-is-unquote-splice (assert-same '(unquote-splicing uqsfoo)
                                           ',@uqsfoo)
              quasiquote-quotes-symbols (assert-same `qqqfoo
                                                     'qqqfoo)
              double-quasiquote (assert-same '(quasiquote double-qq)
                                             ``double-qq)
              unquote-is-value (assert-same "foo"
                                            ((fn (x) `,x) "foo"))
              unquote-evals-argument (assert-same '(1 2 3)
                                       `(1 2 ,(+ 1 2)))
              unquote-looks-deep-inside-lists (assert-same '(a b c (+ 1 2 3))
                                                           `(a b c (+ 1 2 ,(+ 1 2))))
              unquote-in-first-position (assert-same '(3 2 1)
                                                     `(,(+ 1 2) 2 1))
              unquote-splices-in-list (assert-same '(a b c (+ 1 2 3 4))
                                                   `(a b c (+ 1 2 ,@(cons 3 (cons 4 nil)))))
              unquote-splices-list-in-first-position (assert-same '(3 4 a b c 5)
                                                                  `(,@(cons 3 (cons 4 nil)) a b c ,(+ 0 5)))
              nested-quasiquotes-arent-expanded (assert-same '(* 17 6 `(+ 1 ,(plusall 1 2 3)))
                                                             `(* 17 ,(plusall 1 2 3) `(+ 1 ,(plusall 1 2 3))))
              quasiquote-uses-local-namespace (assert-same 17
                                                           ((fn (x) (overwrite (+ x x))) 7))
              double-unquote (assert-same '(a b qqq qoo `(a b qqq ,qqq ,qoo))
                                          ((fn (qqq) `(a b qqq ,qqq `(a b qqq ,qqq ,,qqq))) 'qoo))
              double-unquote-in-function (assert-same '`,y
                                                      ((fn (x) ``,,x) 'y)))
       (suite if
              when-one-arg-returns-it (assert-same 12
                                                   (if 12))
              non-nil-atom-is-t (assert-same 'titi
                                             (if 'toto 'titi))
              false-condition-with-two-args-returns-nil (assert-nil (if (is 12 13) 'foo))
              returns-value-paired-with-first-true-condition (assert-same "yadda"
                                                                          (if (is 12 13) 'foo
                                                                            (is 'foo 'bar) 'toto
                                                                            (is "a" "a") "yadda"
                                                                            'doh))
              default-value-is-returned-when-no-true-condition (assert-same 'doh
                                                                            (if (is 12 13) 'foo
                                                                              (is 'foo 'bar) 'toto
                                                                              'doh)))

       (suite function-definition
              invoked-no-arg-function (assert-same "foobar"
                                                   ((fn () "foobar")))
              single-rest-arg (assert-same '(b c)
                                           ((fn args cdr.args) 'a 'b 'c ))
              body-is-addition (assert-same 30
                                 ((fn (x y) (+ x y)) 17 13))
              apply-plus-to-rest-arg (assert-same 60
                                                  ((fn args (apply + args)) 17 13 14 16))
              rest-arg-with-other-args (assert-same 300
                                                    ((fn (a b . c) (* (- a b) (apply + c))) 20 15 19 20 21))
              assign-inside-function (assert-same "bar"
                                                  ((fn () (assign foo "bar") foo)))
              nested-variables-are-lexically-scoped (assert-same 99
                                                                 ((fn ()
                                                                      ((fn (x)
                                                                           (assign toto (fn () x))) 99)
                                                                      (toto))))
              extremely-nested-variables-are-lexically-scoped (assert-same "foobartoto-foobartoto"
                                                                           ((fn ()
                                                                                ((fn (a)
                                                                                     ((fn (b)
                                                                                          ((fn (c)
                                                                                               (assign abcfun (fn (x) (+ a b c x)))
                                                                                               ) "toto")
                                                                                          ) "bar")
                                                                                     ) "foo")
                                                                                (abcfun "-foobartoto") )))
              optional-arguments-take-default (assert-same 74
                                                           ((fn (x y (o z 2))
                                                                (+ x y z)
                                                                ) 31 41))
              optional-arguments-can-have-value-provided (assert-same 75
                                                                      ((fn (x y (o z 2))
                                                                           (+ x y z)
                                                                           ) 31 41 3))
              optional-argument-default-can-be-other-arg (assert-same 65
                                                                      ((fn (x y (o z x))
                                                                           (+ x y z)
                                                                           ) 16 33))
              optional-argument-evaluated-at-invocation-time (assert-same '(2 . goo)
                                                                          ((fn (z)
                                                                               (assign test-opt-arg (fn (x (o y z)) (cons x y)))
                                                                               ( (fn (z) (test-opt-arg 2)) 'zoo)
                                                                               )'goo))
              optional-argument-default-can-be-function-call (assert-same '(2 . 50)
                                                                          ((fn (z)
                                                                               (assign test-opt-arg (fn (x (o y (+ z z))) (cons x y)))
                                                                               ((fn (z) (test-opt-arg 2)) 101)
                                                                               ) 25))
              o-can-be-name-of-argument (assert-same 'obar
                                                     ((fn ()
                                                          (assign fioip (fn ((i o ip)) o))
                                                          (fioip '(ifoo obar "iptoto"))
                                                          )))
              optional-argument-can-refer-to-outer-scope (assert-same "expected"
                                                                      ((fn (x)
                                                                           ((fn (a (o b x)) nil b) "ignored")
                                                                           ) "expected"))
              arguments-can-destructuring-bind (assert-same '(1 2 3 4 5 6 7 8)
                                                            ((fn (a b (c d e (f g)) h) `(,a ,b ,c ,d ,e ,f ,g ,h)) 1 2 '(3 4 5 (6 7)) 8))
              optional-arguments-can-destructuring-bind (assert-same '(1 2 3 4 5 6 22 8)
                                                                     ((fn (a b (c d e (f (o g 22))) h) `(,a ,b ,c ,d ,e ,f ,g ,h)) 1 2 '(3 4 5 (6)) 8))
              can-have-arguments-after-destructuring-bind (assert-same "id=4 class=myclass "
                                                                       ((fn (out)
                                                                            (assign foo (fn ((opt val) . rest) (disp opt) (disp "=") (disp val) (disp " ")  (if (is (car rest) nil) '() (foo (car rest) (cdr rest)))))
                                                                            (call-w/stdout out (fn () (foo '(id 4) '(class "myclass"))))
                                                                            (inside out)
                                                                            ) (outstring)))
              destructured-args-are-optional (assert-same "foobar"
                                                          ((fn (a (b c d)) (+ a b c d)) "foo" '("bar")))
              extra-destructured-args-are-ignored (assert-same "foobarbaztoto"
                                                    ((fn (a (b c d)) (+ a b c d)) "foo" '("bar" "baz" "toto" "extra" "and some more")))
              empty-body-is-nil (assert-nil ((fn ())))
              nil-can-be-argument-name (assert-nil ((fn (nil) nil) 3)))
       (suite assign
              sets-value-in-top-namespace (assert-same 10.3
                                                       ((fn () (assign earthquake 10.3) earthquake)))
              macro-expands-first-argument (assert-same 8.9
                                                        ((fn ()
                                                             (assign (act-of-god) 8.9)
                                                             earthquake)))
              sets-several-values-at-once (assert-same '(15.3 2.4 4.1 10.3)
                                                       ((fn ()
                                                             (assign volcano 2.4 (act-of-god) 10.3 tsunami 15.3 avalanche 4.1)
                                                             `(,tsunami ,volcano ,avalanche ,earthquake))))))
