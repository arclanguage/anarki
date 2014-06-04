(mac test-double (x) `(+ ,x ,x))

(suite bracket-fn
       directly (assert-same 225
                             ([* _ _]
                                 15))
       apply (assert-same 256
                          (apply [* _ _] '(16)))
       empty (assert-nil ([] 21)))

(suite apply
       add (assert-same 3
                        (apply + '(1 2)))
       inline-function (assert-same 51
                                    (apply (fn (x y) (* x y)) '(17 3)))
       passes-all-args (assert-same "abcd"
                                    (apply + "a" "b" '("c" "d"))))

(suite eval
       sum (assert-same 25
                        (eval '(+ 21 4)))
       inline (assert-same 64
                           (eval '( (fn (x y) (* x y)) 16 4))))
(suite ssyntax
       compose (assert-t (ssyntax 'a:b))
       complement-and-compose (assert-t (ssyntax '~a:b))
       complement (assert-t (ssyntax '~a))
       list (assert-t (ssyntax 'a.b))
       list-quoted (assert-t (ssyntax 'a!b))
       and (assert-t (ssyntax 'a&b))
       and-2 (assert-t (ssyntax '&a&b&))
       (suite invocation
              direct (assert-same 6.5
                                  ((fn ()
                                       (sqrt:+ 40 2.25))))
              macro-invocation (assert-same 32
                                            (coerce (test-double:sqrt 256) 'int))
              invoke-as-parameter (assert-same 11.0
                                               ((fn (addand)
                                                    (addand sqrt:* 5 20 1.0)) (fn (op x y z) (+ z (op x y)))))))
(suite ssexpand
       compose (assert-same '(compose x y)
                            (ssexpand 'x:y))
       complement (assert-same '(complement p)
                               (ssexpand '~p))
       compose-complement (assert-same '(compose p (complement q) r)
                                       (ssexpand 'p:~q:r))
       complement-compose (assert-same '(compose (complement p) q r)
                                       (ssexpand '~p:q:r))
       compose-with-float (assert-same '(compose x 1.2)
                                       (ssexpand 'x:1.2)) ; bizarre but true
       result-of-compose-with-float (assert-same 'num
                                                 (type ((ssexpand 'x:1.2) 2))) ; bizarre but true
       list (assert-same '((* a) b)
                         (ssexpand '*.a.b))
       quoted-list (assert-same '((cons (quote a)) (quote b))
                                (ssexpand 'cons!a!b))
       chained-dots-and-bangs (assert-same '(((a b) (quote c)) d)
                                           (ssexpand 'a.b!c.d))
       initial-dot (assert-same '(((get a) b) c)
                                (ssexpand '.a.b.c))
       initial-quote (assert-same '(((get (quote a)) b) c)
                                  (ssexpand '!a.b.c))
       andf (assert-same '(andf a b)
                         (ssexpand 'a&b)))

(suite special-syntax
       embedded-nil (assert-same (list nil)
                                 list.nil)
       expands-numbers-too (assert-same #\o
                                        ((fn (s) s.1) "foo"))
       everything-in-functional-position (assert-nil (let x (fn(n) (fn(p) (is (mod n p) 0)))
                                                          (~odd&x.9 3)))
       everything-as-argument (assert-same '(nil t t)
                                (let x (fn(n) (fn(p) (is (mod n p) 0)))  ; contrived 'not a factor of 9' function
                                     (map ~odd&x.9 '(3 4 5)))))
