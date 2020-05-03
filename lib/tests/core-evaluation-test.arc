(mac test-double (x) `(+ ,x ,x))

(suite bracket-fn
       (test directly
             (assert-same 225 ([* _ _] 15)))
       (test apply
             (assert-same 256 (apply [* _ _] '(16))))
       (test empty (assert-nil ([] 21))))

(suite apply
       (test add (assert-same 3 (apply + '(1 2))))
       (test inline-function
             (assert-same 51
                          (apply (fn (x y) (* x y)) '(17 3))))
       (test passes-all-args
             (assert-same "abcd"
                          (apply + "a" "b" '("c" "d")))))

(suite eval
       (test sum (assert-same 25 (eval '(+ 21 4))))
       (test inline
             (assert-same 64
                          (eval '((fn (x y) (* x y)) 16 4)))))

(suite ssyntax
       (test compose (assert-t (ssyntax 'a:b)))
       (test complement-and-compose
             (assert-t (ssyntax '~a:b)))
       (test complement (assert-t (ssyntax '~a)))
       (test list (assert-t (ssyntax 'a.b)))
       (test list-quoted (assert-t (ssyntax 'a!b)))
       (test and (assert-t (ssyntax 'a&b)))
       (test and-2 (assert-t (ssyntax '&a&b&)))
       (suite invocation
              (test direct
                    (assert-same 6.5 ((fn () (sqrt:+ 40 2.25)))))
              (test macro-invocation
                    (assert-same 32
                                 (coerce (test-double:sqrt 256) 'int)))
              (test invoke-as-parameter
                    (assert-same 11.0
                                 ((fn (addand) (addand sqrt:* 5 20 1.0))
                                  (fn (op x y z) (+ z (op x y))))))))

(suite ssexpand
       (test compose
             (assert-same '(compose x y) (ssexpand 'x:y)))
       (test complement
             (assert-same '(complement p) (ssexpand '~p)))
       (test compose-complement
             (assert-same '(compose p (complement q) r)
                          (ssexpand 'p:~q:r)))
       (test complement-compose
             (assert-same '(compose (complement p) q r)
                          (ssexpand '~p:q:r)))
       (test compose-with-float
             (assert-same '(compose x 1.2)
                          (ssexpand 'x:1.2)))
       (test result-of-compose-with-float
             (assert-same 'num
                          (type ((ssexpand 'x:1.2) 2))))
       (test list
             (assert-same '((* a) b) (ssexpand '*.a.b)))
       (test quoted-list
             (assert-same '((cons 'a) 'b)
                          (ssexpand 'cons!a!b)))
       (test chained-dots-and-bangs
             (assert-same '(((a b) 'c) d)
                          (ssexpand 'a.b!c.d)))
       (test initial-dot
             (assert-same '(((get a) b) c)
                          (ssexpand '.a.b.c)))
       (test initial-quote
             (assert-same '(((get 'a) b) c)
                          (ssexpand '!a.b.c)))
       (test andf
             (assert-same '(andf a b) (ssexpand 'a&b))))

(suite special-syntax
       (test embedded-nil
             (assert-same (list nil) list.nil))
       (test expands-numbers-too
             (assert-same #\o ((fn (s) s.1) "foo")))
       (test everything-in-functional-position
             (assert-nil (let x (fn (n) (fn (p) (is (mod n p) 0)))
                           (~odd&x.9 3))))
       (test everything-as-argument
             (assert-same '(nil t t)
                          (let x (fn (n) (fn (p) (is (mod n p) 0)))
                            (map ~odd&x.9 '(3 4 5))))))

