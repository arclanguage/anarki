(def test-stack-assignment (x)
  (assign x (+ x 1))
  x)

(def test-break-closure (x)
  (let y (* x 2)
    (atomic-invoke (fn () (fn () (+ x y))))))

(register-test '(suite "miscellaneous tests"
                       ("w/bars should ignore nil elements" (do (declare 'atstrings t)
                                                                (eval '(tostring 
                                                                         (w/bars (pr "baa") nil (pr "floop") nil))))
                                                            "baa | floop")
                       ("make sure faster no works properly" (map no
                                                                  '(a nil 4 nil (x y z) nil (obj x 1 y 2) nil))
                                                             (nil t nil t nil t nil t))
                       ("macex1 is implemented" (macex1 '(let a 10 (pr a)))
                                                (with (a 10) (pr a)))
                       (suite "optional args nested inside destructuring args"
                              ("simple case: no destructuring" (let default 2 ((fn ((o x default)) x)))
                                                               2)
                              ("destructuring with solo optional arg" (let default 2 (let ((o x default)) nil x))
                                                                      2)
                              ("destructuring alongside another arg" (let default 2
                                                                       (let (a (o b default)) '(1) b))
                                                                     2))
                       (suite "watch out for dangerous optimisations"
                              ("special case: swap two values" ((fn (x y)
                                                                  (let tmp x
                                                                    (assign x y)
                                                                    (assign y tmp)
                                                                    `(,x ,y)))
                                                                1
                                                                2)
                                                               (2 1))
                              ("assignment within a function" (test-stack-assignment 10)
                                                              11)
                              ("closure with no args referencing outer args" ((test-break-closure 2))
                                                                             6)
                              ("special case: inlining literals; scheme and rainbow do the same thing (shouldn't this be a bug?)" ((fn (f) (cons (f) (f)))
                                                                                                                                   (fn ()
                                                                                                                                     ((fn (xs) (scdr xs (cons 'x (cdr xs))) xs)
                                                                                                                                      '(a b c))))
                                                                                                                                  ((a x x b c) a x x b c))
                              ("special case: don't inline stuff if it ends up out of order" (tostring (prn "a" "b" "c"))
                                                                                             "abc\n")
                              ("special case: can't inline pop-me here" (let mylist '(a b c d)
                                                                          ((fn (pop-me)
                                                                             (assign mylist (cdr mylist))
                                                                             pop-me)
                                                                           (car mylist)))
                                                                        a)
                              ("special case: confused nesting" (let it 'foo
                                                                  (with (foobar (string it 'bar)
                                                                         foobaz (cons it 'baz))
                                                                    (cons foobar foobaz)))
                                                                ("foobar" foo . baz)))))

