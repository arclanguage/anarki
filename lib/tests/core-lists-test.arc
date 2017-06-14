(suite lists
       (suite cons
              (test create
                    (assert-same '(a b c) (cons 'a '(b c))))
              (test cons-strings
                    (assert-same '("a" . "b") (cons "a" "b"))))
       (suite car
              (test car-of-nil-is-nil
                    (assert-nil (car nil)))
              (test car-of-empty-list-is-nil
                    (assert-nil (car '())))
              (test empty-list-can-be-not-quoted
                    (assert-nil (car ())))
              (test car-returns-car
                    (assert-same 'foo (car '(foo 12.34 "bar")))))
       (suite cdr
              (test cdr-of-nil-is-nil
                    (assert-nil (cdr nil)))
              (test cdr-of-empty-list-is-nil
                    (assert-nil (cdr '())))
              (test cdr-returns-cdr
                    (assert-same '(12.34 "bar")
                                 (cdr '(foo 12.34 "bar")))))
       (suite scar
              (test on-lists
                    (assert-same '(99 2 3)
                                 (ret lst '(1 2 3) (scar lst 99))))
              (test on-strings
                    (assert-same "boo"
                                 (ret str "foo" (scar str #\b)))))
       (suite scdr
              (test on-lists
                    (assert-same '(a y z)
                                 (ret lst '(a b c) (scdr lst '(y z)))))
              (test on-nested-list
                    (assert-same '(a b c x y z)
                                 (ret lst '(a b c)
                                   (scdr (cdr (cdr lst)) '(x y z))))))
       (test size
             (assert-same 7 (len '(a b c d e f g))))
       (suite set-element
              (test index-exists
                    (assert-same '(x b c)
                                 (ret lst '(a b c) (sref lst 'x 0))))
              (test index-negative
                    (assert-same '(a b x)
                                 (ret lst '(a b c) (sref lst 'x -1)))))
       (suite get-element
              (test index-exists
                    (assert-same 'c ('(a b c d) 2)))
              (test index-out-of-bounds
                    (assert-nil ('(a b) 4)))
              (test index-negative
                    (assert-same 'd ('(a b c d) -1)))
              (test index-very-negative
                    (assert-nil ('(a b c d) -100)))))

