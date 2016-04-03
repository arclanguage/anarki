(suite lists
       (suite cons
              create (assert-same '(a b c)
                                  (cons 'a '(b c)))
              cons-strings (assert-same '("a" . "b")
                                        (cons "a" "b")))
       (suite car
              car-of-nil-is-nil (assert-nil (car nil))
              car-of-empty-list-is-nil (assert-nil (car '()))
              empty-list-can-be-not-quoted (assert-nil (car ()))
              car-returns-car (assert-same 'foo
                                           (car '(foo 12.34 "bar"))))
       (suite cdr
              cdr-of-nil-is-nil (assert-nil (cdr nil))
              cdr-of-empty-list-is-nil (assert-nil (cdr '()))
              cdr-returns-cdr (assert-same '(12.34 "bar")
                                           (cdr '(foo 12.34 "bar"))))
       (suite scar
              on-lists (assert-same '(99 2 3)
                                    (ret lst '(1 2 3)
                                         (scar lst 99)))
              on-strings (assert-same "boo"
                                      (ret str "foo"
                                           (scar str #\b))))
       (suite scdr
              on-lists (assert-same '(a y z)
                                    (ret lst '(a b c)
                                         (scdr lst '(y z))))
              on-nested-list (assert-same '(a b c x y z)
                                          (ret lst '(a b c)
                                               (scdr (cdr (cdr lst))
                                                     '(x y z)))))

       size (assert-same 7
                         (len '(a b c d e f g)))
       set-element (assert-same '(x b c)
                                (ret lst '(a b c)
                                     (sref lst 'x 0)))

       (suite get-element
              index-exists (assert-same 'c
                                        ('(a b c d) 2))
              index-out-of-bounds (assert-nil ('(a b) 4))))
