(suite memtable
       no-args (assert-same (obj)
                            (memtable))
       only-one-val-in-list (assert-same (obj 1 t)
                                         (memtable (list 1)))
       two-vals-in-list (assert-same (obj 1 t 2 t)
                                     (memtable (list 1 2)))
       repeated-val-in-list (assert-same (obj 1 t 2 t)
                                         (memtable (list 1 2 1)))
       default-value-is-respected (assert-same (obj 1 'not-the-default)
                                               (memtable (list 1) 'not-the-default)))

(suite do
       sequences (assert-same 3
                              (ret x 1
                                   (do ++.x ++.x)))
       returns-final-form (assert-same 34
                                       (let x 1
                                            (do ++.x ++.x 34))))

(suite ssyntax
       ssyntax? (assert-nil ($.ssyntax? 'car))

       infix (assert-t ($.ssyntax? 'car.body))

       expand-ssyntax-infix (assert-same '(car body)
                                         ($.expand-ssyntax 'car.body))

       ssyntax?-!-prefix (assert-t ($.ssyntax? '!a))

       expand-ssyntax-!-prefix (assert-same '(get 'a)
                                            ($.expand-ssyntax '!a))

       ssyntax?-!-infix (assert-t ($.ssyntax? 'car!body))

       expand-ssyntax-!-infix (assert-same '(car 'body)
                                           ($.expand-ssyntax 'car!body))

       ssyntax?-:-infix (assert-t ($.ssyntax? 'f:g))

       expand-ssyntax-:-infix (assert-same '(compose f g)
                                           ($.expand-ssyntax 'f:g))

       ssyntax?-~-prefix (assert-t ($.ssyntax? '~f))

       expand-ssyntax-~-prefix (assert-same '(complement f)
                                            ($.expand-ssyntax '~f))

       ssyntax?-&-infix (assert-t ($.ssyntax? 'f&g))

       expand-ssyntax-&-infix (assert-same '(andf f g)
                                           ($.expand-ssyntax 'f&g)))
