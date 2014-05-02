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

(suite for-goto
       break (assert-same '(1 x 2 x 3)
                          (accum acc
                                 (up i 1 6
                                     (acc i)
                                     (if (> i 2) (break))
                                     (acc 'x))))

       continue (assert-same '(1 x 2 x 3 4 5)
                             (accum acc
                                    (up i 1 6
                                        (acc i)
                                        (if (> i 2) (continue))
                                        (acc 'x)))))

(suite coerce
       nil-to-cons (assert-nil (as cons nil)))

(suite-w/setup copy
               (old-list '(1 2 3)
                list-copy (copy old-list)
                old-string "abc"
                string-copy (copy old-string)
                old-table (obj a 1 b 2)
                table-copy (copy old-table))
               list-copies-are-same (assert-same old-list
                                                 list-copy)
               copy-list-returns-new-list (assert-nil (is old-list
                                                          list-copy))
               string-copies-are-same (assert-same old-string
                                                   string-copy)
               copy-string-returns-new-string (assert-nil ($.eq? old-string
                                                                 string-copy)) ; ugly that string copies are 'is' each other
               table-copies-are-same (assert-same old-table
                                                  table-copy)
               copy-table-returns-new-table (assert-nil (is old-table
                                                            table-copy)))

(suite len
       lists (assert-same 3
                          (len '(1 2 3)))
       improper-lists (assert-same 3
                                   (len '(1 2 . 3)))
       symbols (assert-same 0
                            (len 'a)))
