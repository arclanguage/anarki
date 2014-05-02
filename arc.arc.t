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

(suite find
       list-element-exists (assert-same #\b
                                        (find #\b '(#\a #\b #\c)))
       list-element-doesnt-exist (assert-nil (find #\d '(#\a #\b #\c)))
       arbitrary-predicate-finds-single-element (assert-same 34
                                                             (find even '(34)))
       arbitrary-predicate-no-match-single-element (assert-nil (find even '(35)))
       arbitrary-predicate-finds-in-car (assert-same 34
                                                     (find even '(34 35)))
       arbitrary-predicate-finds-in-cdr (assert-same 34
                                                     (find even '(33 34)))
       arbitrary-predicate-finds-in-middle-of-list (assert-same 34
                                                                (find even '(33 34 35)))
       returns-first-match (assert-same 34
                                        (find even '(34 35 36)))
       string-element-exists (assert-same #\b
                                          (find #\b "abc"))
       string-element-doesnt-exist (assert-nil (find #\d "abc"))
       improper-list-exists (assert-same 'a
                                         (find 'a '(a b . c)))
       improper-list-exists-in-last-position (assert-same 'c
                                                          (find 'c '(a b . c)))
       improper-list-element-doesnt-exist (assert-nil (find 'd '(a b . c))))

(suite mem
       element-exists (assert-same '(6 7)
                                   (mem 6 '(2 4 5 6 7)))
       element-doesnt-exist (assert-nil (mem 6 '(2 4 5 7)))
       improper-list-element-in-car (assert-same '(6 . 7)
                                         (mem 6 '(2 4 5 6 . 7)))
       improper-list-element-in-cdr (assert-same 6
                                                 (mem 6 '(2 4 5 . 6))))

(suite some
       improper-list-element-in-car (assert-t (some odd '(2 4 5 . 6)))
       improper-list-element-in-cdr (assert-t (some 6 '(2 4 5 . 6)))
       improper-list-element-doesnt-exist (assert-nil (some 7 '(2 4 5 . 6))))

(suite pushnew
       improper-list-new-element (assert-same '(2 . 3)
                                              (ret x 3
                                                   (pushnew 2 x)))
       improper-list-element-already-exists (assert-same 3
                                                         (ret x 3
                                                              (pushnew 3 x)))
       nil-can-be-pushed (assert-same '(nil 3)
                                      (ret x '(3)
                                           (pushnew nil x))))

(suite map
       one-list (assert-same '(2 4 6)
                             (map [* _ 2] '(1 2 3)))
       multiple-lists (assert-same '(1 4 9)
                                   (map * '(1 2 3) '(1 2 3)))
       one-string (assert-same "mno"
                               (map (obj #\a #\m #\b #\n #\c #\o)
                                    "abc"))
       multiple-strings (assert-same "dahe"
                                     (map (fn (a b) (min a b))
                                          "dave" "john")))
