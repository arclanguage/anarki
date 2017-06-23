(suite atom
       (test includes-int (assert-t (atom 3)))
       (test includes-float
             (assert-t (atom 3.14159)))
       (test includes-exact (assert-t (atom 3/16)))
       (test includes-symbol (assert-t (atom 'a)))
       (test includes-char (assert-t (atom #\a)))
       (test includes-string
             (assert-t (atom "hello")))
       (test includes-nil (assert-t (atom nil)))
       (test excludes-list
             (assert-nil (atom '(1 2 3))))
       (test excludes-table
             (assert-nil (atom (obj a 1 b 2))))
       (test excludes-tagged-types
             (assert-nil (atom (annotate 'foo 34)))))

(suite memtable
       (test no-args
             (assert-same (obj) (memtable)))
       (test only-one-val-in-list
             (assert-same (obj 1 t) (memtable (list 1))))
       (test two-vals-in-list
             (assert-same (obj 1 t 2 t)
                          (memtable (list 1 2))))
       (test repeated-val-in-list
             (assert-same (obj 1 t 2 t)
                          (memtable (list 1 2 1))))
       (test default-value-is-respected
             (assert-same (obj 1 'not-the-default)
                          (memtable (list 1) 'not-the-default))))

(suite do
       (test sequences
             (assert-same 3 (ret x 1 (do ++.x ++.x))))
       (test returns-final-form
             (assert-same 34 (let x 1 (do ++.x ++.x 34)))))

(suite for
       (test iterates
             (assert-same 6
                          (ret result 0
                            (for i 1 (<= i 3) ++.i
                              (= result (+ result i))))))
       (test multiple-vars
             (assert-same 6
                          (ret result 0
                            (for (i j) '(1 2) (<= j 4) (do ++.i ++.j)
                              (= result (+ result i))))))
       (test returns-nil
             (assert-nil (for i 1 (<= i 3) ++.i 1))))

(suite ssyntax
       (test ssyntax?
             (assert-nil ($.ssyntax? 'car)))
       (test infix
             (assert-t ($.ssyntax? 'car.body)))
       (test expand-ssyntax-infix
             (assert-same '(car body)
                          ($.expand-ssyntax 'car.body)))
       (test ssyntax?-!-prefix
             (assert-t ($.ssyntax? '!a)))
       (test expand-ssyntax-!-prefix
             (assert-same '(get 'a)
                          ($.expand-ssyntax '!a)))
       (test ssyntax?-!-infix
             (assert-t ($.ssyntax? 'car!body)))
       (test expand-ssyntax-!-infix
             (assert-same '(car 'body)
                          ($.expand-ssyntax 'car!body)))
       (test ssyntax?-:-infix
             (assert-t ($.ssyntax? 'f:g)))
       (test expand-ssyntax-:-infix
             (assert-same '(compose f g)
                          ($.expand-ssyntax 'f:g)))
       (test ssyntax?-~-prefix
             (assert-t ($.ssyntax? '~f)))
       (test expand-ssyntax-~-prefix
             (assert-same '(complement f)
                          ($.expand-ssyntax '~f)))
       (test ssyntax?-&-infix
             (assert-t ($.ssyntax? 'f&g)))
       (test expand-ssyntax-&-infix
             (assert-same '(andf f g)
                          ($.expand-ssyntax 'f&g))))

(suite break-continue
       (test break
             (assert-same '(1 x 2 x 3)
                          (accum acc
                            (up i 1 6
                              (acc i)
                              (if (> i 2) (break))
                              (acc 'x)))))
       (test continue
             (assert-same '(1 x 2 x 3 4 5)
                          (accum acc
                            (up i 1 6
                              (acc i)
                              (if (> i 2) (continue))
                              (acc 'x))))))

(suite isa
       (test checks-type (isa "abc" 'string))
       (test checks-type-predicate
             (do (assert-nil (isa 1 'positive-num))
                 (def-isa positive-num
                   (and (or (isa _ 'num) (isa _ 'int)) (> _ 0)))
                 (assert 
                   (isa 1 'positive-num)
                   "isa supports predicate types")
                 (wipe (type-predicates* 'positive-num))
                 (assert-nil (isa 1 'positive-num)
                             "isa test suite failed to cleanup"))))

(suite coerce
       (test nil-to-cons
             (assert-nil (as cons nil))))

(suite copy
       (setup old-list    '(1 2 3)
              list-copy   (copy old-list)
              old-string  "abc"
              string-copy (copy old-string)
              old-table   (obj a 1 b 2)
              table-copy  (copy old-table))
       (test list-copies-are-same
             (assert-same old-list list-copy))
       (test copy-list-returns-new-list
             (assert-nil (is old-list list-copy)))
       (test string-copies-are-same
             (assert-same old-string string-copy))
       (test copy-string-returns-new-string
             (assert-nil ($.eq? old-string string-copy)))
       (test table-copies-are-same
             (assert-same old-table table-copy))
       (test copy-table-returns-new-table
             (assert-nil (is old-table table-copy))))

(suite len
       (test lists (assert-same 3 (len '(1 2 3))))
       (test symbols (assert-same 0 (len 'a))))

(suite find
       (test list-element-exists
             (assert-same #\b (find #\b '(#\a #\b #\c))))
       (test list-element-doesnt-exist
             (assert-nil (find #\d '(#\a #\b #\c))))
       (test arbitrary-predicate-finds-single-element
             (assert-same 34 (find even '(34))))
       (test arbitrary-predicate-no-match-single-element
             (assert-nil (find even '(35))))
       (test arbitrary-predicate-finds-in-car
             (assert-same 34 (find even '(34 35))))
       (test arbitrary-predicate-finds-in-cdr
             (assert-same 34 (find even '(33 34))))
       (test arbitrary-predicate-finds-in-middle-of-list
             (assert-same 34 (find even '(33 34 35))))
       (test returns-first-match
             (assert-same 34 (find even '(34 35 36))))
       (test string-element-exists
             (assert-same #\b (find #\b "abc")))
       (test string-element-doesnt-exist
             (assert-nil (find #\d "abc")))
       (test improper-list-exists
             (assert-same 'a (find 'a '(a b . c))))
       (test improper-list-exists-in-last-position
             (assert-same 'c (find 'c '(a b . c))))
       (test improper-list-element-doesnt-exist
             (assert-nil (find 'd '(a b . c)))))

(suite mem
       (test element-exists
             (assert-same '(6 7) (mem 6 '(2 4 5 6 7))))
       (test element-doesnt-exist
             (assert-nil (mem 6 '(2 4 5 7))))
       (test improper-list-element-in-car
             (assert-same '(6 . 7)
                          (mem 6 '(2 4 5 6 . 7))))
       (test improper-list-element-in-cdr
             (assert-same 6 (mem 6 '(2 4 5 . 6)))))

(suite some
       (test improper-list-element-in-car
             (assert-t (some odd '(2 4 5 . 6))))
       (test improper-list-element-in-cdr
             (assert-t (some 6 '(2 4 5 . 6))))
       (test improper-list-element-doesnt-exist
             (assert-nil (some 7 '(2 4 5 . 6)))))

(suite pushnew
       (test improper-list-new-element
             (assert-same '(2 . 3)
                          (ret x 3 (pushnew 2 x))))
       (test improper-list-element-already-exists
             (assert-same 3 (ret x 3 (pushnew 3 x))))
       (test nil-can-be-pushed
             (assert-same '(nil 3)
                          (ret x '(3) (pushnew nil x)))))

(suite map
       (test one-list
             (assert-same '(2 4 6)
                          (map [* _ 2] '(1 2 3))))
       (test multiple-lists
             (assert-same '(1 4 9)
                          (map * '(1 2 3) '(1 2 3))))
       (test one-string
             (assert-same "mno"
                          (map (obj #\a #\m #\b #\n #\c #\o) "abc")))
       (test multiple-strings
             (assert-same "dahe"
                          (map (fn (a b) (min a b)) "dave" "john"))))

(suite subst
       (test lists
             (assert-same '(2 2 3) (subst 1 2 '(1 2 3))))
       (test old-arg-can-be-a-function
             (assert-same '(2 2 2 (4 2 . 6) . 2)
                          (rep:subst atom&odd
                                     2
                                     (tree '(1 2 3 (4 5 . 6) . 7)))))
       (test new-arg-can-be-a-function
             (assert-same '(2 2 4 (4 6 . 6) . 8)
                          (rep:subst atom&odd
                                     [+ _ 1]
                                     (tree '(1 2 3 (4 5 . 6) . 7)))))
       (test can-replace-subtrees
             (assert-same '((3 4) (5 6))
                          (rep:subst '(1 2)
                                     '(3 4)
                                     (tree '((1 2) (5 6)))))))

(suite cut
       (test finds-element-in-list
             (assert-same '(3 4 5) (cut '(1 2 3 4 5) 2)))
       (test respects-end-index-in-list
             (assert-same '(3 4) (cut '(1 2 3 4 5) 2 4)))
       (test end-index-at-end-of-list-works
             (assert-same '(3 4 5)
                          (cut '(1 2 3 4 5) 2 5)))
       (test end-index-beyond-end-of-list-works
             (assert-same '(3 4 5)
                          (cut '(1 2 3 4 5) 2 6)))
       (test negative-end-index-in-list-is-ok
             (assert-same '(3 4) (cut '(1 2 3 4 5) 2 -1)))
       (test finds-element-in-string
             (assert-same "cde" (cut "abcde" 2)))
       (test respects-end-index-in-string
             (assert-same "cd" (cut "abcde" 2 4)))
       (test end-index-at-end-of-string-works
             (assert-same "cde" (cut "abcde" 2 5)))
       (test end-index-beyond-end-of-string-works
             (assert-same "cde" (cut "abcde" 2 6)))
       (test negative-end-index-in-string-is-ok
             (assert-same "cd" (cut "abcde" 2 -1))))

(suite split
       (test can-split-lists
             (assert-same '((1 2) (3 4))
                          (split '(1 2 3 4) 2)))
       (test can-split-strings
             (assert-same '((1 2) (3 4))
                          (split '(1 2 3 4) 2))))

(suite before
       (test returns-t-when-first-is-before
             (assert-t (before 3 4 '(1 2 3 4))))
       (test respects-starting-index
             (assert-nil (before 3 4 '(1 2 3 4 3) 3)))
       (test returns-nil-when-first-isnt-before
             (assert-nil (before 4 3 '(1 2 3 4))))
       (test returns-t-when-second-is-absent
             (assert-t (before 3 5 '(1 2 3 4))))
       (test returns-nil-when-first-is-absent
             (assert-nil (before 5 3 '(1 2 3 4))))
       (test returns-nil-when-both-are-absent
             (assert-nil (before 6 5 '(1 2 3 4)))))

(suite serialize
       (test nil (assert-nil (serialize ())))
       (test lists
             (assert-same '(1 2 3) (serialize '(1 2 3))))
       (test strings
             (assert-same "abc" (serialize "abc")))
       (test tables
             (assert-same '(tagged table ((3 4) (1 2)))
                          (serialize (obj 1 2 3 4))))
       (test tables-inside-lists
             (assert-same '(1 (tagged table ()) 2 3)
                          (serialize `(1 ,(table) 2 3))))
       (test nested-tables
             (assert-same '(tagged table ((2 3) (1 (tagged table ()))))
                          (serialize (obj 1 (table) 2 3)))))

(suite deserialize
       (test nil
             (assert-nil (unserialize:serialize ())))
       (test lists
             (assert-same '(1 2 3)
                          (unserialize:serialize '(1 2 3))))
       (test quoted-lists-suppress-unserialize
             (assert-same ''(tagged table ((1 2) (3 4)))
                          (unserialize ''(tagged table ((1 2) (3 4))))))
       (test strings
             (assert-same "abc"
                          (unserialize:serialize "abc")))
       (test empty-tables
             (assert-same (table)
                          (unserialize:serialize (table))))
       (test full-tables
             (assert-same (obj 1 2 3 4)
                          (unserialize:serialize (obj 1 2 3 4))))
       (test tables-inside-lists
             (assert-same `(1 ,(table) 2 3)
                          (unserialize:serialize `(1 ,(table) 2 3))))
       (test nested-tables
             (assert-same (obj 1 (table) 2 3)
                          (unserialize:serialize (obj 1 (table) 2 3)))))

(mac foo-good (x)
  `(let y@ (+ ,x 1) (+ y@ ,x)))

(mac foo-bad (x) `(let y (+ ,x 1) (+ y ,x)))

(mac foo-ssyntax ()
  `(let y@ (list 1 2 3) y@.1))

(suite gensyms
       (test gensyms-dont-capture-variables
             (assert-same 7 (let y@ 3 (foo-good y@))))
       (test no-gensyms-capture-variables
             (assert-same 8 (let y 3 (foo-bad y))))
       (test gensyms-mix-with-ssyntax
             (assert-same 2 (foo-ssyntax))))

(suite sort
       (test doesnt-alter-elements-of-list
             (let x '((1) (2) (3))
               (assert 
                 (is car.x (car:sort (compare < car) x))
                 "elements should be untouched after sort"))))

(suite vector
       (test get-set
             (assert-same '(0 12)
                          (let x vec.20 (= x.4 12) (list x.3 x.4)))))

(suite readline
       (test stops-at-newlines
             (assert-same '("a" "b" "c")
                          (fromstring "a\nb\nc\n" (drain (readline)))))
       (test stops-at-eof
             (assert-same '("a" "b" "c")
                          (fromstring "a\nb\nc" (drain (readline)))))
       (test handles-empty-lines
             (assert-same '("" "" "a" "c" "" "d")
                          (fromstring "\n\na\nc\n\nd"
                            (drain (readline)))))
       (test handles-cr-lf
             (assert-same '("" "" "a" "c" "" "d")
                          (fromstring "\r\n\r\na\r\nc\r\n\r\nd"
                            (drain (readline)))))
       (test returns-eof
             (assert-nil (fromstring "" (readline)))))

(suite at-string
       (test interpolates
             (assert-same '"abc foo"
                          (let x 'foo "abc @x")))
       (test escapes-at
             (assert-same '"abc @x"
                          (let x 'foo "abc @@x")))
       (test interpolate-with-escape
             (assert-same '"abc foo @x"
                          (let x 'foo "abc @x @@x"))))

