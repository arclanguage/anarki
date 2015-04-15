(suite atom
       includes-int (assert-t (atom 3))
       includes-float (assert-t (atom 3.14159))
       includes-exact (assert-t (atom 3/16))
       includes-symbol (assert-t (atom 'a))
       includes-char (assert-t (atom #\a))
       includes-string (assert-t (atom "hello"))
       includes-nil (assert-t (atom nil))
       excludes-list (assert-nil (atom '(1 2 3)))
       excludes-table (assert-nil (atom (obj a 1 b 2)))
       excludes-tagged-types (assert-nil (atom (annotate 'foo 34))))

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

(suite for
       iterates (assert-same 6
                             (ret result 0
                               (for i 1 (<= i 3) ++.i
                                 (= result (+ result i)))))
       multiple-vars (assert-same 6
                                  (ret result 0
                                    (for (i j) '(1 2) (<= j 4) (do ++.i ++.j)
                                      (= result (+ result i)))))
       returns-nil (assert-nil (for i 1 (<= i 3) ++.i 1)))

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

(suite break-continue
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

(suite isa
       checks-type (isa "abc" 'string)
       checks-type-predicate (do (assert-nil (isa 1 'positive-num))
                                 (def-isa positive-num
                                   (and (or (isa _ 'num) (isa _ 'int))
                                        (> _ 0)))
                                 (assert (isa 1 'positive-num) "isa supports predicate types")
                                 ; cleanup
                                 (wipe (type-predicates* 'positive-num))
                                 (assert-nil (isa 1 'positive-num) "isa test suite failed to cleanup")))

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
(suite subst
       lists (assert-same '(2 2 3)
                          (subst 1 2 '(1 2 3)))
       old-arg-can-be-a-function (assert-same '(2 2 2 (4 2 . 6) . 2)
                                              (rep:subst atom&odd 2 (tree '(1 2 3 (4 5 . 6) . 7))))
       new-arg-can-be-a-function (assert-same '(2 2 4 (4 6 . 6) . 8)
                                              (rep:subst atom&odd [+ _ 1] (tree '(1 2 3 (4 5 . 6) . 7))))
       can-replace-subtrees (assert-same '((3 4) (5 6))
                                         (rep:subst '(1 2) '(3 4) (tree '((1 2) (5 6))))))
(suite cut
       finds-element-in-string (assert-same '(3 4 5)
                                            (cut '(1 2 3 4 5) 2))
       respects-end-index-in-list (assert-same '(3 4)
                                               (cut '(1 2 3 4 5) 2 4))
       end-index-at-end-of-list-works (assert-same '(3 4 5)
                                                   (cut '(1 2 3 4 5) 2 5))
       end-index-beyond-end-of-list-works (assert-same '(3 4 5)
                                                       (cut '(1 2 3 4 5) 2 6))
       negative-end-index-in-list-is-ok (assert-same '(3 4)
                                                     (cut '(1 2 3 4 5) 2 -1))
       finds-element-in-string (assert-same "cde"
                                            (cut "abcde" 2))
       respects-end-index-in-string (assert-same "cd"
                                                 (cut "abcde" 2 4))
       end-index-at-end-of-string-works (assert-same "cde"
                                                     (cut "abcde" 2 5))
       end-index-beyond-end-of-string-works (assert-same "cde"
                                                         (cut "abcde" 2 6))
       negative-end-index-in-string-is-ok (assert-same "cd"
                                                       (cut "abcde" 2 -1)))

(suite split
       can-split-lists (assert-same '((1 2) (3 4))
                                    (split '(1 2 3 4) 2))
       can-split-strings (assert-same  '((1 2) (3 4))
                                       (split '(1 2 3 4) 2)))

(suite before
       returns-t-when-first-is-before (assert-t (before 3 4 '(1 2 3 4)))
       respects-starting-index (assert-nil (before 3 4 '(1 2 3 4 3) 3))
       returns-nil-when-first-isnt-before (assert-nil (before 4 3 '(1 2 3 4)))
       returns-t-when-second-is-absent (assert-t (before 3 5 '(1 2 3 4)))
       returns-nil-when-first-is-absent (assert-nil (before 5 3 '(1 2 3 4)))
       returns-nil-when-both-are-absent (assert-nil (before 6 5 '(1 2 3 4))))

(suite serialize
       nil (assert-nil (serialize ()))
       lists (assert-same '(1 2 3)
                          (serialize '(1 2 3)))
       strings (assert-same "abc"
                            (serialize "abc"))
       tables (assert-same '(tagged table ((3 4) (1 2)))
                           (serialize (obj 1 2 3 4)))
       tables-inside-lists (assert-same '(1 (tagged table ()) 2 3)
                                        (serialize `(1 ,(table) 2 3)))
       nested-tables (assert-same '(tagged table ((2 3) (1 (tagged table ()))))
                                  (serialize (obj 1 (table) 2 3))))

(suite deserialize
       nil (assert-nil (unserialize:serialize ()))
       lists (assert-same '(1 2 3)
                          (unserialize:serialize '(1 2 3)))
       strings (assert-same "abc"
                            (unserialize:serialize "abc"))
       empty-tables (assert-same (table)
                                 (unserialize:serialize (table)))
       full-tables (assert-same (obj 1 2 3 4)
                                (unserialize:serialize (obj 1 2 3 4)))
       tables-inside-lists (assert-same `(1 ,(table) 2 3)
                                        (unserialize:serialize `(1 ,(table) 2 3)))
       nested-tables (assert-same (obj 1 (table) 2 3)
                                  (unserialize:serialize (obj 1 (table) 2 3))))

(mac foo-good (x) `(let y@ (+ ,x 1) (+ y@ ,x)))
(mac foo-bad (x) `(let y (+ ,x 1) (+ y ,x)))
(mac foo-ssyntax () `(let y@ (list 1 2 3) y@.1))

(suite gensyms
       gensyms-dont-capture-variables (assert-same 7
                                                   (let y@ 3 (foo-good y@)))
       no-gensyms-capture-variables (assert-same 8  ; probably not what you want
                                                 (let y 3 (foo-bad y)))
       gensyms-mix-with-ssyntax (assert-same 2 (foo-ssyntax)))

(suite sort
       doesnt-alter-elements-of-list (let x '((1) (2) (3))
                                       (assert (is car.x
                                                   (car:sort (compare < car) x))
                                               "elements should be untouched after sort")))

(suite vector
       ; creation, write, read, and value of uninitialized location
       get-set (assert-same '(0 12)
                 (let x vec.20
                   (= x.4 12)
                   (list x.3 x.4))))

(suite readline
       stops-at-newlines  (assert-same '("a" "b" "c")
                                       (fromstring "a\nb\nc\n"
                                         (drain (readline))))
       stops-at-eof  (assert-same '("a" "b" "c")
                                  (fromstring "a\nb\nc"
                                    (drain (readline))))
       handles-empty-lines  (assert-same '("" "" "a" "c" "" "d")
                                         (fromstring "\n\na\nc\n\nd"
                                           (drain (readline)))))
