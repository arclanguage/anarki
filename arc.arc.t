(test-iso "find works on lists"
  #\b
  (find #\b '(#\a #\b #\c)))

(test-iso "find works on lists - 2"
  nil
  (find #\d '(#\a #\b #\c)))

(test-iso "find handles arbitrary predicate"
  34
  (find even '(33 34 35)))

(test-iso "find handles arbitrary predicate - 2"
  34
  (find even '(34 35)))

(test-iso "find handles arbitrary predicate - 3"
  34
  (find even '(33 34)))

(test-iso "find handles arbitrary predicate - 4"
  34
  (find even '(34)))

(test-iso "find handles arbitrary predicate - 5"
  nil
  (find even '(35)))

(test-iso "find returns first match"
  34
  (find even '(34 35 36)))

(test-iso "find works on strings"
  #\b
  (find #\b "abc"))

(test-iso "find works on strings - 2"
  nil
  (find #\d "abc"))

(test-iso "find works on improper lists"
  'a
  (find 'a '(a b . c)))

(test-iso "find works on improper lists - 2"
  'c
  (find 'c '(a b . c)))

(test-iso "find works on improper lists - 3"
  nil
  (find 'd '(a b . c)))

(test-iso "mem works"
  '(6 7)
  (mem 6 '(2 4 5 6 7)))

(test-iso "mem works on improper lists"
  '(6 . 7)
  (mem 6 '(2 4 5 6 . 7)))

(test-iso "mem works on improper lists - 2"
  6
  (mem 6 '(2 4 5 . 6)))

(test-iso "some works on improper lists"
  t
  (some odd '(2 4 5 . 6)))

(test-iso "some works on improper lists - 2"
  t
  (some 6 '(2 4 5 . 6)))

(test-iso "some works on improper lists - 3"
  nil
  (some 7 '(2 4 5 . 6)))

(test-iso "pushnew works on improper lists"
  '(2 . 3)
  (ret x 3
    (pushnew 2 x)))

(test-iso "pushnew works on improper lists - 2"
  3
  (ret x 3
    (pushnew 3 x)))

(test-iso "pushnew works with nil"
  '(nil 3)
  (ret x '(3)
    (pushnew nil x)))

(mac foo (x) `(let y@ (+ ,x 1) (+ y@ ,x)))
(mac foo-bad (x) `(let y (+ ,x 1) (+ y ,x)))
(test-iso "mac gensyms don't capture variables"
  7
  (let y@ 3 (foo y@)))
(test-iso "mac without gensyms does capture variables"
  8  ; probably not what you want
  (let y 3 (foo-bad y)))

(test-iso "tree-subst can take functions"
  '(2 2 2 (4 2 . 6) . 2)
  (tree-subst atom&odd 2 '(1 2 3 (4 5 . 6) . 7)))

(test-iso "tree-subst can take functions - 2"
  '(2 2 4 (4 6 . 6) . 8)
  (tree-subst atom&odd [+ _ 1] '(1 2 3 (4 5 . 6) . 7)))
