(test "ssyntax?"
  (not ($.ssyntax? 'car)))

(test "ssyntax? . infix"
  ($.ssyntax? 'car.body))

(test-iso "expand-ssyntax . infix"
  '(car body)
  ($.expand-ssyntax 'car.body))

(test "ssyntax? ! prefix"
  ($.ssyntax? '!a))

(test-iso "expand-ssyntax ! prefix"
  '(get 'a)
  ($.expand-ssyntax '!a))

(test "ssyntax? ! infix"
  ($.ssyntax? 'car!body))

(test-iso "expand-ssyntax ! infix"
  '(car 'body)
  ($.expand-ssyntax 'car!body))

(test "ssyntax? : infix"
  ($.ssyntax? 'f:g))

(test-iso "expand-ssyntax : infix"
  '(compose f g)
  ($.expand-ssyntax 'f:g))

(test "ssyntax? ~ prefix"
  ($.ssyntax? '~f))

(test-iso "expand-ssyntax ~ prefix"
  '(complement f)
  ($.expand-ssyntax '~f))

(test "ssyntax? & infix"
  ($.ssyntax? 'f&g))

(test-iso "expand-ssyntax & infix"
  '(andf f g)
  ($.expand-ssyntax 'f&g))


(test "copy works on lists"
  (withs (old  '(1 2 3)
          new  copy.old)
    (and (iso old new)
         (~is old new))))

(test "copy works on strings"
  (withs (old  "abc"
          new  copy.old)
    (and (iso old new)
         (not ($.eq? old new)))))  ; ugly that string copies are 'is' each other

(test "copy works on tables"
  (withs (old  (obj a 1 b 2)
          new  copy.old)
    (and (iso old new)
         (~is old new))))

(test-iso "len works on lists"
  3
  (len '(1 2 3)))

(test-iso "len of syms is 0"
  0
  (len 'a))

(test-iso "len works on improper lists"
  3
  (len '(1 2 . 3)))

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

(test-iso "map works with one list"
  '(2 4 6)
  (map [* _ 2] '(1 2 3)))

(test-iso "map works with multiple lists"
  '(1 4 9)
  (map (fn(a b) (* a b))  '(1 2 3) '(1 2 3)))

(test-iso "map works with one string"
  "mno"
  (map (obj #\a #\m #\b #\n #\c #\o)
       "abc"))

(test-iso "map works with multiple strings"
  "dahe"
  (map (fn (a b) (min a b))
       "dave" "john"))

(test-iso "subst works on lists"
  '(2 2 3)
  (subst 1 2 '(1 2 3)))

(test-iso "subst can take functions"
  '(2 2 2 (4 2 . 6) . 2)
  (rep:subst atom&odd 2 (tree '(1 2 3 (4 5 . 6) . 7))))

(test-iso "subst can take functions - 2"
  '(2 2 4 (4 6 . 6) . 8)
  (rep:subst atom&odd [+ _ 1] (tree '(1 2 3 (4 5 . 6) . 7))))

(test-iso "subst can replace subtrees"
  '((3 4) (5 6))
  (rep:subst '(1 2) '(3 4) (tree '((1 2) (5 6)))))

(test-iso "serialize works on tables"
  '(tagged table ((b 2) (a 1)))
  (serialize (obj a 1 b 2)))

(let h (obj a 1 b 2)
  (test-iso "unserialize undoes serialize for tables"
    h
    (unserialize:serialize h)))

(test-iso "unserialize undoes serialize for empty table"
  (table)
  (unserialize:serialize (table)))

(mac foo (x) `(let y@ (+ ,x 1) (+ y@ ,x)))
(mac foo-bad (x) `(let y (+ ,x 1) (+ y ,x)))
(test-iso "mac gensyms don't capture variables"
  7
  (let y@ 3 (foo y@)))
(test-iso "mac without gensyms does capture variables"
  8  ; probably not what you want
  (let y 3 (foo-bad y)))
