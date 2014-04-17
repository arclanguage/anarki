(require "lib/unit-test.arc")

(register-test
  '(suite "lines"
     ("empty string"
       (lines "")
       (""))
     ("empty lines"
       (lines "\n")
       ("" ""))
     ("single line without newline"
       (lines "a b")
       ("a b"))
     ("trailing newline"
       (lines "a b\n")
       ("a b" ""))
     ("skip returns"
       (lines "a b\r\n")
       ("a b" ""))
     ("skip returns without newline"
       (lines "a b\r")
       ("a b"))
     ("all together"
       (lines "a b\nc d\n\ne f")
       ("a b" "c d" "" "e f"))))

(register-test
  '(suite "urlencode"
     ("passes alphanumerics through"
       (urlencode "abc")
       "abc")
     ("escapes spaces"
       (urlencode "a b")
       "a%20b")
     ("escapes other punctuation"
       (urlencode "a@@b#%^def")
       "a%40b%23%25%5edef")
     ("escapes unicode"
       (urlencode "abcüd")
       "abc%c3%bcd")
     ("escapes url params"
       (urlencode "item?id=1")
       "item%3fid%3d1")
))

(register-test
  '(suite "subst"
     ("substitutes one occurrence of single char"
       (subst "a" "m" "abc")
       "mbc")
     ("substitutes all occurrences of single char"
       (subst "a" "m" "abcabd")
       "mbcmbd")
     ("substitutes all occurrences of multi-char pattern"
       (subst "ab" "m" "abcabd")
       "mcmd")))

(register-test
  '(suite "posmatch"
     ("handles equal character"
       (posmatch "a" "a")
       0)
     ("handles inequal character"
       (posmatch "a" "b")
       nil)
     ("handles single-character patterns at start"
       (posmatch "a" "abc")
       0)
     ("handles single-character pattern in middle"
       (posmatch "b" "abc")
       1)
     ("handles single-character pattern at end"
       (posmatch "c" "abc")
       2)
     ("handles missing single-character pattern"
       (posmatch "d" "abc")
       nil)
     ("handles patterns at start"
       (posmatch "ab" "abcd")
       0)
     ("handles pattern in middle"
       (posmatch "bc" "abcd")
       1)
     ("handles pattern at end"
       (posmatch "cd" "abcd")
       2)
     ("handles missing pattern"
       (posmatch "de" "abcd")
       nil)
     ("can take an optional start index"
       (posmatch "a" "banana" 2)
       3)
     ("can take a list of chars instead of a substring"
       (posmatch '(#\a #\b) '(#\c #\a #\b))
       1)
     ("can take a predicate instead of a substring"
       (posmatch (fn (c) (in c #\a #\b)) "foobar")
       3)))

(register-test
  '(suite "tokens"
     ("splits at whitespace"
        (tokens "abc def I'm too lazy")
        ("abc" "def" "I'm" "too" "lazy"))
     ("splits at other char"
        (tokens "banana" #\a)
        ("b" "n" "n"))))

(register-test
  '(suite "halve"
     ("splits at first whitespace without dropping it"
        (halve "ab cd ef")
        ("ab" " cd ef"))
     ("handles single word"
        (halve "abc")
        ("abc"))))

(register-test
  '(suite "positions"
     ("1"
       (positions #\space "abc def I'm too lazy")
       (3 7 11 15))
     ("2"
       (positions #\a "That abacus")
       (2 5 7))
     ("with predicate"
       (positions odd '(1 2 4 5 7))
       (0 3 4))))

(register-test
  '(suite "nonascii"
     ("fails on all ascii"
       (nonascii "Abc")
       nil)
     ("passes on some unicode"
       (nonascii "bcΓ")
       t)))

(register-test
  '(suite "litmatch"
     ("matches at head"
       (litmatch "abc" "abcde")
       t)
     ("fails elsewhere"
       (litmatch "abc" "xabcde")
       nil)
     ("passes at explicitly provided index"
       (litmatch "abc" "xabcde" 1)
       t)
     ("works with literal unquoted lists of chars"
       (litmatch (#\a #\b #\c) "abcde")
       t)))

(register-test
  '(suite "headmatch works like litmatch but with non-literal patterns as well"
     ("matches at head"
       (headmatch "abc" "abcde")
       t)
     ("fails elsewhere"
       (headmatch "abc" "xabcde")
       nil)
     ("passes at explicitly provided index"
       (headmatch "abc" "xabcde" 1)
       t)
     ("works with lists of chars"
       (headmatch '(#\a #\b #\c) "abcde")
       t)
     ("matches lists against lists"
       (headmatch '(#\a #\b) '(#\a #\b #\c))
       t)))

(register-test
  '(suite "endmatch"
     ("passes at end"
       (endmatch "cde" "abcde")
       t)
     ("fails elsewhere"
       (endmatch "abc" "abcde")
       nil)
     ("works with lists of chars"
       (endmatch (#\c #\d #\e) "abcde")
       t)))

(register-test
  '(suite "subst"
     ("substitutes all found patterns"
       (subst "foo" "bar" "catfood dogfood")
       "catbard dogbard")
     ("can substitute any value with a printed representation"
       (subst "a" '(1 2) "banana")
       "b(1 2)n(1 2)n(1 2)")))

(register-test
  '(suite "multisubst"
     ("substitutes multiple patterns at once"
       (multisubst '(("a" 1) ("b" "B")) "banana")
       "B1n1n1")))

(register-test
  '(suite "trim"
     ("drops whitespace from end"
       (trim " abc " 'end)
       " abc")
     ("drops whitespace from start and end"
       (trim " abc " 'both)
       "abc")
     ("can drop arbitrary characters"
       (trim "aabcaa" 'both #\a)
       "bc")
     ("can drop based on a predicate"
       (trim "aabcaa" 'both (fn (_) (in _ #\a #\b)))
       "c")))

(register-test
  '(suite "num"
     ("converts numbers to strings"
       (num 123)
       "123")
     ("inserts a comma every three digits"
       (num 123456)
       "123,456")
     ("handles negative numbers"
       (num -123456)
       "-123,456")
     ("can take an optional precision"
       (num 1.2345 2)
       "1.23")
     ("can pad zeroes to the right"
       (num 1.2 4 t)
       "1.2000")
     ("can add a leading zero"
       (num 0.3 4 t t)
       "0.3000")))

(register-test
  '(suite "pluralize"
     ("appends an 's' if number > 1"
       (pluralize 2 "fox")
       "foxs")
     ("can take an optional plural form"
       (pluralize 2 "fox" "foxes")
       "foxes")))

(register-test
  '(suite "plural"
     ("prints number and appends an 's' if number > 1"
       (plural 2 "fox")
       "2 foxs")))

(run-all-tests)
