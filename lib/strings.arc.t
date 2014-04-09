(require "lib/unit-test.arc")

(register-test
  '(suite "urlencode"
     ("passes alphanumerics through"
       (urlencode "abc")
       "abc")
     ("escapes spaces"
       (urlencode "a b")
       "a%20b")
     ("passes through other punctuation"
       (urlencode "a@@b#%^def")
       "a@b#%25^def")
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
       nil)))

(run-all-tests)
