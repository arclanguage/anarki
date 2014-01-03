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

(run-all-tests)
