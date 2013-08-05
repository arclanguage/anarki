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

(run-all-tests)
