(load "lib/boyer-moore.arc")
(suite boyer-moore
  (suite scan-past
       (test found
             (assert-same '(97 98 99)
                          (scan-past "de" (instring "abcde"))))
       )
  (suite rev-mismatch
       (test found
             (assert-same 0
                          (rev-mismatch "abcd" "zbcd")))
       (test equal-so-mismatch-not-found
             (assert-nil (rev-mismatch "abcd" "abcd")))
       ))
