(load "lib/boyer-moore.arc")
(suite boyer-moore-scan-past
       (test found
             (assert-same '(97 98 99)
                          (scan-past "de" (instring "abcde"))))
       )
