(suite types
       (suite type
              (test nil-is-symbol
                    (assert-same 'sym (type nil)))
              (test empty-list-is-symbol
                    (assert-same 'sym (type ())))
              (test t-is-symbol
                    (assert-same 'sym (type t)))
              (test symbol (assert-same 'sym (type 'foo)))
              (test string
                    (assert-same 'string (type "foo")))
              (test integer (assert-same 'int (type 12)))
              (test rational-that-is-integer
                    (assert-same 'int (type 4)))
              (test rational
                    (assert-same 'num (type 1/4)))
              (test float
                    (assert-same 'num (type 3.1415)))
              (test double-that-is-integer
                    (assert-same 'num (type 3.0)))
              (test list
                    (assert-same 'cons (type '(a b c))))
              (test fn
                    (assert-same 'fn (type (fn (x) x))))
              (test char (assert-same 'char (type #\a)))
              (test newline
                    (assert-same 'char (type #\newline)))
              (test table
                    (assert-same 'table (type (table)))))
       (suite coerce
              (test nil
                    (assert-same "" (coerce nil 'string)))
              (test double->int
                    (assert-same 3 (coerce 3.14 'int)))
              (suite strings
                     (test list
                           (assert-same '(#\a #\b #\c)
                                        (coerce "abc" 'cons)))
                     (test sym
                           (assert-same 'foo (coerce "foo" 'sym)))
                     (test empty-string
                           (assert-same '|| (coerce "" 'sym)))
                     (suite string->int
                            (test simple-coercion
                                  (assert-same 256 (coerce "256" 'int)))
                            (test integer-rational
                                  (assert-same 3 (coerce "12/4" 'int)))
                            (test round-rational-to-int
                                  (assert-same 1 (coerce "3/4" 'int)))
                            (test round-rational-to-int-in-binary
                                  (assert-same 1 (coerce "101/100" 'int 2)))
                            (test round-float-to-int
                                  (assert-same 14 (coerce "14.2857" 'int)))
                            (test scientific-notation
                                  (assert-same 13000000000
                                               (coerce "1.3E10" 'int)))
                            (test lower-case-scientific-notation
                                  (assert-same 13000000000
                                               (coerce "1.3e10" 'int)))
                            (test round-float-to-int-in-binary
                                  (assert-same 3 (coerce "10.11" 'int 2)))
                            (test scientific-notation-to-binary
                                  (assert-same 80 (coerce "101E100" 'int 2)))
                            (test hex-to-binary
                                  (assert-same 255 (coerce "FF" 'int 16)))
                            (test hex-string-with-E
                                  (assert-same 62 (coerce "3E" 'int 16)))
                            (test coerce-positive-infinity-to-int
                                  (assert-error (coerce "+inf.0" 'int)
                                                "inexact->exact: no exact representation\n  number: +inf.0"))
                            (test coerce-negative-infinity-to-int
                                  (assert-error (coerce "-inf.0" 'int)
                                                "inexact->exact: no exact representation\n  number: -inf.0")))
                     (suite string->num
                            (test rational
                                  (assert-same 3/4 (coerce "3/4" 'num)))
                            (test rational-is-num
                                  (assert-same 'num
                                               (type (coerce "3/4" 'num))))
                            (test integer-valued-rational
                                  (assert-same 3 (coerce "12/4" 'num)))
                            (test integer-rational-is-int
                                  (assert-same 'int
                                               (type (coerce "12/4" 'num))))
                            (test integer-float
                                  (assert-same 'num
                                               (type (coerce "3.0" 'num))))
                            (test rational-in-binary
                                  (assert-same 5/4 (coerce "101/100" 'num 2)))
                            (test float
                                  (assert-same 14.2857
                                               (coerce "14.2857" 'num)))
                            (test scientific-notation
                                  (assert-same 13000000000.0
                                               (coerce "1.3E10" 'num)))
                            (test scientific-notation-with-plus-sign
                                  (assert-same 13000000000.0
                                               (coerce "1.3e+10" 'num)))
                            (test scientific-notation-with-minus-sign
                                  (assert-same t
                                               (< 0.012999 (coerce "1.3e-2" 'num) 0.013001)))
                            (test binary-float
                                  (assert-same 2.75 (coerce "10.11" 'num 2)))
                            (test scientific-notation-binary-float
                                  (assert-same 2816.0
                                               (coerce "10.11E1010" 'num 2)))
                            (test scientific-notation-trinary-float
                                  (assert-same 709180566103791.0
                                               (coerce "10.11E1010" 'num 3)))
                            (test scientific-notation-binary
                                  (assert-same 80.0 (coerce "101E100" 'num 2)))
                            (test type-is-num
                                  (assert-same 'num
                                               (type (coerce "80.0" 'num))))
                            (test positive-infinity
                                  (assert-same +inf.0 (coerce "+inf.0" 'num)))
                            (test negative-infinity
                                  (assert-same -inf.0 (coerce "-inf.0" 'num)))
                            (test nan
                                  (assert-same "+nan.0"
                                               (coerce (coerce "+nan.0" 'num) 'string))))
                     (suite coercions-to-same-type
                            (test symbol
                                  (assert-same 'foo (coerce 'foo 'sym)))
                            (test string
                                  (assert-same "montmartre"
                                               (coerce "montmartre" 'string)))
                            (test int
                                  (assert-same 2079 (coerce 2079 'int))))
                     (test sym->string
                           (assert-same "foo" (coerce 'foo 'string)))
                     (test empty-sym-to-string
                           (assert-same "" (coerce '|| 'string)))
                     (test list->string
                           (assert-same "zoo, no?"
                                        (coerce '(#\z #\o #\o #\, #\space #\n #\o #\?)
                                                'string)))
                     (test list->email-string
                           (assert-same "abc@@example.com"
                                        (coerce '(#\a #\b #\c #\@ #\e #\x #\a #\m #\p #\l #\e #\. #\c #\o #\m)
                                                'string)))
                     (test list->string-with-non-chars
                           (assert-same "12x34y56/17z3.1415"
                                        (coerce '(12 #\x 34 #\y 56/17 #\z 3.1415)
                                                'string)))
                     (test list-including-empty-string
                           (assert-same "" (coerce '("") 'string)))
                     (suite characters
                            (test char->int
                                  (assert-same 65 (coerce #\A 'int)))
                            (test char->int-with-unused-base-arg
                                  (assert-same 110 (coerce #\n 'int 16)))
                            (test char->string
                                  (assert-same "A" (coerce #\A 'string)))
                            (test char->sym
                                  (assert-same 'g (coerce #\g 'sym))))
                     (suite numbers
                            (test int->string
                                  (assert-same "66" (coerce 66 'string)))
                            (test int->num
                                  (assert-same 0 (coerce 0 'num)))
                            (test int->char
                                  (assert-same #\C (coerce 67 'char)))
                            (test int->string-with-specified-base
                                  (assert-same "77" (coerce 63 'string 8)))
                            (test double->string
                                  (assert-same "0.0004523"
                                               (coerce 0.0004523 'string)))
                            (test rational->to-string-in-specified-base
                                  (assert-same "51/15"
                                               (coerce 41/13 'string 8)))
                            (suite float-rounds-to-even
                                   (test 1-and-a-half
                                         (assert-same 2 (coerce 1.5 'int)))
                                   (test 2-and-a-half
                                         (assert-same 2 (coerce 2.5 'int)))
                                   (test 3-and-a-half
                                         (assert-same 4 (coerce 3.5 'int)))
                                   (test 4-and-a-half
                                         (assert-same 4 (coerce 4.5 'int)))
                                   (test 5-and-a-half
                                         (assert-same 6 (coerce 5.5 'int)))
                                   (test 6-and-a-half
                                         (assert-same 6 (coerce 6.5 'int))))
                            (suite rational-rounds-to-even
                                   (test 1-and-a-half
                                         (assert-same 2 (coerce 3/2 'int)))
                                   (test 2-and-a-half
                                         (assert-same 2 (coerce 5/2 'int)))
                                   (test 3-and-a-half
                                         (assert-same 4 (coerce 7/2 'int)))
                                   (test 4-and-a-half
                                         (assert-same 4 (coerce 9/2 'int)))
                                   (test 5-and-a-half
                                         (assert-same 6 (coerce 11/2 'int)))
                                   (test 6-and-a-half
                                         (assert-same 6 (coerce 13/2 'int))))
                            (suite rational-rounds-with-unused-base-arg
                                   (test 1-and-a-half
                                         (assert-same 2 (coerce 3/2 'int 16)))
                                   (test 2-and-a-half
                                         (assert-same 2 (coerce 5/2 'int 16)))
                                   (test 3-and-a-half
                                         (assert-same 4 (coerce 7/2 'int 16)))
                                   (test 4-and-a-half
                                         (assert-same 4 (coerce 9/2 'int 16)))
                                   (test 5-and-a-half
                                         (assert-same 6 (coerce 11/2 'int 16)))
                                   (test 6-and-a-half
                                         (assert-same 6 (coerce 13/2 'int 16)))))))
       (suite annotation
              (test identity
                    (assert-same 'foo (rep 'foo)))
              (test dont-wrap-objects-of-same-type
                    (assert-same 'foo
                                 (rep (annotate 'a (annotate 'a 'foo)))))
              (test can-get-type
                    (assert-same 'foo
                                 (type (annotate 'foo 'bar))))
              (test can-get-value
                    (assert-same 'bar
                                 (rep (annotate 'foo 'bar))))))

