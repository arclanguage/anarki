(suite types
       (suite type
              nil-is-symbol (assert-same 'sym
                                         (type nil))
              empty-list-is-symbol (assert-same 'sym
                                                (type ()))
              t-is-symbol (assert-same 'sym
                                       (type t))
              symbol (assert-same 'sym
                                  (type 'foo))
              string (assert-same 'string
                                  (type "foo"))
              integer (assert-same 'int
                                   (type 12))
              rational-that-is-integer (assert-same 'int
                                                    (type 12/3))
              rational (assert-same 'num
                                    (type 1/4))
              float (assert-same 'num
                                 (type 3.1415))
              double-that-is-integer (assert-same 'num ;;yep, this is different behavior from rationals
                                                  (type 3.0))
              list (assert-same 'cons
                                (type '(a b c)))
              fn (assert-same 'fn
                              (type (fn (x) x)))
              char (assert-same 'char
                                (type #\a))
              newline (assert-same 'char
                                   (type #\newline))
              table (assert-same 'table
                                 (type (table))))
       (suite coerce
              nil (assert-same ""
                               (coerce nil 'string))
              double->int (assert-same 3
                                       (coerce 3.14 'int))
              (suite strings
                     list (assert-same '(#\a #\b #\c)
                                        (coerce "abc" 'cons))
                     sym (assert-same 'foo
                                      (coerce "foo" 'sym))
                     empty-string (assert-same '||
                                               (coerce "" 'sym))
                     (suite string->int
                            simple-coercion (assert-same 256
                                                         (coerce "256" 'int))
                            integer-rational (assert-same 3
                                                          (coerce "12/4" 'int))
                            round-rational-to-int (assert-same 1
                                                      (coerce "3/4" 'int))
                            round-rational-to-int-in-binary (assert-same 1
                                                                (coerce "101/100" 'int 2))
                            round-float-to-int (assert-same 14
                                                            (coerce "14.2857" 'int))
                            scientific-notation (assert-same 13000000000
                                                             (coerce "1.3E10" 'int))
                            lower-case-scientific-notation (assert-same 13000000000
                                                                        (coerce "1.3e10" 'int))
                            round-float-to-int-in-binary (assert-same 3
                                                                      (coerce "10.11" 'int 2))
                            scientific-notation-to-binary (assert-same 80
                                                                       (coerce "101E100" 'int 2))
                            hex-to-binary (assert-same 255
                                                       (coerce "FF" 'int 16))
                            hex-string-with-E (assert-same 62
                                                           (coerce "3E" 'int 16))
                            coerce-positive-infinity-to-int (assert-error (coerce "+inf.0" 'int)
                                                                          "inexact->exact: no exact representation\n  number: +inf.0")
                            coerce-negative-infinity-to-int (assert-error (coerce "-inf.0" 'int)
                                                                          "inexact->exact: no exact representation\n  number: -inf.0"))

                     (suite string->num
                            rational (assert-same 3/4
                                                  (coerce "3/4" 'num))
                            rational-is-num (assert-same 'num
                                                         (type (coerce "3/4" 'num)))
                            integer-valued-rational (assert-same 3
                                                                 (coerce "12/4" 'num))
                            integer-rational-is-int (assert-same 'int
                                                                 (type (coerce "12/4" 'num)))
                            integer-float (assert-same 'num
                                                       (type (coerce "3.0" 'num)))
                            rational-in-binary (assert-same 5/4
                                                            (coerce "101/100" 'num 2))
                            float (assert-same 14.2857
                                               (coerce "14.2857" 'num))
                            scientific-notation (assert-same 13000000000.0
                                                             (coerce "1.3E10" 'num))
                            scientific-notation-with-plus-sign (assert-same 13000000000.0
                                                                            (coerce "1.3e+10" 'num))
                            scientific-notation-with-minus-sign (assert-same t
                                                                             (< 0.012999 (coerce "1.3e-2" 'num) 0.013001))
                            binary-float (assert-same 2.75
                                                      (coerce "10.11" 'num 2))
                            scientific-notation-binary-float (assert-same 2816.0
                                                                    (coerce "10.11E1010" 'num 2))
                            scientific-notation-trinary-float (assert-same 709180566103791.0
                                                                     (coerce "10.11E1010" 'num 3))
                            scientific-notation-binary (assert-same 80.0
                                                                    (coerce "101E100" 'num 2))
                            type-is-num (assert-same 'num
                                                     (type (coerce "80.0" 'num)))
                            positive-infinity (assert-same +inf.0
                                                           (coerce "+inf.0" 'num))
                            negative-infinity (assert-same -inf.0
                                                           (coerce "-inf.0" 'num))
                            nan (assert-same "+nan.0"
                                             (coerce (coerce "+nan.0" 'num) 'string)))

                     (suite coercions-to-same-type
                            symbol (assert-same 'foo
                                                (coerce 'foo 'sym))
                            string (assert-same "montmartre"
                                                (coerce "montmartre" 'string))
                            int (assert-same 2079
                                             (coerce 2079 'int)))
                     sym->string (assert-same "foo"
                                              (coerce 'foo 'string) )
                     empty-sym-to-string (assert-same ""
                                                      (coerce '|| 'string))
                     list->string (assert-same "zoo, no?"
                                               (coerce '(#\z #\o #\o #\, #\space #\n #\o #\?) 'string))
                     list->email-string (assert-same "abc@@example.com"
                                                     (coerce '(#\a #\b #\c #\@ #\e #\x #\a #\m #\p #\l #\e #\. #\c #\o #\m) 'string))
                     list->string-with-non-chars (assert-same "12x34y56/17z3.1415"
                                                              (coerce '(12 #\x 34 #\y 56/17 #\z 3.1415) 'string))
                     list-including-empty-string (assert-same ""
                                                              (coerce '("") 'string))
                     (suite characters
                            char->int (assert-same 65
                                                   (coerce #\A 'int))
                            char->int-with-unused-base-arg (assert-same 110
                                                                        (coerce #\n 'int 16))
                            char->string (assert-same "A"
                                                      (coerce #\A 'string))
                            char->sym (assert-same 'g
                                                   (coerce #\g 'sym)))
                     (suite numbers
                            int->string (assert-same "66"
                                                     (coerce 66 'string))
                            int->num (assert-same 0
                                                  (coerce 0 'num))
                            int->char (assert-same #\C
                                                   (coerce 67 'char))
                            int->string-with-specified-base (assert-same "77"
                                                                         (coerce 63 'string 8))
                            double->string (assert-same "0.0004523"
                                                        (coerce 45.23e-5 'string))
                            rational->to-string-in-specified-base (assert-same "51/15"
                                                                               (coerce 41/13 'string 8))
                            (suite float-rounds-to-even
                                   1-and-a-half (assert-same 2
                                                             (coerce 1.5 'int))
                                   2-and-a-half (assert-same 2
                                                             (coerce 2.5 'int))
                                   3-and-a-half (assert-same 4
                                                             (coerce 3.5 'int))
                                   4-and-a-half (assert-same 4
                                                             (coerce 4.5 'int))
                                   5-and-a-half (assert-same 6
                                                             (coerce 5.5 'int))
                                   6-and-a-half (assert-same 6
                                                             (coerce 6.5 'int)))
                            (suite rational-rounds-to-even
                                   1-and-a-half (assert-same 2
                                                             (coerce 3/2 'int))
                                   2-and-a-half (assert-same 2
                                                             (coerce 5/2 'int))
                                   3-and-a-half (assert-same 4
                                                             (coerce 7/2 'int))
                                   4-and-a-half (assert-same 4
                                                             (coerce 9/2 'int))
                                   5-and-a-half (assert-same 6
                                                             (coerce 11/2 'int))
                                   6-and-a-half (assert-same 6
                                                             (coerce 13/2 'int)))
                            (suite rational-rounds-with-unused-base-arg
                                   1-and-a-half (assert-same 2
                                                             (coerce 3/2 'int 16))
                                   2-and-a-half (assert-same 2
                                                             (coerce 5/2 'int 16))
                                   3-and-a-half (assert-same 4
                                                             (coerce 7/2 'int 16))
                                   4-and-a-half (assert-same 4
                                                             (coerce 9/2 'int 16))
                                   5-and-a-half (assert-same 6
                                                             (coerce 11/2 'int 16))
                                   6-and-a-half (assert-same 6
                                                             (coerce 13/2 'int 16))))))
       (suite annotation
              identity (assert-same 'foo
                                    (rep 'foo))
              dont-wrap-objects-of-same-type (assert-same 'foo
                                                          (rep (annotate 'a (annotate 'a 'foo))))
              can-get-type (assert-same 'foo
                                        (type (annotate 'foo 'bar)))
              can-get-value (assert-same 'bar
                                         (rep (annotate 'foo 'bar)))))
