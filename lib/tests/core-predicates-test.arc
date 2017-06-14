(suite predicates
       (suite bound
              (test t-for-assigned-variables
                    (assert-t ((fn () (assign foo 'bar) (bound 'foo)))))
              (test nil-for-unset-items
                    (assert-nil (bound 'toto-yadda)))
              (test nil-for-local-parameters
                    (assert-nil ((fn (yadda-toto) (bound 'yadda-toto))
                                 "blah"))))
       (suite less-than
              (test first-number-less-than-second
                    (assert-t (< 1 2)))
              (test first-number-greater-than-second
                    (assert-nil (< 20 5)))
              (test first-number-equal-to-second
                    (assert-nil (< 20 20)))
              (test first-string-less-than-second
                    (assert-t (< "bar" "foo")))
              (test first-string-greater-than-second
                    (assert-nil (< "zizi" "alpha")))
              (test first-string-equal-to-second
                    (assert-nil (< "foo" "foo")))
              (test first-symbol-less-than-second
                    (assert-t (< 'bar 'foo)))
              (test first-symbol-greater-than-second
                    (assert-nil (< 'zizi 'alpha)))
              (test first-symbol-equal-to-second
                    (assert-nil (< 'foo 'foo)))
              (test is-variadic (assert-t (< 1 2 3 4)))
              (test variadic-works-beyond-two-args
                    (assert-nil (< 1 2 10 4)))
              (test is-variadic-on-characters
                    (assert-t (< #\a #\c #\z)))
              (test variadic-on-characters-works-beyond-two-args
                    (assert-nil (< #\a #\b #\z #\d))))
       (suite greater-than
              (test first-number-less-than-second
                    (assert-nil (> 1 2)))
              (test first-number-greater-than-second
                    (assert-t (> 20 5)))
              (test first-number-equal-to-second
                    (assert-nil (> 20 20)))
              (test first-string-less-than-second
                    (assert-nil (> "bar" "foo")))
              (test first-string-greater-than-second
                    (assert-t (> "zizi" "alpha")))
              (test first-string-equal-to-second
                    (assert-nil (> "foo" "foo")))
              (test first-symbol-less-than-second
                    (assert-nil (> 'bar 'foo)))
              (test first-symbol-greater-than-second
                    (assert-t (> 'zizi 'alpha)))
              (test first-symbol-equal-to-second
                    (assert-nil (> 'foo 'foo)))
              (test is-variadic (assert-t (> 4 3 2 1)))
              (test variadic-works-beyond-two-args
                    (assert-nil (> 4 3 22 1)))
              (test is-variadic-on-characters
                    (assert-t (> #\z #\c #\a)))
              (test variadic-on-characters-works-beyond-two-args
                    (assert-nil (> #\z #\y #\a #\w))))
       (suite exact
              (test integers (assert-t (exact 12)))
              (test reals-arent-exact-even-if-are-ints
                    (assert-nil (exact 12.0)))
              (test rational-resulting-in-ints
                    (assert-t (exact 3)))
              (test rational-not-resulting-in-int
                    (assert-nil (exact 12/5)))
              (test floats (assert-nil (exact 3.14))))
       (suite is
              (test nil-nil (assert-t (is nil nil)))
              (test t-t (assert-t (is t t)))
              (test t-nil (assert-nil (is t nil)))
              (test nil-t (assert-nil (is nil t)))
              (test space-characters
                    (assert-t (is #\space #\space)))
              (test newline-characters
                    (assert-t (is #\newline #\newline)))
              (test same-ints (assert-t (is 2 2)))
              (test same-rationals
                    (assert-t (is 22/7 22/7)))
              (test same-floats
                    (assert-t (is 1.618 1.618)))
              (test same-strings
                    (assert-t (is "foobar" "foobar")))
              (test different-integers
                    (assert-nil (is 2 3)))
              (test same-numerical-value-different-types
                    (assert-nil (is 3/4 0.75)))
              (test different-floats
                    (assert-nil (is 1.1234567 1.1234569)))
              (test different-strings
                    (assert-nil (is "foobar" "toto")))
              (test variadic-for-same-numbers
                    (assert-t (is 1.23 1.23 1.23 1.23)))
              (test variadic-for-different-numbers
                    (assert-nil (is 1.23 1.23 1.23 3.14159265)))
              (test variadic-for-same-strings
                    (assert-t (is "f" "f" "f" "f" "f" "f")))
              (test variadic-for-different-strings
                    (assert-nil (is "f" "f" "f" "g" "f" "f")))
              (test variadic-for-symbols
                    (assert-t (is 'f 'f 'f 'f 'f 'f)))
              (test variadic-for-different-symbols
                    (assert-nil (is 'f 'f 'f 'g 'f 'f)))
              (test identical-lists-are-different
                    (assert-nil (is (list 'cons) (list 'cons))))
              (test empty-lists (assert-t (is '() '())))))

