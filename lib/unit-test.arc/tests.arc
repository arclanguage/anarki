;; Copyright 2013-2015 Zachary Kanfer

;; This file is part of unit-test.arc .

;; unit-test.arc is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; unit-test.arc is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with unit-test.arc.  If not, see <http://www.gnu.org/licenses/>.



(suite unit-test-tests
       (suite comparisons
              (suite same
                     (test numbers-same (assert-t (same 1 1)))
                     (test numbers-diff (assert-nil (same 1 2)))
                     (test strings-same (assert-t (same "Now Mary" "Now Mary")))
                     (test strings-diff (assert-nil (same "Now Mary" "Mow Nary")))
                     (test lists-same (assert-t (same '(1 2 3)
                                                      '(1 2 3))))
                     (test lists-diff (assert-nil (same '(1 2 3)
                                                        '(1 3 3))))
                     (test lists-sub-lists-of-the-second (assert-nil (same '(1 2 3)
                                                                           '(1 2 3 4))))
                     (test tables-same (assert-t (same (obj 1 2)
                                                       (obj 1 2))))
                     (test tables-different-vals (assert-nil (same (obj 1 2)
                                                                   (obj 1 1))))
                     (test tables-different-keys (assert-nil (same (obj 1 1)
                                                                   (obj 2 1))))
                     (test tables-extra-keys (assert-nil (same (obj 1 2)
                                                               (obj 1 2 3 4))))
                     (test cross-number-and-obj (assert-nil (same 1
                                                                  (obj 1 2))))
                     (test cross-obj-and-number (assert-nil (same (obj 1 2)
                                                                  1)))
                     (test cross-number-and-string (assert-nil (same 1
                                                                     "1")))
                     (test obj-as-key-of-obj-are-same (assert-t (same (w/table tbl (= (tbl (obj 1 2)) 'val))
                                                                      (w/table tbl (= (tbl (obj 1 2)) 'val)))))
                     (test obj-as-val-of-obj-are-same (assert-t (same (obj 1 (obj))
                                                                      (obj 1 (obj))))))

              (suite hash-same
                     (test empty (assert-t (hash-same (obj)
                                                      (obj))))
                     (test single-elt-same (assert-t (hash-same (obj 1 t)
                                                                (obj 1 t))))
                     (test single-elt-different-val (assert-nil (hash-same (obj 1 t)
                                                                           (obj 1 'pants))))
                     (test single-elt-different-key (assert-nil (hash-same (obj 1 t)
                                                                           (obj 2 t))))
                     (test multiple-elts-same (assert-t (hash-same (obj 1 t 2 'a (1) 2)
                                                                   (obj 1 t 2 'a (1) 2))))
                     (test multiple-elts-different-key (assert-nil (hash-same (obj 1 t 2 t 3 t)
                                                                              (obj 1 t 2 t 4 t))))
                     (test multiple-elts-different-val (assert-nil (hash-same (obj 1 t 2 t 3 t)
                                                                              (obj 1 t 2 t 3 4))))
                     (test extra-elt-on-left (assert-nil (hash-same (obj 1 t 2 t)
                                                                    (obj 1 t 2 t 3 t))))
                     (test extra-elt-on-right (assert-nil (hash-same (obj 1 t 2 t 3 t)
                                                                     (obj 1 t 2 t))))
                     (test does-order-matter? (assert-t (hash-same (obj 1 t 2 t)
                                                                   (obj 2 t 1 t))))))

       (suite asserts
              (suite assert
                     (test t-doesnt-error (assert t "shouldn't throw"))
                     (test nil-errors (assert-error (assert nil "should throw")
                                                    "should throw")))


              (suite assert-same
                     (test equal-vals (assert-same 1 1 "equal values are good"))
                     (test lists-are-same (assert-same (list 1) (list 1) "equal lists are good"))
                     (test different-vals (assert-error (assert-same 1 2)
                                                        "2 should be 1 but instead was 2"))
                     (test hashtables-are-same (assert-same (obj 1 2) (obj 1 2) "equal hashtables are good")))

              (suite assert-t
                     (test t-is-good (assert-t t))
                     (test nil-throws (assert-error (assert-t nil)
                                                    "nil should be non-nil but instead was nil"))
                     (test 3-is-treated-as-good (assert-t 3)))

              (suite assert-nil
                     (test t-is-good (assert-error (assert-nil t)
                                                   "t should be nil but instead was t"))
                     (test nil-is-good (assert-nil nil))
                     (test 3-is-treated-as-bad (assert-error (assert-nil 3)
                                                             "3 should be nil but instead was 3")))

              (suite assert-error
                     (test err-is-ok (assert-error (err "oh dear!")
                                                   "oh dear!"))
                     (test no-err-fails (assert-nil (errsafe (do (assert-error "no error")
                                                                 t))))
                     (test checks-error-message (assert-nil (errsafe (do (assert-error (err "this is bad")
                                                                                       "this is the wrong message")
                                                                         t))))
                     (test valid-error-message-passes (assert-error (err "oh no...")
                                                                    "oh no..."))
                     (test no-error-fails-when-error-message-given (assert-nil (errsafe (do (assert-error "no error" "error message")
                                                                                            t))
                                                                               "Even when an error message is given, not having an error should fail assert-error.")))

              (suite assert-no-error
                     (test no-error-is-ok (assert-no-error 3))

                     ;;errsafe returns nil if error happens, so if assert-no-error properly errors
                     ;;(because there was an error in its body), errsafe will return nil.
                     ;;If there's no error, it'll return t and not error
                     (test error-fails (assert-nil (errsafe (do (assert-no-error (err "error!"))
                                                                t))))))

       (suite make-test
              (setup sample-test (make-test 'sample-suite 'sample-test nil nil 3)
                     simple-setup (make-test 'sample-suite 'simple-setup (x 3) nil x)
                     multiple-variable-setup (make-test 'sample-suite 'multiple-variable-setup (x 3 y 4) nil (+ x y))
                     reliant-variable-setup (make-test 'sample-suite 'reliant-variable-setup (x 3 y x) nil (+ x y))
                     let-setup (let suite-name 'my-suite-name
                                    (make-test suite-name 'test-name nil nil t)))
              (test test-name (assert-same 'sample-test sample-test!test-name))
              (test suite-name (assert-same 'sample-suite sample-test!suite-name))
              (test test-fn (assert-same 3
                                         ((sample-test!test-fn)
                                          'return-value)))
              (test simple-setup (assert-same 3
                                              ((simple-setup!test-fn)
                                               'return-value)))
              (test multiple-variable-setup (assert-same 7
                                                         ((multiple-variable-setup!test-fn)
                                                          'return-value)))
              (test reliant-variable-setup (assert-same 6
                                                        ((reliant-variable-setup!test-fn)
                                                         'return-value)))

              (test suite-names-cant-have-periods (assert-error (make-test 'sample 'test.name nil nil)
                                                                "Test names can't have periods in them. test.name does."))
              (test let-bound-name-works (assert-same 'my-suite-name
                                                      let-setup!suite-name))

              (suite teardown
                     (setup failing-test-and-teardown-result (((make-test 'sample 'failing nil ((err "fails in teardown")) (err "fails in test")) 'test-fn))
                            failing-teardown-result (((make-test 'sample 'failing-teardown nil ((err "fails in teardown")) 'passes-in-test)
                                                      'test-fn))
                            multiple-teardown-passing-result (((make-test 'sample 'multiple-teardown nil ((+ 1 2) (+ 3 4))) 'test-fn))
                            multiple-teardown-first-fails-result (((make-test 'sample 'multiple-teardown nil ((err "first teardown fails") (+ 3 4))) 'test-fn))
                            multiple-teardown-second-fails-result (((make-test 'sample 'multiple-teardown nil ((+ 1 2) (err "second teardown fails"))) 'test-fn)))
                     (test failing-test-and-teardown-has-test-error-message (assert-same "fails in test" failing-test-and-teardown-result!details))
                     (test failing-teardown-message (assert-same "fails in teardown" failing-teardown-result!details))
                     (test failing-teardown-fails (assert-same 'fail failing-teardown-result!status))
                     (test multiple-teardown-passes (assert-same 'pass multiple-teardown-passing-result!status))
                     (test multiple-teardown-first-fails (assert-same 'fail multiple-teardown-first-fails-result!status))
                     (test multiple-teardown-second-fails (assert-same 'fail multiple-teardown-second-fails-result!status))))

       (suite make-test-fn
              ;;make-test-fn should take symbols as args.
              ;;so (make-test-fn 'sample-suite 'pass-test nil 3)
              ;;or the calling code should unpack the gensym.
              (setup pass-test-val ((make-test-fn 'sample-suite 'pass-test nil nil 3))
                     fail-test-val ((make-test-fn 'sample-suite 'fail-test nil nil (err "failing...")))
                     simple-setup ((make-test-fn 'sample-suite 'w/setup (x 3) nil x))
                     multiple-variable-setup ((make-test-fn 'sample-suite 'multiple-variable-setup (x 3 y 4) nil (+ x y)))
                     reliant-variable-setup ((make-test-fn 'sample-suite 'reliant-variable-setup (x 3 y x) nil (+ x y)))
                     multiple-asserts-all-pass-val ((make-test-fn 'sample-suite 'multiple-asserts-all-pass-val nil nil (assert-same 2 2) (assert-same 1 1)))
                     multiple-asserts-first-fails-val ((make-test-fn 'sample-suite 'multiple-asserts-first-fails-val nil nil (assert-same 2 1) (assert-same 1 1)))
                     multiple-asserts-second-fails-val ((make-test-fn 'sample-suite 'multiple-asserts-second-fails-val nil nil (assert-same 2 2) (assert-same 1 42)))
                     let-bound-passing-names (let name 'my-special-name ((make-test-fn name name nil nil (assert-t t))))
                     let-bound-failing-names (let name 'my-special-name ((make-test-fn name name nil nil (assert-t nil))))
                     teardown-pass-result ((make-test-fn 'sample-suite 'teardown-fine nil nil 3))
                     teardown-fail-result ((make-test-fn 'sample-suite 'teardown-errors nil ((err "teardown caused this!")) 3))
                     teardown-multiple-values-result ((make-test-fn 'sample-suite 'teardown-multiple-values nil ((+ 1 2) (+ 3 4))))
                     passing-result-setup-and-teardown ((make-test-fn 'sample-suite 'setup-and-teardown (x 3) ((prn "teardown")) (assert-same 3 3) (assert-same 4 (* 2 2))))
                     failing-result-setup-and-teardown ((make-test-fn 'sample-suite 'setup-and-teardown (x 3) ((prn "teardown")) (assert-same 3 3) (assert-same 4 (* 2 -2)))))
              (test pass-has-right-return-value (assert-same 3 pass-test-val!return-value))
              (test pass-has-test-name (assert-same 'pass-test pass-test-val!test-name))
              (test fail-has-test-name (assert-same 'fail-test fail-test-val!test-name))
              (test pass-has-suite-name (assert-same 'sample-suite pass-test-val!suite-name))
              (test fail-has-suite-name (assert-same 'sample-suite fail-test-val!suite-name))
              (test pass-has-pass-status (assert-same 'pass pass-test-val!status))
              (test fail-has-fail-status (assert-same 'fail fail-test-val!status))
              (test fail-has-proper-details (assert-same "failing..." fail-test-val!details))
              (test setup-has-right-value (assert-same 3 simple-setup!return-value))
              (test multiple-variables-are-setup-properly (assert-same 7 multiple-variable-setup!return-value))
              (test reliant-variables-are-setup-properly (assert-same 6 reliant-variable-setup!return-value))
              (test multiple-asserts-work (assert-t (result-is-pass multiple-asserts-all-pass-val)))
              (test multiple-asserts-first-fails (assert-nil (result-is-pass multiple-asserts-first-fails-val)))
              (test multiple-asserts-second-fails (assert-nil (result-is-pass multiple-asserts-second-fails-val)))
              (test let-bound-passing-name-suite-name (assert-same 'my-special-name let-bound-passing-names!suite-name))
              (test let-bound-passing-name-test-name (assert-same 'my-special-name let-bound-passing-names!test-name))
              (test let-bound-failing-name-suite-name (assert-same 'my-special-name let-bound-failing-names!suite-name))
              (test let-bound-failing-name-test-name (assert-same 'my-special-name let-bound-failing-names!test-name))
              (test teardown-fail-causes-failure (assert-same 'fail teardown-fail-result!status))
              (test teardown-fail-error-message (assert-same "teardown caused this!" teardown-fail-result!details))
              (test teardown-pass-success (assert-same 'pass teardown-pass-result!status))
              (test teardown-pass-result (assert-same 3 teardown-pass-result!return-value))
              (test passing-has-proper-code
                    (assert-same '(withs (x 3) (do1 (do (assert-same 3 3) (assert-same 4 (* 2 2))) (prn "teardown")))
                                 passing-result-setup-and-teardown!code))
              (test failing-has-proper-code
                    (assert-same '(withs (x 3) (do1 (do (assert-same 3 3) (assert-same 4 (* 2 -2))) (prn "teardown")))
                                 failing-result-setup-and-teardown!code)))

       (suite suite-creation
              (suite make-suite
                     (setup empty-suite (make-suite nil (suite empty-suite-1))
                            single-test (make-suite nil (suite single-test-suite (test a 3)))
                            single-suite (make-suite nil (suite single-suite-suite (suite a (test b 3))))
                            one-of-each (make-suite nil (suite one-of-each-suite (test a 3) (suite and-nested (test b 4))))
                            two-of-each (make-suite nil (suite two-of-each-suite (test a 3) (suite b (test c 4)) (test d 5) (suite e (test f 6) (test g 7))))
                            test-with-setup (make-suite nil (suite setup-suite (setup x 3) (test a x)))
                            suite-with-teardown (make-suite nil (suite teardown-suite (teardown (err "teardown running!")) (test foo nil)))
                            suite-with-parent (make-suite my-parent (suite empty-child-suite)))
                     (test empty-suite-has-right-name (assert-same 'empty-suite-1
                                                                   empty-suite!full-suite-name))
                     (test empty-suite-has-no-tests (assert-same 0
                                                                 (len empty-suite!tests)))
                     (test empty-suite-has-no-nested-suites (assert-same 0
                                                                         (len empty-suite!nested-suites)))
                     (test single-test-has-right-name (assert-same 'single-test-suite
                                                                   single-test!full-suite-name))
                     (test single-test-has-one-test (assert-same 1
                                                                 (len single-test!tests)))
                     (test single-test-has-no-nested-suites (assert-same 0
                                                                         (len single-test!nested-suites)))
                     (test single-test-has-right-test (assert-same 'a
                                                                   single-test!tests!a!test-name))
                     (test single-test-has-right-suite (assert-same 'single-test-suite
                                                                    single-test!tests!a!suite-name))
                     (test single-test-has-right-return-value (assert-same 3
                                                                           ((single-test!tests!a!test-fn) 'return-value)))
                     (test single-test-has-right-test-name (assert-same 'a
                                                                        ((single-test!tests!a!test-fn) 'test-name)))
                     (test single-test-has-right-full-test-name (assert-same 'single-test-suite.a
                                                                             ((single-test!tests!a!test-fn) 'full-test-name)))
                     (test single-test-has-right-suite-name (assert-same 'single-test-suite
                                                                         ((single-test!tests!a!test-fn) 'suite-name)))
                     (test single-test-has-right-status (assert-same 'pass
                                                                     ((single-test!tests!a!test-fn) 'status)))
                     (test single-suite-has-right-suite-name (assert-same 'single-suite-suite
                                                                          single-suite!full-suite-name))
                     (test single-suite-has-right-full-name (assert-same 'single-suite-suite
                                                                         single-suite!full-suite-name))
                     (test single-suite-has-no-tests (assert-same 0
                                                                  (len single-suite!tests)))
                     (test single-suite-has-one-nested-suite (assert-same 1
                                                                          (len single-suite!nested-suites)))
                     (test one-of-each-has-right-name (assert-same 'one-of-each-suite
                                                                   one-of-each!full-suite-name))
                     (test one-of-each-has-one-test (assert-same 1
                                                                 (len one-of-each!tests)))
                     (test one-of-each-has-one-nested-suite (assert-same 1
                                                                         (len one-of-each!nested-suites)))
                     (test two-of-each-has-right-name (assert-same 'two-of-each-suite
                                                                   two-of-each!full-suite-name))
                     (test two-of-each-has-two-tests (assert-same 2
                                                                  (len two-of-each!tests)))
                     (test two-of-each-has-two-nested-suites (assert-same 2
                                                                          (len two-of-each!nested-suites)))
                     (test test-with-setup-has-right-name (assert-same 'setup-suite
                                                                       test-with-setup!full-suite-name))
                     (test test-with-setup-has-one-test (assert-same 1
                                                                     (len test-with-setup!tests)))
                     (test test-with-setup-has-no-nested-suites (assert-same 0
                                                                             (len test-with-setup!nested-suites)))
                     (test setup-is-done-properly (assert-same 3
                                                               ((test-with-setup!tests!a!test-fn)
                                                                'return-value)))
                     (test teardown-block-causes-failure (assert-same 'fail
                                                                  ((suite-with-teardown!tests!foo!test-fn) 'status)))

                     (test single-suites-nested-suite-has-right-suite-name (assert-same 'a
                                                                                        single-suite!nested-suites!a!suite-name))
                     (test single-suites-nested-suite-has-right-full-suite-name (assert-same 'single-suite-suite.a
                                                                                             single-suite!nested-suites!a!full-suite-name))
                     (test single-suite-nested-suite-contains-one-test (assert-same 1
                                                                                    (len single-suite!nested-suites!a!tests)))
                     (test single-suite-nested-suite-contains-right-test (assert-same 'b
                                                                                      single-suite!nested-suites!a!tests!b!test-name))
                     (test single-suite-nested-suite-test-has-right-suite-name (assert-same 'single-suite-suite.a
                                                                                            single-suite!nested-suites!a!tests!b!suite-name))
                     (test periods-in-suite-names-error (assert-error (make-suite nil (suite bad.name (test whatever t)))
                                                                      "Suite names can't have periods in them. bad.name does."))
                     (test mixed-checks-for-shadowing (assert-error (make-suite nil (suite a (test b t) (suite b (test c t))))
                                                                    "In suite a, there are two things named b."))
                     (test parent-suite-name-is-respected (assert-same 'my-parent.empty-child-suite
                                                                       suite-with-parent!full-suite-name))
                     (test two-tests-with-same-name-error (assert-error (make-suite nil (suite a (test b t) (test b nil)))
                                                                        "In suite a, there are two things named b."))
                     (test two-suites-with-same-name-error (assert-error (make-suite nil (suite a (suite b) (suite b)))
                                                                         "In suite a, there are two things named b."))
                     (test suite-with-bad-initial-value-errors (assert-error (make-suite nil (bad-syntax a (test b t)))
                                                                             "The first element of the suite should be the symbol 'suite. It is: bad-syntax"))
                     (test suite-with-bad-later-value-errors (assert-error (make-suite nil (suite a (bad-value b t)))
                                                                           "Each element of a suite should begin with one of the symbols 'suite, 'test, 'setup, or 'teardown. This doesn't: (bad-value b t)."))
                     (test suite-with-bad-setup-errors (assert-error (make-suite nil (suite a (setup x) (test val t)))
                                                                     "In a setup clause, all variables should have a value. This doesn't: (setup x)")))

              (suite add-tests-to-suite
                     (setup one-test (add-tests-to-suite (inst 'suite 'suite-name 'base 'full-suite-name 'base)
                                                         base
                                                         (x 1)
                                                         nil
                                                         ((test passes (assert-same x 1))))
                            one-test-returns-proper-value (add-tests-to-suite (inst 'suite)
                                                                              nil
                                                                              (x 42)
                                                                              nil
                                                                              ((test returns-x x)))
                            two-tests (add-tests-to-suite (inst 'suite)
                                                          nil
                                                          (x 1)
                                                          nil
                                                          ((test passes (assert-same x 1))
                                                           (test fails (assert-same x 2))))
                            test-with-multiple-lines (add-tests-to-suite
                                                      (inst 'suite)
                                                      nil
                                                      nil
                                                      nil
                                                      ((test two-lines
                                                             (assert-t t)
                                                             (assert-t nil))))
                            suite-with-teardown-test-result
                            (((((add-tests-to-suite
                                 (inst 'suite)
                                 nil
                                 nil
                                 ((err "teardown running!"))
                                 ((test foo 3)))
                                'tests)
                               'foo)
                              'test-fn))
                            suite-with-multiple-tests
                            (add-tests-to-suite (inst 'suite)
                                                nil
                                                nil
                                                ((err "teardown running!"))
                                                ((test one nil)
                                                 (test two nil)))
                            first-test-with-teardown-result (suite-with-multiple-tests!tests!one!test-fn)
                            second-test-with-teardown-result (suite-with-multiple-tests!tests!two!test-fn))
                     (test no-tests (assert-same (inst 'suite)
                                                 (add-tests-to-suite (inst 'suite) nil nil nil ())))
                     (test one-is-added
                           (assert-same 1
                                        (len one-test!tests)))
                     (test one-is-in-right-place
                           (assert-t one-test!tests!passes))
                     (test one-has-right-test-name
                           (assert-same 'passes
                                        one-test!tests!passes!test-name))
                     (test one-test-test-has-right-suite-name
                           (assert-same 'base
                                        one-test!tests!passes!suite-name))
                     (test return-value-is-right (assert-same 42
                                                              ((one-test-returns-proper-value!tests!returns-x!test-fn) 'return-value)))
                     (test two-are-added
                           (assert-same 2
                                        (len two-tests!tests)))
                     (test two-is-in-right-places
                           (assert-t two-tests!tests!passes)
                           (assert-t two-tests!tests!fails))
                     (test tests-are-wrapped-to-create-test-result-template
                           (let result (one-test!tests!passes!test-fn)
                                ;;arc templates are of type 'table
                                ;;Anarki templates are of type 'tem
                                ;;So work with either.
                                (assert-t (or (isa result 'table)
                                              (isa result 'tem)))))
                     (test passing-test-passes
                           (let result (two-tests!tests!passes!test-fn)
                                (assert-t (is result!status 'pass))))
                     (test failing-test-fails
                           (let result (two-tests!tests!fails!test-fn)
                                (assert-t (is result!status 'fail))))
                     (test multiple-lines-fails
                           (let result (test-with-multiple-lines!tests!two-lines!test-fn)
                                (assert-t (is result!status 'fail))))
                     (test shadowed-tests-error
                           (assert-error (add-tests-to-suite (inst 'suite 'suite-name 'base 'full-suite-name 'base 'tests (obj this-shadows (inst 'test 'test-name 'this-shadows)))
                                                             base
                                                             nil
                                                             nil
                                                             ((test this-shadows (assert-same 3 3))))
                                         "In suite base, there are two things named this-shadows."))
                     (test shadowed-suite-errors
                           (assert-error (add-tests-to-suite (inst 'suite 'suite-name 'base 'full-suite-name 'base 'nested-suites (obj this-shadows (inst 'suite 'suite-name 'this-shadows)))
                                                             base
                                                             nil
                                                             nil
                                                             ((suite this-shadows (test whatever (assert-same 3 3)))))
                                         "In suite base, there are two things named this-shadows."))
                     (test first-test-teardown-runs
                           (assert-same 'fail first-test-with-teardown-result!status))
                     (test first-test-teardown-has-right-message
                           (assert-same "teardown running!"
                                        first-test-with-teardown-result!details))
                     (test second-test-teardown-runs
                           (assert-same 'fail second-test-with-teardown-result!status))
                     (test second-test-teardown-has-right-message
                           (assert-same "teardown running!"
                                        second-test-with-teardown-result!details)))
              (suite add-suites-to-suite
                     (setup one-suite (add-suites-to-suite base
                                                           (inst 'suite 'suite-name 'base 'full-suite-name 'base)
                                                           ((suite nested-one)))
                            two-suites (add-suites-to-suite base
                                                            (inst 'suite 'suite-name 'base 'full-suite-name 'base)
                                                            ((suite nested-one)
                                                             (suite nested-two))))
                     (test no-suites (assert-same (inst 'suite)
                                                  (add-suites-to-suite empty
                                                                       (inst 'suite)
                                                                       ())))
                     (test one-suite-has-right-number-of-children
                           (assert-same 1
                                        (len one-suite!nested-suites)))
                     (test one-suite-is-in-right-place
                           (assert-t one-suite!nested-suites!nested-one))
                     (test one-suite-has-right-name
                           (assert-same 'nested-one
                                        one-suite!nested-suites!nested-one!suite-name))
                     (test one-suite-has-right-full-name
                           (assert-same 'base.nested-one
                                        one-suite!nested-suites!nested-one!full-suite-name))
                     (test two-suites-has-right-number-of-children
                           (assert-same 2
                                        (len two-suites!nested-suites)))
                     (test two-suites-first-is-in-right-place
                           (assert-t two-suites!nested-suites!nested-one))
                     (test two-suites-first-has-right-name
                           (assert-same 'nested-one
                                        two-suites!nested-suites!nested-one!suite-name))
                     (test two-suites-first-has-right-full-name
                           (assert-same 'base.nested-one
                                        two-suites!nested-suites!nested-one!full-suite-name))
                     (test two-suites-second-is-in-right-place
                           (assert-t two-suites!nested-suites!nested-two))
                     (test two-suites-second-has-right-name
                           (assert-same 'nested-two
                                        two-suites!nested-suites!nested-two!suite-name))
                     (test two-suites-second-has-right-full-name
                           (assert-same 'base.nested-two
                                        two-suites!nested-suites!nested-two!full-suite-name))
                     (test shadowing-suite-errors (assert-error (add-suites-to-suite base
                                                                                     (inst 'suite 'suite-name 'base 'full-suite-name 'base 'nested-suites (obj problem (inst 'suite 'suite-name 'problem)))
                                                                                     ((suite problem)))
                                                                "In suite base, there are two things named problem."))
                     (test shadowing-test-errors (assert-error (add-suites-to-suite base
                                                                                    (inst 'suite 'suite-name 'base 'full-suite-name 'base 'tests (obj problem (inst 'test 'test-name 'problem)))
                                                                                    ((suite problem)))
                                                               "In suite base, there are two things named problem.")))


              (suite would-shadow
                     (setup sample-suite (inst 'suite 'tests (obj test-child t) 'nested-suites (obj suite-child t)))
                     (test shadowing-test (assert-t (would-shadow sample-suite 'test-child)))
                     (test shadowing-suite (assert-t (would-shadow sample-suite 'suite-child)))
                     (test shadowing-nothing (assert-nil (would-shadow sample-suite 'something-else))))

              (suite keep-by-car
                     (setup nothing '(suite empty)
                            one-test '(suite one-test (test whatever t))
                            one-suite '(suite one-suite (suite nested (test whatever t)))
                            one-of-each '(suite one-suite (suite nested (test nested-whatever t)) (test whatever t))
                            two-of-each '(suite one-suite (suite nested (test nested-whatever t)) (test whatever t) (suite other-nested (test nested-whatever t)) (test other-whatever t)))

                     (test nothing-tests (assert-nil (keep-by-car nothing 'test)))
                     (test nothing-suites (assert-nil (keep-by-car nothing 'suite)))

                     (test one-test-tests (assert-same '((test whatever t))
                                                       (keep-by-car one-test 'test)))
                     (test one-test-suites (assert-nil (keep-by-car one-test 'suite)))

                     (test one-suite-tests (assert-nil (keep-by-car one-suite 'test)))
                     (test one-suite-suites (assert-same '((suite nested (test whatever t)))
                                                         (keep-by-car one-suite 'suite)))

                     (test one-of-each-tests (assert-same '((test whatever t))
                                                          (keep-by-car one-of-each 'test)))
                     (test one-of-each-suites (assert-same '((suite nested (test nested-whatever t)))
                                                           (keep-by-car one-of-each 'suite)))
                     (test two-of-each-tests (assert-same '((test whatever t) (test other-whatever t))
                                                          (keep-by-car two-of-each 'test)))
                     (test two-of-each-suites (assert-same '((suite nested (test nested-whatever t)) (suite other-nested (test nested-whatever t)))
                                                           (keep-by-car two-of-each 'suite))))

              (suite get-suite-structure-problems
                     (test beginning-wrong-symbol (assert-t (get-suite-structure-problems '(suitex arst (test a t)))))
                     (test beginning-not-a-symbol (assert-t (get-suite-structure-problems '((suite wrongly-nested) arst (test a t)))))
                     (test suite-name-not-a-symbol (assert-t (get-suite-structure-problems '(suite (arst) (test a t)))))
                     (test nested-bad-sym (assert-t (get-suite-structure-problems '(suite arst (oops-bad a t)))))
                     (test late-elements-not-a-list (assert-t (get-suite-structure-problems '(suite arst (test a t) what-is-this))))
                     (test two-errors-both-reported (let problems (get-suite-structure-problems '(suitex arst (test a t) what-is-this))
                                                         (assert-t (posmatch "suitex" problems))
                                                         (assert-t (posmatch "what-is-this" problems))))
                     (test wrong-number-of-symbols-in-setup (assert-t (get-suite-structure-problems '(suite a (setup x) (test val t)))))
                     (test everything-cool (assert-nil (get-suite-structure-problems '(suite arst (setup x 3) (test a t) (suite nested)))))))


       (suite count-passes
              (test 0-empty (assert-same 0
                                         (count-passes (inst 'suite-results
                                                             'test-results (obj)))))
              (test 0-stuff (assert-same 0
                                         (count-passes (inst 'suite-results
                                                             'test-results (obj fail (inst 'test-results 'status 'fail))))))
              (test 1 (assert-same 1
                                   (count-passes (inst 'suite-results
                                                       'test-results (obj numbers
                                                                          (inst 'test-results 'status 'pass))))))
              (test 2-flat (assert-same 2
                                        (count-passes (inst 'suite-results
                                                            'test-results (obj sample1 (inst 'test-results 'status 'pass)
                                                                               sample2 (inst 'test-results 'status 'fail)
                                                                               sample3 (inst 'test-results 'status 'pass))))))
              (test 3-nested (assert-same 3
                                          (count-passes (inst 'suite-results
                                                              'test-results (obj sample1 (inst 'test-results 'status 'pass)
                                                                                 sample2 (inst 'test-results 'status 'pass)
                                                                                 sample3 (inst 'test-results 'status 'fail))
                                                              'nested-suite-results (obj nested (inst 'suite-results
                                                                                                      'test-results (obj sample1 (inst 'test-results 'status 'pass)
                                                                                                                         sample2 (inst 'test-results 'status 'fail))))))))
              (test 1-empty-main-suite (assert-same 1
                                                    (count-passes (inst 'suite-results
                                                                        'nested-suite-results (obj nested (inst 'suite-results
                                                                                                                'test-results (obj sample1 (inst 'test-results 'status 'pass)
                                                                                                                                   sample2 (inst 'test-results 'status 'fail))))))))
              (test 3-doubly-nested (assert-same 3
                                                 (count-passes (inst 'suite-results
                                                                     'test-results (obj sample1 (inst 'test-results 'status 'fail))
                                                                     'nested-suite-results (obj nested1 (inst 'suite-results
                                                                                                              'test-results (obj sample2 (inst 'test-results 'status 'pass)
                                                                                                                                 sample3 (inst 'test-results 'status 'fail))
                                                                                                              'nested-suite-results (obj nested2 (inst 'suite-results
                                                                                                                                                       'test-results (obj sample4 (inst 'test-results 'status 'pass)
                                                                                                                                                                          sample5 (inst 'test-results 'status 'pass)
                                                                                                                                                                          sample6 (inst 'test-results 'status 'fail)
                                                                                                                                                                          sample7 (inst 'test-results 'status 'fail))))))))))
              (test 6-multiple-nested (assert-same 6
                                                   (count-passes (inst 'suite-results
                                                                       'test-results (obj sample1 (inst 'test-results 'status 'pass))
                                                                       'nested-suite-results (obj nested1 (inst 'suite-results
                                                                                                                'test-results (obj sample2 (inst 'test-results 'status 'pass)
                                                                                                                                   sample3 (inst 'test-results 'status 'pass)
                                                                                                                                   sample4 (inst 'test-results 'status 'fail)))
                                                                                                  nested2 (inst 'suite-results
                                                                                                                'test-results
                                                                                                                (obj sample4 (inst 'test-results 'status 'pass)
                                                                                                                     sample5 (inst 'test-results 'status 'pass)
                                                                                                                     sample6 (inst 'test-results 'status 'pass)
                                                                                                                     sample7 (inst 'test-results 'status 'fail)))))))))



       (suite result-is-pass
              (test pass (assert-t (result-is-pass (inst 'test-results
                                                         'status 'pass))))
              (test fail (assert-nil (result-is-pass (inst 'test-results
                                                           'status 'fail)))))

       (suite to-readable-string
              (test strings-are-quoted (assert-same "\"string!\""
                                                    (to-readable-string "string!")))
              (test numbers-are-ok (assert-same "42"
                                                (to-readable-string 42)))
              (test lists-are-ok (assert-same "(1 (2 3) . 4)"
                                              (to-readable-string '(1 (2 3) . 4))))
              (test lists-containing-string (assert-same "(1 \"2\" 3)"
                                                         (to-readable-string '(1 "2" 3))))
              (test tables-containing-string (assert-same "(obj (\"3\" 4)(\"5\" 6)(0 14)(1 \"2\"))"
                                                          (to-readable-string (obj 1 "2" "3" 4 "5" 6 0 14)))))

       (suite names
              (suite make-full-name
                     (test regular (assert-same 'pants
                                                (make-full-name nil
                                                                'pants)))
                     (test nested (assert-same 'parent.child
                                               (make-full-name 'parent
                                                               'child)))
                     (test multi-nested (assert-same 'parent.child.grandchild
                                                     (make-full-name 'parent 'child 'grandchild))))

              (suite is-valid-name
                     (test periods-not-ok (assert-nil (is-valid-name "hi.there")))
                     (test no-period-is-ok (assert-t (is-valid-name "hi;there!mom:)"))))

              (suite get-suite-and-test-name
                     (test no-test-name (assert-same '(suite nil)
                                                     (get-suite-and-test-name 'suite)))
                     (test simple-test-name (assert-same '(suite test)
                                                         (get-suite-and-test-name 'suite.test)))
                     (test nested-suites (assert-same '(suite.nested test)
                                                      (get-suite-and-test-name 'suite.nested.test)))
                     (test deeply-nested-suites (assert-same '(suite.nested1.nested2.nested3 test)
                                                             (get-suite-and-test-name 'suite.nested1.nested2.nested3.test))))

              (suite get-name-fragments
                     (test simple (assert-same '(top-level)
                                               (get-name-fragments 'top-level)))
                     (test one-nested (assert-same '(top-level single)
                                                   (get-name-fragments 'top-level.single)))
                     (test two-nested (assert-same '(top-level nested-1 nested-2)
                                                   (get-name-fragments 'top-level.nested-1.nested-2))))

              (suite get-nesting-level
                     (test 0 (assert-same 0
                                          (get-nesting-level 'top-level-suite)))
                     (test 1 (assert-same 1
                                          (get-nesting-level 'top-level-suite.single-nested)))
                     (test 2 (assert-same 2
                                          (get-nesting-level 'top-level-suite.single-nested.again))))

              (suite filter-unique-names
                     (test empty (assert-nil (filter-unique-names '())))
                     (test one-thing (assert-same '(my-name) (filter-unique-names '(my-name))))
                     (test different-things (assert-same '(one two) (filter-unique-names '(one two))))
                     (test removes-duplicates (assert-same '(one) (filter-unique-names '(one one one one))))
                     (test removes-nested-duplicates (assert-same '(one) (filter-unique-names '(one one.two.three.four))))
                     (test removes-nonconsecutive-duplicates (assert-same '(one two) (filter-unique-names '(one two one two))))
                     (test leading-differs (assert-same '(hell hello) (filter-unique-names '(hell hello))))
                     (test keeps-more-general (assert-same '(top-scope) (filter-unique-names '(top-scope top-scope.nested))))
                     (test keeps-nested-names (assert-same '(a.b.c.d) (filter-unique-names '(a.b.c.d))))
                     (test many-items (assert-same '(math top.one top.three.nested top.two a)
                                                   (filter-unique-names '(math top.one a.b top.three.nested top.two a math.adding))))
                     (test multiple-removals (assert-same '(math xyzzy) (filter-unique-names '(math xyzzy math.adding xyzzy math))))
                     (test order-is-kept (assert-same '(z a y b) (filter-unique-names '(z a y b))))
                     (test first-duplicate-is-kept (assert-same '(z a) (filter-unique-names '(z a z))))))
       (suite ensure-bound
              (setup bound-symbol (let val (uniq)
                                       (eval `(ensure-bound ,val 3))
                                       val))
              (test binds-unbound-value (assert-t (bound `,bound-symbol)))
              (test binds-proper-value (assert-same 3 (eval `,bound-symbol)))
              (test doesnt-change-already-bound-value (eval `(ensure-bound ,bound-symbol 4)) (eval `(assert-same 3 ,bound-symbol))))

       (suite suite-has-content
              (test empty-suite (assert-nil (suite-has-content (inst 'suite))))
              (test only-nested-test (assert-t (suite-has-content (inst 'suite 'tests (obj nested-test (inst 'test))))))
              (test only-nested-suite (assert-t (suite-has-content (inst 'suite 'nested-suites (obj nested-suite (inst 'suite))))))
              (test nested-test-and-suite (assert-t (suite-has-content (inst 'suite 'nested-suites (obj nested-suite (inst 'suite)) 'tests (obj nested-test (inst 'test)))))))

       (suite remove-thing
              (setup empty-hash (obj)
                     single-suite (obj single (inst 'suite 'tests (obj nested (inst 'test))))
                     orig-single-suite (obj single (inst 'suite 'tests (obj nested (inst 'test))))
                     two-suites (obj first (inst 'suite 'tests (obj nested (inst 'test)))
                                     second (inst 'suite 'tests (obj nested (inst 'test))))
                     single-nested-suite (obj first (inst 'suite 'nested-suites (obj nested (inst 'suite))))
                     double-nested-suite (obj first (inst 'suite 'nested-suites (obj nested (inst 'suite) nested2 (inst 'suite))))
                     deep-nested-suite (obj top (inst 'suite 'nested-suites (obj middle (inst 'suite 'tests (obj bottom (inst 'test))))))
                     two-nested-tests (obj single (inst 'suite 'nested-suites (obj double (inst 'suite 'tests (obj one (inst 'test) two (inst 'test)))))))
              (test remove-from-empty (assert-t (empty (remove-thing '(whatever) empty-hash))))
              (test remove-from-single
                    (remove-thing '(single) single-suite)
                    (assert-t (empty single-suite)))
              (test remove-nothing-from-single
                    (remove-thing '(not-found) single-suite)
                    (assert-same (keys orig-single-suite)
                                 (keys single-suite)))
              (test remove-one-of-two-suites
                    (remove-thing '(first) two-suites)
                    (assert-same '(second)
                                 (keys two-suites)))
              (test remove-nested-suite
                    (remove-thing '(first nested)
                                  double-nested-suite)
                    (assert-same '(nested2)
                                 (keys double-nested-suite!first!nested-suites)))
              (test remove-nested-test
                    (remove-thing '(single double one)
                                  two-nested-tests)
                    (assert-same '(two)
                                 (keys two-nested-tests!single!nested-suites!double!tests)))
              (test removes-empty-suites (assert-same (obj)
                                                      (remove-thing '(single nested)
                                                                    single-suite)))
              (test nested-empty-suites-removed
                    (assert-same (obj)
                                 (remove-thing '(top middle bottom)
                                               deep-nested-suite))))

       (suite print-suite-run-summary
              (test empty-top-level (assert-same "There are no tests directly in suite empty-suite.\n"
                                                 (tostring (print-suite-run-summary (inst 'suite-result 'suite-name 'empty-suite 'test-results (obj) 'nested-suite-results (obj))))))
              (test single-passing-test
                    (assert-same "Suite single-passing-test: the single test passed!\n"
                                 (tostring (print-suite-run-summary (inst 'suite-results 'suite-name 'single-passing-test 'test-results (obj foo (inst 'test-result 'status 'pass)))))))
              (test single-failing-test
                    (assert-same "Suite single-failing-test:\nsingle-failing-suite.foo failed: fail message here.\trerun this test: (this is failing code)\nIn suite single-failing-test, 0 of 1 tests passed.\n"
                                 (tostring (print-suite-run-summary (inst 'suite-results 'suite-name 'single-failing-test 'test-results (obj foo (inst 'test-result 'test-name 'foo 'suite-name 'single-failing-suite 'status 'fail 'code '(this is failing code) 'details "fail message here")))))))
              (test two-passing-tests
                    (assert-same "Suite two-passing: all 2 tests passed!\n"
                                 (tostring (print-suite-run-summary (inst 'suite-results 'suite-name 'two-passing 'test-results (obj foo (inst 'test-result 'status 'pass) bar (inst 'test-result 'status 'pass)))))))
              (test empty-second-level (assert-same "    There are no tests directly in suite empty.suite.\n"
                                                    (tostring (print-suite-run-summary (inst 'suite-result 'suite-name 'empty.suite 'test-results (obj) 'nested-suite-results (obj))))))
              ;;zck add more tests before refactoring
              )
       (suite pretty-results
              (test passing (assert-same "s.t passed!\n"
                                         (tostring (pretty-results (inst 'test-result 'suite-name 's 'test-name 't 'full-test-name 's.t 'status 'pass) t))))
              (test failing (assert-same "s.t failed: everything hurts.\trerun this test: (huh?)\n"
                                         (tostring (pretty-results (inst 'test-result 'suite-name 's 'test-name 't 'full-test-name 's.t 'status 'fail 'details "everything hurts" 'code '(huh?))))))))

;;minitest in ruby
