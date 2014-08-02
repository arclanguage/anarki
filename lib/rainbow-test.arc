; This software is copyright (c) Conan Dalton 2008. Permission to use
; it is granted under the Perl Foundations's Artistic License 2.0.

;; to run a test,
;;
;; arc>(run-tests '("addition" (+ 2 3) 5))
;;
;; a test is either a single test or a test suite
;;
;; a single test has this form:
;; ("name" expr expected-value)
;;
;; for example:
;;
;; ("addition" (+ 2 3) 5)
;;
;; a suite has this form:
;; (suite "name" test test)
;;
;; suites may be nested
;;
;; you can also register your tests or suites thus:
;;
;; (register-test '("silly test" (list 'a 'b) (a b)))
;;
;; all registered tests will be run by
;; arc>(run-all-tests)
;;

(assign show-failed-only t)
(assign all-tests nil)

(def register-test (test)
  "register a test to be run later
   by 'run-all-tests"
  (push test all-tests))

(def run-all-tests ()
  "runs all tests that have been registered
   with 'register-test"
  (let results (obj passed 0 failed 0)
    (run-tests `(suite "all tests" ,@all-tests) results)
    (prn "passed: " results!passed)
    (prn "failed: " results!failed)
    (/ results!passed (+ results!passed results!failed))))

(def run-tests (tests (o results (obj passed 0 failed 0)))
  "executes the given tests. 'results is a hash with
   keys 'passed and 'failed"
  (execute-test "" tests results)
  results)

(def execute-test (desc test results)
  (if (is 'suite (car test))
      (execute-tests (+ desc " - " (cadr test)) (cddr test) results)
      (execute-single-test desc test results)))

(def execute-single-test (desc test results)
  (prn desc)
	(with (expected (test 2) result (on-err [+ "Error thrown: " (details _)] (fn () (eval (cadr test)))))
	      (if (iso result expected)
	          (do (if (is show-failed-only nil)
	                  (prn desc " - " (car test) " - ok"))
	              (++ results!passed))
	          (do (prn desc " - " (car test) " - FAILED:\n   expected " expected ",\n   got " result "\n")
	              (++ results!failed)))))

(def execute-tests (desc tests results)
  (execute-test desc (car tests) results)
  (if (cdr tests) (execute-tests desc (cdr tests) results)))

(def rat ()
  (assign all-tests nil)
  (let loader [do prn._ (load (string _ ".arc"))]
    (map loader '(lib/tests/foundation-test
                  lib/tests/misc-tests
                  lib/tests/core-errors-continuations-test
                  lib/tests/core-evaluation-test
                  lib/tests/core-lists-test
                  lib/tests/core-macros-test
                  lib/tests/core-maths-test
                  lib/tests/core-predicates-test
                  lib/tests/core-special-forms-test
                  lib/tests/core-typing-test
;?                   lib/tests/parser-test
                  ))
    (if (bound 'java-new)
        (map loader '(rainbow/tests/anarki-compatibility-test
                      rainbow/tests/extra-math-test
                      rainbow/tests/chained-ssexpand-test
                      rainbow/tests/string-interpolation-test
                      rainbow/tests/java-interface-test
                      rainbow/tests/misc-extra-test
                      ))))
  (run-all-tests))
