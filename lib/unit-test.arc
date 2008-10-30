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

(set show-failed-only t)
(set all-tests nil)

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
	(with (expected (test 2) result (on-err [+ "Error thrown: " (details _)] (fn () (eval (cadr test)))))
	      (if (iso result expected)
	          (do (if (is show-failed-only nil) (prn desc " - " (car test) " - ok")) (++ results!passed))
	          (do (prn desc " - " (car test) " - FAILED: expected " expected ", got " result) (++ results!failed)))))

(def execute-tests (desc tests results)
  (execute-test desc (car tests) results)
  (if (cdr tests) (execute-tests desc (cdr tests) results)))
