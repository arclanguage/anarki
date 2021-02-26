;;; unit-test.arc --- Simply write and run tests.

;; Version: 1.0
;; This file is part of unit-test.arc .

;; Copyright 2013-2017 Zachary Kanfer <zkanfer@gmail.com>

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



(deftem suite
  suite-name 'suite-with-no-name
  tests (obj)
  nested-suites (obj)
  full-suite-name 'suite-with-no-full-name)

(mac suite args
     `(summarize-suite-creation (make-and-save-suite (suite ,@args))))

(def summarize-suite-creation (cur-suite)
     (summarize-single-suite cur-suite)
     (each (suite-name nested-suite) cur-suite!nested-suites
           (summarize-suite-creation nested-suite)))

(def summarize-single-suite (cur-suite)
     (let padding (newstring (* 4
                                (get-nesting-level cur-suite!full-suite-name))
                             #\space)
          (prn padding
               cur-suite!suite-name
               ":  \t"
               (plural (len cur-suite!tests) "test")
               ",  \t"
               (plural (len cur-suite!nested-suites) "nested suite")
               ".")))

(mac ensure-bound (place default)
     `(unless (bound ',place)
        (= ,place ,default)))

(ensure-bound *unit-tests* (obj))

;;full suite name -> suite results template
(ensure-bound *suite-results* (obj))

;;full test name -> test results template
(ensure-bound *test-results* (obj))

(def store-test-result (result)
     (= (*test-results*
         (make-full-name result!suite-name
                         result!test-name))
        result))

(def get-test-result (name)
     *test-results*.name)

(def store-suite-result (result)
     (= (*suite-results* result!suite-name)
        result))

(def get-suite-result (name)
     *suite-results*.name)

(def make-full-name args
     (sym (string (intersperse #\.
                               (keep idfn ;;for when called with nil, as in make-and-save-suite of top-level suites
                                     args)))))

(mac make-and-save-suite (suite-body)
     `(let this-suite (make-suite nil ,suite-body)
           (= (*unit-tests* this-suite!full-suite-name)
              this-suite)))

(mac make-suite (parent-suite-full-name suite-sexp)
     "Makes a suite.

      PARENT-SUITE-FULL-NAME is the full name of the parent suite, or nil. Don't quote it.
      FULL-SUITE is the full s-exp for the suite; something like (suite my-name (test...) ...). "
     (w/uniq (suite-name cur-suite setup)
             (withs (cur-suite-name (cadr suite-sexp)
                     full-suite-name (make-full-name parent-suite-full-name cur-suite-name)
                     suite-error-string (get-suite-structure-problems suite-sexp))
                    (if suite-error-string
                        `(err ,suite-error-string)
                      `(let ,suite-name ',(cadr suite-sexp)
                                (if (no (is-valid-name ,suite-name))
                                    (err (string "Suite names can't have periods in them. "
                                                 ,suite-name
                                                 " does."))
                                  (add-suites-to-suite ,full-suite-name
                                                       (add-tests-to-suite (inst 'suite
                                                                                 'suite-name ,suite-name
                                                                                 'full-suite-name ',(make-full-name parent-suite-full-name cur-suite-name))
                                                                           ,(make-full-name parent-suite-full-name cur-suite-name)
                                                                           ;;zck should this error if there's more than one setup clause?
                                                                           ,(cdr (car (keep-by-car suite-sexp 'setup)))
                                                                           ,(keep-by-car suite-sexp 'test))
                                                       ,(keep-by-car suite-sexp 'suite))))))))

(mac add-tests-to-suite (cur-suite-sexp full-suite-name setup tests-sexps)
     "Add tests to CUR-SUITE-SEXP, with setup SETUP. Tests come from TESTS-SEXPS.

SETUP is a list of var val pairs, like (x 1) or (age 31 height 70).
Don't quote FULL-SUITE-NAME."
     (if (no tests-sexps)
         cur-suite-sexp
       (w/uniq (cur-suite test-name)
               `(withs (,cur-suite ,cur-suite-sexp
                        ,test-name ',((car tests-sexps) 1))
                       (when (would-shadow ,cur-suite ,test-name)
                         (err (string "In suite "
                                      ',full-suite-name
                                      ", there are two things named "
                                      ,test-name
                                      ".")))
                       (= ((,cur-suite 'tests) ,test-name)
                          (make-test ',full-suite-name
                                     ',((car tests-sexps) 1) ;;this is the same as ',test-name, but needs to be macroexpanded directly. For reasons!
                                     ,setup
                                     ,@(cddr (car tests-sexps)))) ;;zck we need to be able to call this, right?
                       (add-tests-to-suite ,cur-suite
                                           ,full-suite-name
                                           ,setup
                                           ,(cdr tests-sexps))))))

(mac add-suites-to-suite (full-suite-name cur-suite-sexp suites-sexps)
     "Add the first suite from SUITES-SEXPS to CUR-SUITE-SEXP.
FULL-SUITE-NAME is the unquoted name of the suite."
     (if (no suites-sexps)
         cur-suite-sexp
       (w/uniq (cur-suite nested-suite nested-suite-name)
               `(withs (,cur-suite ,cur-suite-sexp
                        ,nested-suite (make-suite
                                       ,full-suite-name
                                       ,(car suites-sexps))
                        ,nested-suite-name (,nested-suite 'suite-name))
                       (when (would-shadow ,cur-suite ,nested-suite-name);;zck quoting nested-suite-name is the problem
                         (err (string "In suite "
                                      ',full-suite-name
                                      ", there are two things named "
                                      ,nested-suite-name
                                      ".")))
                       (= ((,cur-suite 'nested-suites) ,nested-suite-name)
                          ,nested-suite)
                       (add-suites-to-suite ,full-suite-name
                                            ,cur-suite
                                            ,(cdr suites-sexps))))))

(def get-suite-structure-problems (suite-sexp)
     "Return the problems with the structure of SUITE-SEXP, as a string.

For example, a SUITE-SEXP of (suitex name (test a b)) should have an error about
beginning with the symbol 'suitex ."
     (let errors
       (keep idfn
             (cons (unless (caris suite-sexp 'suite)
                     (string "The first element of the suite should be the symbol 'suite. It is: "
                             (car suite-sexp)))
                   (cons (unless (isa suite-sexp.1 'sym)
                           (string "The second element of the suite should be the suite-name, a symbol. It is: "
                                   suite-sexp.1))
                         (let setup-clause (car (keep-by-car suite-sexp 'setup))
                              (cons (when (is 1
                                              (mod (len (cdr setup-clause))
                                                   2))
                                      (string "In a setup clause, all variables should have a value. This doesn't: "
                                              (to-readable-string setup-clause)))
                                    (map [unless (and (acons _)
                                                      (mem (car _)
                                                           '(suite test setup)))
                                         (string "Each element of a suite should begin with one of the symbols 'suite, 'test, or 'setup. This doesn't: "
                                                 (to-readable-string _)
                                                 ".")]
                                                 (cddr suite-sexp)))))))
       (when errors (apply string
                           (intersperse "  "
                                        errors)))))

(def would-shadow (cur-suite thing-name)
     "Returns t iff CUR-SUITE has a test or suite named THING-NAME."
     (or cur-suite!tests.thing-name
         cur-suite!nested-suites.thing-name))

(def keep-by-car (sexp identifier)
     "Gets each element of SEXP where that element is a list, and the car of that element is IDENTIFIER."
     (keep [and (alist _)
               (is (car _)
                   identifier)]
               sexp))

(deftem test
  test-name 'test-with-no-name
  suite-name 'test-with-no-suite-name
  test-fn (fn args (assert nil "You didn't give this test a body. So I'm making it fail.")))

(mac make-test (suite-name test-name setup . body)
     "Make a test for SUITE-NAME named TEST-NAME. Quote both of these.

The test should have SETUP and BODY."
     `(if (no (is-valid-name ,test-name))
          (err (string "Test names can't have periods in them. "
                       ,test-name
                       " does."))
        (inst 'test
              'suite-name ,suite-name
              'test-name ,test-name
              'test-fn (make-test-fn ,suite-name ,test-name ,setup ,@body))))

(mac make-test-fn (suite-name test-name setup . body)
     `(fn ()
          (on-err (fn (ex) (inst 'test-result
                                 'suite-name ,suite-name
                                 'test-name ,test-name
                                 'full-test-name (make-full-name ,suite-name ,test-name)
                                 'status 'fail
                                 'details (details ex)))
                  (fn ()
                      (eval '(withs ,setup
                                    (inst 'test-result
                                          'suite-name ,suite-name
                                          'test-name ,test-name
                                          'full-test-name (make-full-name ,suite-name ,test-name)
                                          'status 'pass
                                          'return-value (w/stdout (outstring) ,@body))))))))



(deftem test-result
  test-name 'test-results-with-no-test-name
  full-test-name 'test-results-with-no-suite-name.test-results-with-no-test-name
  suite-name 'test-results-with-no-suite-name
  status 'fail
  details "test results with no details"
  return-value nil)

(def pretty-results (test-result)
     "Print out a pretty summary of TEST-RESULT."
     (pr test-result!suite-name "." test-result!test-name " ")
     (if (is test-result!status 'pass)
         (prn  "passed!")
       (prn "failed: " test-result!details)))

(mac test names-list
     `(do-test ',names-list))

(def do-test (names)
     (if (no names)
         (do (run-all-tests)
             (summarize-run-of-all-tests))
       (let unique-names (filter-unique-names names)
            (do (run-specific-things unique-names t)
                (summarize-run unique-names)))))

(mac test-and-error-on-failure names
     `(do-test-and-error-on-failure ',names))

(def do-test-and-error-on-failure (names)
     "Run the tests in NAMES, as in do-test.

However, if there are any test failures, throw an error.
This is intended for use in scripts, where the exit code
from racket is needed to tell if all tests passed or not"
     (let (passes total) (do-test names)
          (unless (is passes total)
            (err "Not all tests passed."))))

(def retest ()
     "Rerun the last group of tests run."
     (do-test *last-things-run*))

(def filter-unique-names (names)
     "Gets the unique names from NAMES.

      If one name is a full prefix of another (only counting period-separated fragments),
      only include the prefix. So if the input is '(whatever.thing whatever), the output
      should be '(whatever)."
     (withs (names-fragments (map get-name-fragments
                                  (sort < (dedup names)))
             helper (afn (names) (if (len< names 2)
                                     names
                                   (with (first-name (names 0)
                                                     second-name (names 1)
                                                     other-names (nthcdr 2 names))
                                         (if (begins first-name second-name)
                                             (self (cons second-name other-names))
                                           (begins second-name first-name)
                                           (self (cons first-name other-names))
                                           (cons first-name
                                                 (self (cdr names)))))))
             unique-names (memtable (map [apply make-full-name _]
                                         (helper names-fragments))))
            (keep idfn
                  (map [when unique-names._
                             (wipe unique-names._)
                             _]
                       names))))


;; This should be either a list of names, or nil.
;; nil means the last thing run was all tests.
(ensure-bound *last-things-run* nil)


;;; functions dealing with symbol manipulation

(def get-name-fragments (name)
     "Take a full suite name NAME, and return the fragments of it as a list of symbols.
      For example, (get-name-fragments 'math.integers.subtracting)
      returns '(math integers subtracting).
      This function will also work for test names"
     (map sym
          (tokens (string name)
                  #\.)))

(def get-nesting-level (name)
     "Return how nested NAME is, where a top-level suite is level 0.
      Each period in the name increases the level by one."
     (count #\.
            (string name)))

(def get-suite-and-test-name (test-full-name)
     "Return (suite-name test-name), as a list."
     (withs (string-name (string test-full-name)
             pivot (last (positions #\. string-name)))
            (list (sym (cut string-name 0 pivot))
                  (when pivot (sym (cut string-name (+ 1 pivot)))))))

(def get-suite (name)
     "Get the suite with full name NAME out of *unit-tests*
      This method looks at nested suites; that is, for a NAME of math.adding,
      it gets the 'math suite out of *unit-tests*, then looks for a nested
      suite 'adding inside it, rather than looking for a suite named math.adding
      at the top level of *unit-tests*."
     (withs (fragments (get-name-fragments name)
             helper (afn (cur-suite leftover-fragments)
                         (aif (no leftover-fragments)
                              cur-suite
                              (cur-suite!nested-suites (car leftover-fragments))
                              (self it (cdr leftover-fragments)))))
            (aand fragments
                  (*unit-tests* (car fragments))
                  (helper it (cdr fragments)))))

(def get-test (name)
     "Get the test obj referred to by NAME, or nil if it isn't found."
     (let (suite-name test-name) (get-suite-and-test-name name)
       (aand (get-suite suite-name)
             it!tests.test-name)))

(def run-all-tests ()
     "Run all tests. Return t if any were found, nil if none were."
     (when (run-specific-things (get-all-top-level-suite-names) t)
       (= *last-things-run* nil)
       t))

(def get-all-top-level-suite-names ()
     "Get the names of all the top-level suites.

      A top-level suite is a suite that doesn't have a parent."
     (keep is-valid-name
           (keys *unit-tests*)))

(def run-specific-things (names (o store-result nil))
     "Run the things in names, then if there were any, store that in *last-things-run*.
      Return t if at least one of the names is found, nil otherwise."
     (when (run-these-things names store-result)
       (= *last-things-run* names)
       t))

(def run-these-things (names (o store-result nil))
     "Each name in NAMES can either be a suite or a test.
      If STORE-RESULT is t, store the result of each function in *test-results* or *suite-results*
      Return t if at least one of the names is found, nil otherwise."

     (let at-least-one-found nil
          (each name names
                (when (run-this-thing name store-result)
                  (= at-least-one-found t)))
          at-least-one-found))

(def run-this-thing (name (o store-result nil))
     "If NAME is a test or a suite, run it and return the template result.
      If NAME is not either, return nil."
     (aif (get-suite name)
          (run-suite-and-children it store-result)
          (get-test name)
          (run-test it store-result)
          nil))

(def summarize-run (names)
     "Summarize a given test run.
      That is, print out information about the overall status
      of a set of suites."
     (with (tests 0
            passes 0
            names-not-found nil)
           (each name names
                 (print-run-summary name)
                 (aif (get-suite-result name)
                      (do (++ tests (total-tests it))
                          (++ passes (count-passes it)))
                      (get-test-result name)
                      (do (++ tests)
                          (when (result-is-pass it)
                            (++ passes)))
                      (push name names-not-found)))
           (prn)
           (when names-not-found
             (prn "The following names were not found:")
             (each name names-not-found
                   (prn name))
             (prn))
           (if (is tests 0)
               (prn "We didn't find any tests. Odd...")
             (is passes tests)
             (if (is tests 1)
                 (prn "Yay! The single test passed!")
               (prn "Yay! All " tests " tests passed!"))
             (prn "Oh dear, " (- tests passes) " of " tests " failed."))
           (list passes tests)))

(def summarize-run-of-all-tests ()
     "Summarise the run of all tests."
     (summarize-run (get-all-top-level-suite-names)))


(def total-tests (suite-results)
     "Count the total number of tests in SUITE-RESULTS, a list of suite-results templates."
     (apply +
            (len suite-results!test-results)
            (map total-tests
                 (vals suite-results!nested-suite-results))))

(def count-passes (suite-results)
     "Count the total number of passed tests in SUITE-RESULTS, a list of suite-results templates."
     (apply +
            (count result-is-pass
                   (vals suite-results!test-results))
            (map count-passes
                 (vals suite-results!nested-suite-results))))

(def result-is-pass (test-result)
     "Return t if TEST-RESULT, a test-result template instance, represents a passed test."
     (is test-result!status
         'pass))

(def is-valid-name (name)
     "Return t if NAME, a symbol, is a valid name for a suite or test.

      Valid names contain any characters but periods."
     (no (find #\.
               (string name))))

(deftem suite-results
  suite-name 'suite-results-with-no-suite-name
  test-results (obj) ;;hash of test-name -> test-result
  nested-suite-results (obj)) ;;nested-suite-fullname -> suite-result


(def run-suite-and-children (cur-suite (o store-result nil))
     (let results (inst 'suite-results
                        'suite-name cur-suite!full-suite-name
                        'test-results (run-this-suite cur-suite))

          (each (nested-name nested-suite) cur-suite!nested-suites
                (= results!nested-suite-results.nested-name
                   (run-suite-and-children nested-suite)))

          (when store-result
            (store-suite-result results))

          results))

(def run-this-suite (cur-suite)
     (let test-results (obj)
          (each (test-name test-template) cur-suite!tests
                (= test-results.test-name
                   (test-template!test-fn)))
          test-results))

(def run-test (cur-test (o store-result nil))
     (let result (cur-test!test-fn)
          (when store-result
            (store-test-result result))
          result))

(def print-run-summary (name)
     "This should work on both suite and test names"
     (aif (get-suite-result name)
          (print-suite-run-summary it (get-nesting-level name))
          (get-test-result name)
          (print-test-run-summary it)))

(def print-test-run-summary (test-result)
     (prn "Test "
          test-result!full-test-name
          ": "
          (if (result-is-pass test-result)
              "passed!"
            "failed."))) ;;zck this doesn't use pretty-results, so printed message is different.
;;compare (print-run-summary 'unit-test-tests.suite-creation.add-tests-to-suite)
;;with (print-run-summary 'unit-test-tests.suite-creation.add-tests-to-suite.passing-test-passes)

(def print-suite-run-summary (suite-results-template (o nesting-dedent-level 0))
     (when suite-results-template
       (with (tests 0
              passed 0)
               (each (test-name test-result) suite-results-template!test-results
                     (++ tests)
                     (when (is test-result!status 'pass)
                       (++ passed)))
               (pr (newstring (* 4
                                 (- (count #\.
                                           (string suite-results-template!suite-name))
                                    nesting-dedent-level))
                              #\space))
               (if (is tests 0)
                   (prn "There are no tests directly in suite " suite-results-template!suite-name ".")
                 (is tests passed 1)
                 (prn "Suite " suite-results-template!suite-name ": the single test passed!")
                 (is tests passed)
                 (prn "Suite " suite-results-template!suite-name ": all " tests " tests passed!")
                 (do (prn "Suite " suite-results-template!suite-name ":")
                     (each (test-name test-result) suite-results-template!test-results
                           (pretty-results test-result))
                   (prn "In suite " suite-results-template!suite-name ", " passed " of " tests " tests passed."))))
       (each (nested-name nested-results) suite-results-template!nested-suite-results
             (print-suite-run-summary nested-results nesting-dedent-level))))

(mac assert (test fail-message)
     `(unless ,test
        (err ,fail-message)))

(mac assert-two-vals (test expected actual (o fail-message))
     (w/uniq (exp act)
             `(with (,exp ,expected
                          ,act ,actual)
                    (assert (,test ,exp ,act)
                            (string (to-readable-string ',actual)
                                    " should be "
                                    (to-readable-string ,exp)
                                    " but instead was "
                                    (to-readable-string ,act)
                                    (awhen ,fail-message
                                           (string ". " it)))))))

(def to-readable-string (val)
     "Return a readable version of VAL."
     ;; It is intended to be readable when printed.
     ;; This matters, for example, when dealing with strings.
     ;; The return value from this function isn't as readable as it is printed:

     ;; arc> (to-readable-string "hi")
     ;; "\"hi\""
     ;; arc> (prn (to-readable-string "hi"))
     ;; "hi"
     ;; "\"hi\""
     (if (isa val
              'string)
         (string #\" val #\")
       (acons val)
       (list-to-readable-string val)
       (isa val
            'table)
       (table-to-readable-string val)
       (tostring (disp val))))

(def list-to-readable-string (val)
     (string #\(
             (list-innards-to-readable-string val)
             #\)))

(def list-innards-to-readable-string (val)
     (when val
       (string
        (to-readable-string (car val))
        (when (cdr val)
          (if (isa (cdr val)
                   'cons)
              (string #\space
                      (list-innards-to-readable-string (cdr val)))
            (string " . "
                    (to-readable-string (cdr val))))))))

(def table-to-readable-string (tbl)
     (let sorted-table (sort < (map to-readable-string (tablist tbl)))
          (tostring (pr "(obj ")
                    (each ele sorted-table
                          (pr ele))
                    (pr ")"))))



(mac assert-same (expected actual (o fail-message))
     `(assert-two-vals same ,expected ,actual ,fail-message))

(mac assert-t (actual (o fail-message))
     `(assert-two-vals isnt nil ,actual ,fail-message))
;; We can't call (assert-two-vals is t ,actual) because we want to accept _any_ non-nil value, not just 't

(mac assert-nil (actual (o fail-message))
     `(assert-two-vals is nil ,actual ,fail-message))

(def same (thing1 thing2)
     (if (and (isa thing1
                   'table)
              (isa thing2
                   'table))
         (hash-same thing1 thing2)
       (iso thing1 thing2)))

(def hash-same (hash1 hash2)
     (and (is (len hash1)
              (len hash2)) ;;only need to check the length here; if the keys differ, we'll find it below
          (all idfn
               (map [same hash1._
                          hash2._]
                    (keys hash1)))))

(mac assert-error (actual (o expected-error))
     `(unless (on-err (fn (ex) (if ',expected-error
                                   (do (assert-same ,expected-error
                                                    (details ex))
                                       t)
                                 t))
                      (fn () ,actual
                             nil))
        (err "Expected an error to be thrown")))

(mac assert-no-error (actual)
     `(on-err (fn (exception)
                  (err (string "We got an error with details: "
                                 (details exception))
                         ))
              (fn () ,actual)))

(def list-suites ()
     "Prints out all suites that can be run."
     (prn "Here are all the suites that can be run.\nEach nested suite is indented under its parent.\n")
     (let helper (afn (cur-suite nesting-level)
                      (prn (newstring (* 8 nesting-level) #\space)
                           cur-suite!suite-name
                           ": "
                           (plural (len cur-suite!tests)
                                   "test"))
                      (each (child-name child-suite) cur-suite!nested-suites
                            (self child-suite
                                  (+ 1 nesting-level))))
          (each top-level-suite
                (keep [is-valid-name _!full-suite-name]
                      (vals *unit-tests*))
                (helper top-level-suite 0))))


(mac wipe-tests names
     "Delete the tests or suites from *unit-tests*, causing them to not exist anymore.

      If this deletion results in a suite with no tests and no nested suites, that suite
      will be removed also."
     `(wipe-tests-helper ',names))

(def wipe-all-tests ()
     "Delete all tests and suites."
     (= *unit-tests* (obj)))

(def wipe-tests-helper (name-list)
     "For each thing named in NAME-LIST, delete it from *unit-tests*."
     (each name name-list
           (remove-thing (get-name-fragments name)
                         *unit-tests*)))

(def remove-thing (name-fragments suites-obj)
     "Delete the thing referred to by NAME-FRAGMENTS from SUITES-OBJ.

      To do this, take each non-terminal element of NAME-FRAGMENTS,
      treat that symbol as a suite name, and look up that suite.

      Then it takes the last element, and deletes any suites or tests with that name. Finally,
      it will go back up the path and delete any empty suites.

      For example, it might get called as (remove-thing '(top nested1 nested2 last-thing)).
      It will remove a suite called top.nested1.nested2.last-thing, or a test named last-thing
      inside top.nested1.nested2."
     (let (first-name next-name . other-names) name-fragments
          (when first-name
            (if (no next-name)
                (wipe suites-obj.first-name)
              (let current-suite suites-obj.first-name
                   (when current-suite
                     (unless other-names
                       (wipe current-suite!tests.next-name)
                       (wipe current-suite!nested-suites.next-name))
                     (remove-thing (cons next-name
                                         other-names)
                                   current-suite!nested-suites)
                     (unless (suite-has-content current-suite)
                       (wipe suites-obj.first-name)))))))
     suites-obj)


(def suite-has-content (the-suite)
     "Return t if SUITE has either tests or nested suites."
     (or (no (empty the-suite!nested-suites))
         (no (empty the-suite!tests))))
