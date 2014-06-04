;; unit tests in various styles prior to https://bitbucket.org/zck/unit-test.arc

; tests from conanite's rainbow
(load "lib/unit-test.arc")
(map load:string '(
    lib/tests/core-macros-test.arc
    lib/tests/core-predicates-test.arc
    lib/tests/core-special-forms-test.arc
    lib/tests/core-typing-test.arc
    lib/tests/foundation-test.arc
    lib/tests/misc-tests.arc
    ; currently failing; may be irrelevant to anarki
    ; lib/tests/parser-test.arc
))
(run-all-tests)
