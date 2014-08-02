;; unit tests in various styles prior to https://bitbucket.org/zck/unit-test.arc

; tests from conanite's rainbow
(load "lib/rainbow-test.arc")
(map load:string '(
    lib/tests/foundation-test.arc
    lib/tests/misc-tests.arc
    ; currently failing; may be irrelevant to anarki
    ; lib/tests/parser-test.arc
))
(run-all-tests)
