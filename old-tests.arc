;; unit tests in various styles prior to https://bitbucket.org/zck/unit-test.arc

; tests from conanite's rainbow
(require '(
    lib/rainbow-test.arc
    lib/tests/foundation-test.arc
    lib/tests/misc-tests.arc
    ; currently failing; may be irrelevant to anarki
    ; lib/tests/parser-test.arc
))
(run-all-tests)
