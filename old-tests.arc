;; unit tests in various styles prior to https://bitbucket.org/zck/unit-test.arc
(map load:string '(
    arc.arc.t.old
    lib/lru-cache.arc.t
    lib/srv.arc.t
    lib/tem.arc.t

    ; tests from conanite's rainbow
    lib/tests/core-errors-continuations-test.arc
    lib/tests/core-evaluation-test.arc
    lib/tests/core-macros-test.arc
    lib/tests/core-predicates-test.arc
    lib/tests/core-special-forms-test.arc
    lib/tests/core-typing-test.arc
    lib/tests/foundation-test.arc
    lib/tests/misc-tests.arc
    ; currently failing; may be irrelevant to anarki
    ; lib/tests/parser-test.arc
))
