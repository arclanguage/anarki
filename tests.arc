; first hg clone https://bitbucket.org/zck/unit-test.arc
(load "unit-test.arc/unit-test.arc")

; tests currently rely on atstrings
; TODO: minimally turn on atstrings once unit-test.arc has support for suite
; setup/teardown functions
(declare 'atstrings t)

(map load:string '(
    arc.arc.t
    lib/app.arc.t
    lib/json.arc.t
    lib/lru-cache.arc.t
    lib/html.arc.t
    lib/ns.arc.t
    lib/queue.arc.t
    lib/spliceable-list.arc.t
    lib/strings.arc.t
    lib/streams.arc.t
    lib/ppr.arc.t
    lib/srv.arc.t
    lib/tem.arc.t
    lib/client.arc.t

    ; tests from conanite's rainbow
    lib/tests/core-lists-test.arc
    lib/tests/core-maths-test.arc
    lib/tests/core-errors-continuations-test.arc
    lib/tests/core-evaluation-test.arc
    lib/tests/core-macros-test.arc
    lib/tests/core-predicates-test.arc
    lib/tests/core-special-forms-test.arc
    lib/tests/core-typing-test.arc
))
(test-and-error-on-failure)

; check examples
(prn "checking examples interspersed in the codebase")
(each (name examples-and-expected) examples*
  (each (example expected) pair.examples-and-expected
    (when (and (~is expected '_)
               (~iso eval.example expected)
               (~and (caris expected 'valueof)
                     (iso eval.example (eval cadr.expected))))
      (prn "error in example for " name ": " example))))

; since Arc has no modules we have to turn off global settings turned on just
; in this file
(declare 'atstrings nil)
