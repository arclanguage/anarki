; tests based on https://bitbucket.org/zck/unit-test.arc
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
    lib/srv.arc.t
    lib/tem.arc.t
    lib/tests/core-lists-test.arc
    lib/tests/core-maths-test.arc
    lib/web.arc.t

    ; tests from conanite's rainbow
    lib/tests/core-errors-continuations-test.arc
    lib/tests/core-evaluation-test.arc
    lib/tests/core-macros-test.arc
    lib/tests/core-predicates-test.arc
    lib/tests/core-special-forms-test.arc
    lib/tests/core-typing-test.arc
))

; check examples
(prn "checking examples interspersed in the codebase")
(each (name examples-and-expected) examples*
  (each (example expected) pair.examples-and-expected
    (if (and (~is expected '_)
             (~iso eval.example expected)
             (~and (caris expected 'valueof)
                   (iso eval.example (eval cadr.expected))))
      (prn "error in example for " name ": " example))))
