; tests based on https://bitbucket.org/zck/unit-test.arc
(map load:string '(
    lib/app.arc.t
    lib/ns.arc.t
    lib/queue.arc.t
    lib/strings.arc.t
    lib/streams.arc.t
    lib/tests/core-lists-test.arc
    arc.arc.t
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
