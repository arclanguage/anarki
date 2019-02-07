(require 'lib/unit-test.arc/unit-test.arc)

; ensure that all News libs are loaded
(require 'apps/news/news.arc)

; tests currently rely on atstrings
; TODO: minimally turn on atstrings once unit-test.arc has support for suite
; setup/teardown functions
(declare 'atstrings t)

(require '(
    arc.arc.t
    lib/app.arc.t
    lib/json.arc.t
    lib/lru-cache.arc.t
    lib/html.arc.t
    lib/ns.arc.t
    lib/queue.arc.t
    lib/re.arc.t
    lib/spliceable-list.arc.t
    lib/strings.arc.t
    lib/streams.arc.t
    lib/ppr.arc.t
    lib/srv.arc.t
    lib/tem.arc.t
    lib/client.arc.t
    lib/collect.arc.t

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

; since Arc has no modules we have to turn off global settings turned on just
; in this section
(declare 'atstrings nil)


; check examples
(prn "Checking examples interspersed in the codebase...")
(let failed nil
  (each (name examples-and-expected) examples*
    (each (example expected) (pair examples-and-expected)
      (unless (if (is expected '_)
                t
                  (caris expected 'valueof)
                ; We copy to protect against macros mutating their arguments:
                ; https://github.com/arclanguage/anarki/pull/148#issuecomment-459923195
                (iso (eval:copy example) (eval:copy expected.1))
                (iso (eval:copy example) expected))
        (prn "Error in example for " name ": " (tostring:write example))
        (= failed t))))
  (if failed
    (err "Failure in examples; aborting")
    (prn "Example checking complete.")))
