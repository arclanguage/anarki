;; tests based on https://bitbucket.org/zck/unit-test.arc
(map load:string '(
    lib/app.arc.t
    lib/queue.arc.t
    lib/strings.arc.t
    lib/streams.arc.t
    lib/tests/core-lists-test.arc
    arc.arc.t
))
