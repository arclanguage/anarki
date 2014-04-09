;; unit tests in various styles prior to https://bitbucket.org/zck/unit-test.arc
(map load:string '(
    arc.arc.t
    lib/app.arc.t
    lib/json.arc.t
    lib/lru-cache.arc.t
    lib/ns.arc.t
    lib/queue.arc.t
    lib/spliceable-list.arc.t
    lib/srv.arc.t
    lib/strings.arc.t
    lib/tem.arc.t
    lib/web.arc.t
))
