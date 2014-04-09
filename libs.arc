(map load:string '(
    lib/require.arc
    lib/strings.arc
    lib/queue.arc
    lib/spliceable-list.arc
    lib/tem.arc
    lib/tree.arc
    lib/lru-cache.arc
    lib/util.arc
    lib/declare.arc
    lib/make-br-fn.arc

    ; webserver stuff
    lib/html.arc
    lib/srv.arc
    lib/app.arc
    lib/news.arc

    ; helpers for the repl
    lib/test.arc
    lib/ppr.arc
    lib/help.arc
))
