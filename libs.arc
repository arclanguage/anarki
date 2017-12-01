(map load:string '(
    lib/require.arc
    lib/strings.arc
    lib/queue.arc
    lib/spliceable-list.arc
    lib/tem.arc
    lib/tree.arc
    lib/streams.arc
    lib/lru-cache.arc
    lib/util.arc
    lib/declare.arc
    lib/make-br-fn.arc

    ; webserver stuff
    lib/html.arc
    lib/srv.arc
    lib/app.arc
    lib/client.arc
    lib/news.arc

    ; addittilal web apps
    lib/search.arc
    lib/blog.arc
    lib/events.arc
    lib/prompt.arc

    ; speed improvements
    lib/boyer-moore.arc

    ; testing
    lib/form-tests.arc

    ; helpers for the repl
    lib/ppr.arc
    lib/help.arc
))
