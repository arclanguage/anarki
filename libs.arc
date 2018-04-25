(load "lib/require.arc")

(require '(
    ; core
    lib/strings.arc
    lib/re.arc
    lib/binary.arc
    lib/queue.arc
    lib/tem.arc
    lib/declare.arc
    lib/make-br-fn.arc
    lib/collect.arc

;   ; optional
;   lib/files.arc
;   lib/streams.arc
;   lib/lru-cache.arc
;   lib/tree.arc
;   lib/util.arc
;   lib/spliceable-list.arc
;   lib/client.arc
;   lib/pd.arc

    ; web
    lib/html.arc
    lib/srv.arc
    lib/app.arc
    lib/prompt.arc

    ; helpers for the repl
    lib/ppr.arc
    lib/help.arc
))
