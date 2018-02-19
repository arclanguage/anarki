#lang racket

; This is the primary interface to Anarki as a Racket library. It can
; be accessed like so:
;
;   $ raco pkg install anarki
;   $ racket
;   > (require anarki)
;   > (anarki)
;   arc>


(require anarki/boot)

(provide
  anarki-init
  anarki-init-in-main-namespace
  anarki-init-verbose
  (rename-out
    [arc-arc-path anarki-path]
    [main-namespace anarki-main-namespace]
    [tl anarki-repl]
    [aload anarki-load]
    [arc-eval anarki-eval])
  anarki)


; launch anarki repl from the anarki package folder
(define (anarki)
  (parameterize ([current-directory arc-arc-path])
    (anarki-init-verbose)
    (tl)))
