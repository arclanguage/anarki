#lang racket

(require racket/runtime-path)

(define-runtime-path boot-scm-path "boot.scm")

(define anarki-path (path-only boot-scm-path))

; Loading Anarki is expensive, so we do it on demand, and the
; (anarki-init) function is dedicated to doing so. Fortunately, we pay
; the initialization cost only once even though we're running
; `dynamic-require` multiple times.

(define (anarki-init)
  ; We load the export, but then we ignore it. We could use any export
  ; for this.
  (dynamic-require boot-scm-path 'tl)
  (void))

(define-syntax provide-functions-from-anarki
  (syntax-rules ()
    [(_ [internal-name external-name] ...)
      (begin (begin (define (external-name . args)
                      (apply (dynamic-require boot-scm-path
                               'internal-name)
                        args))
                    (provide external-name))
             ...)]))

(provide-functions-from-anarki
  [tl anarki-repl]
  [aload anarki-load]
  [arc-eval anarki-eval])

; launch anarki repl from the anarki package folder
(define (anarki)
  (parameterize ([current-directory anarki-path])
    (anarki-repl)))

(provide anarki-path anarki-init anarki)
