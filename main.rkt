#lang racket

(require racket/runtime-path)

(require (only-in "boot.rkt" aload anarki-init arc-eval tl))

(define-runtime-path boot-rkt-path "boot.rkt")

(define anarki-path (path-only boot-rkt-path))

(define-syntax provide-functions-from-anarki
  (syntax-rules ()
    [(_ [internal-name external-name] ...)
      (begin (begin (define (external-name . args)
                      (anarki-init)
                      (apply internal-name args))
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
