#lang racket/base

(require racket/runtime-path)

(define-runtime-path boot-scm-path "boot.scm")

; Loading Anarki is expensive, so we do it on demand, and the
; (anarki-init) function is dedicated to this. Fortunately, we pay the
; initialization cost only once even though we're loading
; `dynamic-require` multiple times.

(define (anarki-init)
  ; We load the export, but then we ignore it. We could use any export
  ; for this.
  (dynamic-require boot-scm-path 'tl)
  (void))

(define-values (anarki-repl anarki-load anarki-namespace)
  (apply values
    (map (lambda (var)
           (lambda args
             (apply (dynamic-require boot-scm-path var) args)))
      '(tl aload main-namespace))))

(provide (all-defined-out))
