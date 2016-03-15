#lang racket

(require racket/runtime-path)

(define-runtime-path boot-scm-path "boot.scm")

(define anarki-path (path-only boot-scm-path))

; Loading Anarki is expensive, so we do it on demand, and the
; (anarki-init) function is dedicated to doing so. Fortunately, we pay
; the initialization cost only once even though we're loading
; `dynamic-require` multiple times.

(define (anarki-init)
  ; We load the export, but then we ignore it. We could use any export
  ; for this.
  (dynamic-require boot-scm-path 'tl)
  (void))

(define-values
  (anarki-repl anarki-load anarki-namespace anarki-global-name)
  (apply values
    (map (lambda (var)
           (lambda args
             (apply (dynamic-require boot-scm-path var) args)))
      '(tl aload main-namespace ac-global-name))))

(provide (except-out (all-defined-out) boot-scm-path))

; launch anarki repl from the anarki package folder
(define (anarki)
  (parameterize ((current-directory anarki-path)) (anarki-repl)))

