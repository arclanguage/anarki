#lang racket

(require (only-in racket/runtime-path define-runtime-path))
(require "ac.rkt")
(require (only-in "brackets.rkt" use-bracket-readtable))

(define-runtime-path arc-arc-path "arc.arc")
(define-runtime-path libs-arc-path "libs.arc")


; Loading Anarki is expensive, so we do it on demand, and the
; (anarki-init) function is dedicated to doing so. By using Racket's
; promises, we pay the initialization cost only once even though a
; client of the Racket `anarki` module may call `anarki-init` multiple
; times.

(define anarki-init-promise
  (delay
    (use-bracket-readtable)
    (parameterize ([current-directory (path-only arc-arc-path)])
      (aload arc-arc-path)
      (aload libs-arc-path))
    (void)))

(define (anarki-init)
  (force anarki-init-promise))


(define (anarki-init-verbose)
  (parameterize ([current-output-port (current-error-port)])
    (displayln "initializing arc.. (may take a minute)"))
  (anarki-init))


(provide (all-from-out racket))
(provide (all-from-out "ac.rkt"))
(provide (all-defined-out))
