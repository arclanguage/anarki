#lang racket/base

; This module is an implementation detail of ns.arc.

; For the modules ns.arc generates at run time, we use this module as
; their initial bindings.
;
; We only need a few things in particular. Namely, we re-export
; everything from `racket/base`, plus a syntax of our own called
; `define-customvar`. The `define-customvar` syntax is good for
; defining module exports that appear to have custom getters and
; setters.


(require (for-syntax racket/base))

(provide (all-from-out racket/base))
(provide define-customvar)

(define-syntax-rule (define-customvar var getter setter)
  (define-syntax var
    (make-set!-transformer
      (lambda (stx)
        (syntax-protect
          (syntax-case stx (set!)
            (id (identifier? #'id) #'(getter))
            ((set! _ val) #'(setter val))))))))
