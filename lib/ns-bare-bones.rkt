#lang racket/base

; This module is an implementation detail of ns.arc.

; There are some extensive comments in ns-legend.rkt which describe
; the functionality of this module.


(require (for-syntax racket/base))

(require (only-in racket/runtime-path define-runtime-path))

; This module "provides" only the submodule
; (submod "ns-bare-bones.rkt" bare-bones).


(define-for-syntax bare-bones-exports
  (list
    (list 'gensym-legend-submodule
      (gensym 'gensym-legend-submodule))))
(define-for-syntax gensym-legend-phase-1 (hash))

; This macro has side effects when it expands.
(define-syntax (define-gensym stx)
  (syntax-case stx ()
    [ (_ racket-name provided-name)
      ; TODO: We're no longer using actual gensyms here because they
      ; weren't working with Racket 7.0, and it seems likely that
      ; gensym exports/imports are not a stable supported feature of
      ; Racket (https://github.com/racket/racket/issues/2133).
      ; Instead, we're using gensyms for the module's internal
      ; definitions in ns.arc's `make-simple-rmodule` and then
      ; renaming them for export. Now that this is our approach, let's
      ; get rid of the "bare-bones" approach altogether and just
      ; generate `racket/base` modules instead.
;      (let ([g (gensym (syntax-e #'racket-name))])
      (let ([g (syntax-e #'racket-name)])
        (set! bare-bones-exports
          (cons #`(racket-name #,g)
            bare-bones-exports))
        (set! gensym-legend-phase-1
          (hash-set gensym-legend-phase-1 (syntax-e #'provided-name)
            g))
        #'(begin))]))


(define-gensym #%app bare-bones--app)
(define-gensym #%datum bare-bones--datum)
(define-gensym define-customvar bare-bones--define-customvar)
(define-gensym #%plain-module-begin bare-bones--plain-module-begin)
(define-gensym #%provide bare-bones--provide)
(define-gensym #%require bare-bones--require)


(define-syntax (module-bare-bones stx)
  #`(module bare-bones racket/base
      (require (for-syntax racket/base))
      (define-syntax-rule (define-customvar var getter setter)
        (define-syntax var
          (make-set!-transformer
            (lambda (stx)
              (syntax-protect
                (syntax-case stx (set!)
                  (id (identifier? #'id) #'(getter))
                  ((set! _ val) #'(setter val))))))))
      (define gensym-legend-submodule '#,gensym-legend-phase-1)
      (provide (rename-out #,@bare-bones-exports))))

(module-bare-bones)
