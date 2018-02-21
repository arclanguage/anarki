#lang racket/base

; This module is an implementation detail of ns.arc.

; For the modules ns.arc generates at run time, we use the module
; `(submod "lib/ns.rkt" bare-bones)`, defined here, as their initial
; bindings. It's a module which exports nothing but the bindings we
; need for evaluating expressions in the module, and where all the
; bindings we need have been exported as gensyms so as not to conflict
; with names chosen by the user of ns.arc, That is, the user can use
; symbols like `define` or `#%app` without interfering with the
; bindings we need for ns.arc to work.

; This module does the metaprogramming necessary to define that
; `bare-bones` submodule, and it also provides variables whose values
; are all the gensyms that ns.arc needs in order to access the
; gensym-named exports. The variables containing those gensyms all
; have names beginning with "bare-bones--".
;
; The `bare-bones` submodule re-exports several essential Racket
; language primitives, plus a syntax of our own called
; `define-customvar` (well, not actually called `define-customvar`,
; but called some ineffable name that itself is called
; `bare-bones--define-customvar`). The `define-customvar` syntax is
; good for defining module exports that appear to have custom getters
; and setters.


(require (for-syntax racket/base))

(provide
  bare-bones--app
  bare-bones--datum
  bare-bones--define-customvar
  bare-bones--plain-module-begin
  bare-bones--provide
  bare-bones--require)
; This also "provides" the module (submod "ns.rkt" bare-bones).


(define-for-syntax bare-bones-exports (list))

; This macro has side effects when it expands.
(define-syntax (define-gensym stx)
  (syntax-case stx ()
    [ (_ racket-name provided-name)
      (let ([g (gensym (syntax-e #'racket-name))])
        (set! bare-bones-exports
          (cons #`(racket-name #,g)
            bare-bones-exports))
        #`(define provided-name '#,g))]))

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
      (provide (rename-out #,@bare-bones-exports))))

(module-bare-bones)
