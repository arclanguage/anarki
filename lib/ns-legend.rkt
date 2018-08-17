#lang racket/base

; This module is an implementation detail of ns.arc.

; For the modules ns.arc generates at run time, we use the module
; `(submod "lib/ns-bare-bones.rkt" bare-bones)` as their initial
; bindings. It's a module which exports nothing but the bindings we
; need for evaluating expressions in the module, and where all the
; bindings we need have been exported as gensyms so as not to conflict
; with names chosen by the user of ns.arc, That is, the user can put
; top-level bindings of symbols like `define` or `#%app` into the
; module's namespace without interfering with the bindings we need for
; ns.arc to work.
;
; We only need a few things in particular to be exported under gensyms
; in the `bare-bones` submodule. Namely, it re-exports several
; essential Racket language primitives, plus a syntax of our own
; called `define-customvar`. The `define-customvar` syntax is good for
; defining module exports that appear to have custom getters and
; setters. There's also a "legend" table it exports, as we'll describe
; in a moment.
;
;
; The ns-bare-bones.rkt module does the metaprogramming necessary to
; define all the gensym-named exports of that `bare-bones` submodule.
; This module, ns-legend.rkt, provides variables whose *values* are
; all those gensyms. That way ns.arc can access these non-gensym
; exports in order to look up the gensym names it needs. The variables
; containing gensyms all have names beginning with "bare-bones--".
;
; We take a bit of a circuitous route to supply the gensyms from this
; module. Racket has support for marshalling gensyms into compiled
; code; however, we don't rely on its support (if any) for referring
; to the *same* gensym in multiple compiled modules. Instead, we only
; let the gensyms be marshalled in the `bare-bones` submodule itself.
;
; We have this module dynamically load the `bare-bones` submodule and
; snoop around in its (gensym-named) exports to find the one export
; that carries a table of all the gensyms. We call that the "legend."
;
; NOTE: The snooping turns out to be trivial, since there's only one
; non-syntax export from the submodule. If we ever add another export
; to the submodule, we'll need to figure out which one is the legend
; somehow, perhaps by testing it with `hash?` and checking that all
; its keys are interned symbols and all its values are gensyms.



(require (for-syntax racket/base))

(require (only-in racket/runtime-path define-runtime-path))

(provide
  bare-bones--app
  bare-bones--datum
  bare-bones--define-customvar
  bare-bones--plain-module-begin
  bare-bones--provide
  bare-bones--require)


(define-runtime-path ns-bare-bones-rkt-path "ns-bare-bones.rkt")


(define gensym-legend
  (let ([submodule-path
         (list 'submod ns-bare-bones-rkt-path 'bare-bones)])
    
    ; We cause the submodule to be declared, but not yet visited or
    ; instantiated, in the current namespace.
    (dynamic-require submodule-path #f)
    
    ; We look up the symbols exported by the declared module.
    (define-values (exported-values exported-syntaxes)
      (module->exports submodule-path))
    
    ; There is only one exported value, namely the legend. We take the
    ; symbol corresponding to this export, and we look it up with
    ; another call to `dynamic-require`. This causes the submodule to
    ; be visited and instantiated in order to compute that exported
    ; value.
    (dynamic-require submodule-path (caadar exported-values))))

(define-syntax (define-gensym stx)
  (syntax-case stx ()
    [ (_ racket-name provided-name)
      #'(define provided-name
          (hash-ref gensym-legend 'provided-name))]))


(define-gensym #%app bare-bones--app)
(define-gensym #%datum bare-bones--datum)
(define-gensym define-customvar bare-bones--define-customvar)
(define-gensym #%plain-module-begin bare-bones--plain-module-begin)
(define-gensym #%provide bare-bones--provide)
(define-gensym #%require bare-bones--require)
