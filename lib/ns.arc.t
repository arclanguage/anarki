; ns.arc.t
;
; Ross Angle (rocketnia) 2011


(require "lib/ns.arc")


(suite-w/setup namespace (foo (nsobj a 1 b 2))
               make-namespaces (assert-same '(a b)
                                            (sort < (keep $.symbol-interned? ns-keys.foo)))
               get-namespaces (assert-same 1
                                           foo!a)
               assigning-into-namespace (assert-same 2
                                                     (do (= foo!a 2)
                                                         foo!a)))

(suite-w/setup modecule (foo (nsobj i 9 j 10)
                         bar make-modecule.11)
               make-namespaces (assert-same '(i j)
                                            (sort < (keep $.symbol-interned? ns-keys.foo)))
               ns-set-modecule (assert-same bar
                                            (ns-set-modecule 'k bar foo))
               modecule-setting-works (assert-same '(i j k)
                                                   (do (ns-set-modecule 'k bar foo)
                                                       (sort < (keep $.symbol-interned? ns-keys.foo))))
               val-by-evaling-racket-code (assert-same 11
                                                       (do (ns-set-modecule 'k bar foo)
                                                           (w/current-ns foo ($.eval global-arcracket!k))))
               val-by-calling-namespace (assert-same 10
                                                     foo!j)
               set-val-with-= (assert-same 12
                                           (do (= foo!k 12) foo!k)))

(suite-w/setup module (foo (simple-mod a 1 b 2))
               ns-keys (assert-same '(a b)
                                    (sort < (keep $.symbol-interned? ns-keys.foo)))
               extract-from-module (assert-same 1
                                                foo!a)
               set-in-module (assert-same 9
                                          (do (= foo!a 9)
                                              foo!a)))
(suite-w/setup submodule (foo (simple-rmod t 1 nil 2)
                          bar (make-sub-rmodule foo idfn))
               get-keys (assert-same '(nil t)
                                     (sort < (keep $.symbol-interned? rns-keys.foo)))
               submodule-keys (assert-same '(nil t)
                                           (sort < rmodule-keys.foo))
               extract-from-submodule (assert-same 1
                                                   foo!t)
               extract-nil-from-submodule (assert-same 2
                                                       foo!nil)
               can-set-nil-in-submodule (assert-same 9
                                                     (do (= foo!nil 9)
                                                         foo!nil))
               can-use-make-sub-rmodule (assert-same '(t)
                                                     rmodule-keys.bar))
(suite local-require
       w/rmodule (assert-same "Racket module"
                              (w/rmodule (simple-rmod foo "Racket module") foo))
       w/module (assert-same "Arc module"
                             (w/module (simple-mod foo "Arc module") foo)))


; This is the actual code I (Ross Angle) used to test these things in
; development.

#;(def fn-tryout (body)
  (prn)
  (on-err [prn "Error: " inspect._]
          (thunk:ret result (body)
            (prn "Result: " inspect.result))))

#;(mac tryout body
  `(fn-tryout:fn () ,@body))

#;(def ns-tryout ()

;(= gl $.global-arcracket)
;(= foo 1 bar 2)
;(ns-set-renamer gl!foo gl!bar)
;($.eval:$.ac-denil
;  `(define-syntax ,gl!foo (make-rename-transformer #',gl!bar)))
;(= foo 3)
;(prs foo bar)

;(= foo (make-ns 'a 1 'b 2))
;(write:coerce foo 'table)

  (tryout "now namespaces")
  (let foo (nsobj a 1 b 2)
    (tryout:firstn 10 ns-keys.foo)
    (tryout foo!a)
    (tryout:= foo!a 2)
    (tryout foo!a)
    )

  (tryout "now define-boxvar in Racket")
  (tryout:eval
    `($:define-syntax-rule (define-boxvar var base-var)
       (define-syntax var
         (make-set!-transformer
           (lambda (stx)
             (syntax-case stx (set!)
               ((set! _ val) #'(set-box! base-var val))
               (id (identifier? #'id) #'(unbox base-var))))))))
  (tryout:$.eval:ac-denil '(define bar (box 9)))
  (tryout:$.eval:ac-denil '(define-boxvar foo bar))
  (tryout:$.eval:ac-denil 'foo)
  (tryout:$.eval:ac-denil '(set! foo 4))
  (tryout:$.eval:ac-denil 'foo)
  (tryout:$.eval:ac-denil 'bar)

  (tryout "now modecules")
  (with (foo (nsobj i 9 j 10)
         bar (tryout make-modecule.11))
    (tryout:firstn 10 ns-keys.foo)    (tryout:= foo!k 12)
    (tryout foo!k)

    (tryout:ns-set-modecule 'k bar foo)
    (tryout:firstn 10 ns-keys.foo)
    (tryout:w/current-ns foo $.eval!_k)
    (tryout foo!k)
    (tryout:= foo!k 12)
    (tryout foo!k)
    )

  (tryout "now modules")
  (let foo (simple-mod a 1 b 2)
    (tryout:firstn 10 ns-keys.foo)
    (tryout foo!a)
    (tryout:do (= foo!a 9) foo!a))

  (tryout "now submodules")
  (let foo (simple-rmod t 1 nil 2)
    (tryout:firstn 10 rns-keys.foo)
    (tryout:firstn 10 rmodule-keys.foo)
    (tryout foo!t)
    (tryout foo!nil)
    (tryout:do (= foo!nil 9) foo!nil)
    (let bar (tryout:make-sub-rmodule foo idfn)
      (tryout:firstn 10 rns-keys.bar)
      (tryout:firstn 10 rmodule-keys.bar)))

  (tryout "now local requires")
  (tryout:w/rmodule (simple-rmod foo "Racket module")
    foo)
  (tryout:w/module (simple-mod foo "Arc module")
    foo)
  )
