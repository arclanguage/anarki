; ns.arc.t
;
; Ross Angle (rocketnia) 2011


(require "lib/ns.arc")


; Namespace tests
(let foo (nsobj a 1 b 2)

  (test-iso "We can use 'nsobj to make namespaces."
    (sort < (keep $.symbol-interned? ns-keys.foo))
    '(a b))

  (test-iso "Getting from a namespace works."
    foo!a
    1)

  (test-iso "Assigning into a namespace works."
    (do (= foo!a 2) foo!a)
    2)
  )


; Modecule tests
(with (foo (nsobj i 9 j 10) bar make-modecule.11)

  (test-iso "We can still use 'nsobj to make namespaces."
    (sort < (keep $.symbol-interned? ns-keys.foo))
    '(i j))

  (test-iso "We can use 'ns-set-modecule."
    (ns-set-modecule 'k bar foo)
    bar)

  (test-iso "After setting a modecule, it's actually there."
    (sort < (keep $.symbol-interned? ns-keys.foo))
    '(i j k))

  (test-iso "We can get the value of 'k by evaluating Racket code."
    (w/current-ns foo ($.eval global-arcracket!k))
    11)

  (test-iso "We can get the value of 'k by calling the namespace."
    foo!k
    11)

  (test-iso "We can set the value of 'k using '=."
    (do (= foo!k 12) foo!k)
    12)
  )


; Module tests
(let foo (simple-mod a 1 b 2)

  (test-iso "We can get the keys of a module using 'ns-keys."
    (sort < (keep $.symbol-interned? ns-keys.foo))
    '(a b))

  (test-iso "We can call a module to get a value from it."
    foo!a
    1)

  (test-iso "We can set to a module."
    (do (= foo!a 9) foo!a)
    9)
  )


; Submodule tests
(let foo (simple-rmod t 1 nil 2)

  (test-iso "We can get the keys to a Racket module using 'rns-keys."
    (sort < (keep $.symbol-interned? rns-keys.foo))
    '(nil t))

  (test-iso (+ "We can get the keys to a Racket module using "
               "'rmodule-keys.")
    (sort < rmodule-keys.foo)
    '(nil t))

  (test-iso "We can get a variable named 't from a Racket module."
    foo!t
    1)

  (test-iso "We can get a variable named 'nil from a Racket module."
    foo!nil
    2)

  (test-iso "We can set a variable named 'nil in a Racket module."
    (do (= foo!nil 9) foo!nil)
    9)

  (let bar (make-sub-rmodule foo idfn)

    (test-iso "We can use 'make-sub-rmodule."
      rmodule-keys.bar
      '(t))))


; Local require tests

(test-iso "We can use 'w/rmodule."
  (w/rmodule (simple-rmod foo "Racket module") foo)
  "Racket module")

(test-iso "We can use 'w/module."
  (w/module (simple-mod foo "Arc module") foo)
  "Arc module")


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
