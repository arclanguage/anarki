(require 'lib/ns.arc)

(suite namespace
       (setup foo (nsobj a 1 b 2)
              bar (nsobj a 3 b 4))

       (test namespaces-are-distinct
             (assert-same (list 1 3)
                          (list foo!a bar!a)))

       ; We can use 'nsobj to make namespaces.
       (test make-namespaces
             (assert-same '(a b)
                          (sort <
                                (keep $.symbol-interned? ns-keys.foo))))

       ; Getting from a namespace works.
       (test get-namespaces (assert-same 1 foo!a))

       ; Assigning into a namespace works.
       (test assigning-into-namespace
             (assert-same 2 (do (= foo!a 2) foo!a)))

       (test namespaces-do-not-disturb-each-other
             (assert-same 3 (do (= foo!a 2) bar!a)))

       (test w/current-ns-get
             (assert-same 2
               (w/current-ns foo
                 ($.anarki-init)
                 (eval 'b))))

       (test w/current-ns-set-1
             (assert-same 5
               (w/current-ns foo
                 ($.anarki-init)
                 (eval '(= variable-that-should-wind-up-in-foo 5)))
               ; NOTE: We have to call the namespace like this
               ; *outside* the `w/current-ns` section because the
               ; coercion of `ns` to `fn` is stored in the `coerce*`
               ; variable of the namespace these tests run in, not in
               ; the `foo` namespace.
               foo!variable-that-should-wind-up-in-foo))

       (test w/current-ns-set-2
             (assert-same 5
               (w/current-ns foo
                 ($.anarki-init)
                 (eval '(require 'lib/ns.arc))
                 (eval '(= variable-that-should-wind-up-in-foo 5))
                 ; NOTE: This time, we loaded ns.arc in the `foo`
                 ; namespace itself, so we can use the `ns` to `fn`
                 ; coercion.
                 foo!variable-that-should-wind-up-in-foo))))

(suite modecule
       (setup foo (nsobj i 9 j 10)
              bar make-modecule.11)

       ; We can still use 'nsobj to make namespaces.
       (test make-namespaces
             (assert-same '(i j)
                          (sort <
                                (keep $.symbol-interned? ns-keys.foo))))

       ; We can use 'ns-set-modecule.
       (test ns-set-modecule
             (assert-same bar
                          (ns-set-modecule 'k bar foo)))

       ; After setting a modecule, it's actually there.
       (test modecule-setting-works
             (assert-same '(i j k)
                          (do (ns-set-modecule 'k bar foo)
                              (sort <
                                    (keep $.symbol-interned? ns-keys.foo)))))

       ; We can get the value of 'k by evaluating Racket code.
       (test val-by-evaling-racket-code
             (assert-same 11
                          (do (ns-set-modecule 'k bar foo)
                              (w/current-ns foo
                                            ($.eval global-arcracket!k)))))

       ; We can get the value of 'k by calling the namespace.
       (test val-by-calling-namespace
             (assert-same 10 foo!j))

       ; We can set the value of 'k using '=.
       (test set-val-with-=
             (assert-same 12 (do (= foo!k 12) foo!k))))

(suite module
       (setup foo (simple-mod a 1 b 2))

       ; We can get the keys of a module using 'ns-keys.
       (test ns-keys
             (assert-same '(a b)
                          (sort <
                                (keep $.symbol-interned? ns-keys.foo))))

       ; We can call a module to get a value from it.
       (test extract-from-module
             (assert-same 1 foo!a))

       ; We can set to a module.
       (test set-in-module
             (assert-same 9 (do (= foo!a 9) foo!a))))

(suite submodule
       (setup foo (simple-rmod t 1 nil 2)
              bar (make-sub-rmodule foo idfn))

       ; We can get the keys to a Racket module using 'rns-keys.
       (test get-keys-with-rns-keys
             (assert-same '(nil t)
                          (sort <
                                (keep $.symbol-interned? rns-keys.foo))))

       ; We can get the keys to a Racket module using 'rmodule-keys
       (test get-keys-with-rmodule-keys
             (assert-same '(nil t)
                          (sort < rmodule-keys.foo)))

       ; We can get a variable named 't from a Racket module.
       (test get-t-from-rmodule
             (assert-same 1 foo!t))

       ; We can get a variable named 'nil from a Racket module.
       (test get-nil-from-rmodule
             (assert-same 2 foo!nil))

       ; We can set a variable named 'nil in a Racket module.
       (test can-set-nil-in-rmodule
             (assert-same 9 (do (= foo!nil 9) foo!nil)))

       ; We can use 'make-sub-rmodule.
       (test can-use-make-sub-rmodule
             (assert-same '(t) rmodule-keys.bar)))

(suite local-require

       ; We can use 'w/rmodule.
       (test w/rmodule
             (assert-same "Racket module"
                          (w/rmodule (simple-rmod foo "Racket module")
                                     foo)))

       ; We can use 'w/module.
       (test w/module
             (assert-same "Arc module"
                          (w/module (simple-mod foo "Arc module") foo))))

