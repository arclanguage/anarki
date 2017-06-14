(require "lib/ns.arc")

(suite namespace
       (setup foo (nsobj a 1 b 2))
       (test make-namespaces
             (assert-same '(a b)
                          (sort <
                                (keep $.symbol-interned? ns-keys.foo))))
       (test get-namespaces (assert-same 1 foo!a))
       (test assigning-into-namespace
             (assert-same 2 (do (= foo!a 2) foo!a))))

(suite modecule
       (setup foo (nsobj i 9 j 10)
              bar make-modecule.11)
       (test make-namespaces
             (assert-same '(i j)
                          (sort <
                                (keep $.symbol-interned? ns-keys.foo))))
       (test ns-set-modecule
             (assert-same bar
                          (ns-set-modecule 'k bar foo)))
       (test modecule-setting-works
             (assert-same '(i j k)
                          (do (ns-set-modecule 'k bar foo)
                              (sort <
                                    (keep $.symbol-interned? ns-keys.foo)))))
       (test val-by-evaling-racket-code
             (assert-same 11
                          (do (ns-set-modecule 'k bar foo)
                              (w/current-ns foo
                                            ($.eval global-arcracket!k)))))
       (test val-by-calling-namespace
             (assert-same 10 foo!j))
       (test set-val-with-=
             (assert-same 12 (do (= foo!k 12) foo!k))))

(suite module
       (setup foo (simple-mod a 1 b 2))
       (test ns-keys
             (assert-same '(a b)
                          (sort <
                                (keep $.symbol-interned? ns-keys.foo))))
       (test extract-from-module
             (assert-same 1 foo!a))
       (test set-in-module
             (assert-same 9 (do (= foo!a 9) foo!a))))

(suite submodule
       (setup foo (simple-rmod t 1 nil 2)
              bar (make-sub-rmodule foo idfn))
       (test get-keys
             (assert-same '(nil t)
                          (sort <
                                (keep $.symbol-interned? rns-keys.foo))))
       (test submodule-keys
             (assert-same '(nil t)
                          (sort < rmodule-keys.foo)))
       (test extract-from-submodule
             (assert-same 1 foo!t))
       (test extract-nil-from-submodule
             (assert-same 2 foo!nil))
       (test can-set-nil-in-submodule
             (assert-same 9 (do (= foo!nil 9) foo!nil)))
       (test can-use-make-sub-rmodule
             (assert-same '(t) rmodule-keys.bar)))

(suite local-require
       (test w/rmodule
             (assert-same "Racket module"
                          (w/rmodule (simple-rmod foo "Racket module")
                                     foo)))
       (test w/module
             (assert-same "Arc module"
                          (w/module (simple-mod foo "Arc module") foo))))

