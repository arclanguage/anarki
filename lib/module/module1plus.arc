; module1plus.arc
; by AmkG
; a module system based on Ray Myers' module1

; (module foo
;   (interface cat
;     niaw scratch)
;   (interface dog
;     arf dig)
;   (module-vars doodie)
;   (def niaw () (= doodie 'niaw) "niaw!")
;   (def scratch () (= doodie 'scratch) "nyaaaar!")
;   (def arf () (prn "cat: " doodie) "arf! arf!")
;   (def dig () (prn "cat: " doodie " buried!") "grrr...."))
;
; (module bar
;   (public ownzor)
;   (use foo!arf)
;   (use foo!dig -> digger)
;   (use-interface foo!cat)
;   (def ownzor ()
;    (prn "petting cat... " (niaw))
;    (prn "dog's here...")
;    (prn (arf))
;    (prn "cat is scratching... " (scratch))
;    (prn "dog's back...")
;    (prn (digger))))

#|
About interfaces:
  Suppose we have a largeish module with several dozen functions.
It's being actively developed, so future versions of the module,
although maintaining back-compatibility, will keep adding new
functions.  Now it would be bad to use something like:
  (use* large-module) ;add all functions in large-module
  This is because, large-module might, in the future, add a new
function, which would shadow or collide with the importing module's
functions, or functions from another imported module.

  Instead, we should really cherry-pick only the functions we need:
  (use large-module!fun1 large-module!fun2)
  However, if we need a dozen functions, this is not practical.

  Enter interfaces.  The module writer can write an interface
with several functions in the module.  Instead of choosing
individual functions, the module user chooses entire interfaces.

  Of course, this means that module writers should make a promise
that future version of the module will add new functions in different
(possibly versioned) interfaces.  This means that once an interface
is published, it is set in stone, and new functions should be added
in a newer version of the module's interface.

  Interfaces can also be used to separate levels of complexity.
For example, a module might have a simple interface for basic,
everyday programming, and an advanced interface when certain
hacks are necessary.
  Fortunately, interfaces, as they are now, can share functions.
So a function can exist in the simple interface and also be used
in the advanced interface.

  Because of the nature of Ray Myers' module1, module!public is
also an interface, containing all public functions and interfaces
of the module.
|#

; suggestion: instead of (interface ...) (use-interface ...)
; how about (socket ...) (plug ...) ?

(load "lib/module/module1.arc")

(let oldmodule module
  (= module
    (annotate 'mac
      (fn (name . body)
        (withs
          (
            public (table)
            private (table)
            imports (table)
            finalbody nil
            finalbodytl nil
            addpublic
            (fn (e)
              ; if already private, publicize
              (if (private e) (= (private e) nil))
              (= (public e) t))
            addprivate
            (fn (e)
              ; if already public, don't privatize
              (unless (public e) (= (private e) t)))
            addbody
            (fn (e)
              (if finalbody
                (do (= (cdr finalbodytl) (cons e nil))
                    (= finalbodytl (cdr finalbodytl)))
                (do (= finalbody (cons e nil))
                    (= finalbodytl finalbody))))
            addinterface
            (fn (name funcs)
              (addpublic name)
              (each s funcs (addpublic s))
              (addbody `(= ,name ',funcs)))
            addimport
            (fn (alias orig)
              (addprivate alias)
              (= (imports alias) orig))
            check-bang
            (fn (s)
              (if (ssyntax s)
                  (= s (ssexpand s)))
              (if (acons s)
                  (let (name snd) s
                     (and (is (len s) 2) (caris snd 'quote)))))
            module-of
            (fn (s)
              (if (ssyntax s)
                  (= s (ssexpand s)))
              (car s))
            symbol-of
            (fn (s)
              (if (ssyntax s)
                  (= s (ssexpand s)))
              (cadr:cadr s))
            get-interface
            (fn (s)
              ; make sure it's a module!interface
              (unless (check-bang s)
                (err:string
                      "module: (interface ...) must specify module!interface - "
                      s))
              ; make sure it's globally accessible
              (let fns (errsafe (eval s))
                (unless fns
                  (err:string "module: (interface ...) couldn't access " s))
                (unless (acons fns)
                  (err:string "module: (interface ...) not an interface - " s))
                (each f fns
                  (addimport f `(,(module-of s) ',f)))))
            use-syntax
            (afn (args)
              (when args
                (unless (check-bang (car args))
                  (err:string
                         "module: (use ...) must specify module!symbol - "
                         (car args)))
                (if (is (cadr args) '->)
                    (do
                      (addimport (car:cdr:cdr args) (car args))
                      (self (cdr:cdr:cdr args)))
                    (do
                      (addimport (symbol-of (car args)) (car args))
                      (self (cdr args))))))
            module-export-syntax
            (fn (args)
              (each sym (map car (pair (*module-export-helper args)))
                (addprivate sym)))
            form-handler
            (fn (fun args el)
                (case fun
                  module     (addprivate (car args))
                  def        (addprivate (car args))
                  module-export
                             (module-export-syntax args)
                  mac        (err "module: macros not yet supported!")
                             nil))
            ; handles ssyntax.  Really a hack to support p-m:def ^^
            check-syntax
            (fn (fun args el)
              (let fun (ssexpand fun)
                (if (caris fun 'compose)
                  (each fun (cdr fun)
                    (form-handler fun args el)))))
            travelbody
            (each el body
              (if (acons el)
                (let (fun . args) el
                  (if (ssyntax fun)
                      (check-syntax fun args el)
                      (case fun
                        public        (each s args (addpublic s))
                        module-vars   (each s args (addprivate s))
                        interface     (addinterface (car args) (cdr args))
                        use-interface (each s args (get-interface s))
                        use           (use-syntax args)
                                      (do (form-handler fun args el)
                                          (addbody el)))))
                (addbody el)))
            truekeys (fn (tb) (accum co (ontable k v tb (if v (co k))))))
          ; (name public private . body)
          (apply oldmodule
            `(,name ,(truekeys public) ,(truekeys private)
               (= ,@(apply join (accum co (ontable alias real imports
                                                (co `(,alias ,real))))))
               ,@finalbody)))))))

(def *module-export-helper (args)
  (if (ssyntax (car args)) (zap ssexpand (car args)))
  (if (no args)
      ()
      (do
        ; make sure it's module!symbol syntax
        (let f (car args)
          (unless
            (and (acons f) (is (len f) 2) (is (car:car:cdr f) 'quote))
            (err:string "module-export: must specify module!symbol - " f)))
        (if (is (cadr args) '->)
            `(,(car:cdr:cdr args) ,(car args) ,@(*module-export-helper (cdr:cdr:cdr args)))
            `(,(cadr:cadr (car args)) ,(car args) ,@(*module-export-helper (cdr args))) ))))

(mac module-export args
  " Exports the specified module functions to the current namespace.
    See also [[module]] "
  `(=
    ,@(*module-export-helper args)))


