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
            form-handler
            (fn (fun args el)
                (case fun
                  module     (addprivate (car args))
                  def        (addprivate (car args))
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

