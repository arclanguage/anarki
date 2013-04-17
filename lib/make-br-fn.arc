; avoid redefinition warning
($ (namespace-undefine-variable! (ac-global-name 'make-br-fn)))

(def -mbf-argsym (e)
  (and (begins string.e "_")
       (~find e decl!brfn-ignore)))

(def -mbf-argsyms (e)
  (case type.e
    sym (if ssyntax.e (-mbf-argsyms ssexpand.e)
            (-mbf-argsym e) list.e)
    cons (case car.e
           quote nil
           ; TODO: quasiquote & unquote
           (mappend -mbf-argsyms e))))

(declaration 'brfn-ignore
             (fn (old flag (sym))
               (consif (when flag sym)
                       (rem sym old))))

; better make-br-fn
(mac make-br-fn (body)
  (withs (astab (counts:-mbf-argsyms body)
          args  (do (wipe astab!__) '__))
    (each s (sort > keys.astab)
      (push s args))
    `(fn ,args ,body)))
