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
;   treats any symbols starting with '_' as args, ordered alphabetically
;     e.g. _1, _2, _3, or _a, _b, _c, etc.
;   treats __ as the rest arg
(mac make-br-fn (body)
  (let args '__
    (each arg (dedup:sort > -mbf-argsyms.body)
      (pushnew arg args))
    `(fn ,args ,body)))
