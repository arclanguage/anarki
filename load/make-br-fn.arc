; avoid redefinition warning
($ (namespace-undefine-variable! (ac-global-name 'make-br-fn)))

(def -mbf-argsyms (e)
  (case type.e
    sym (if ssyntax.e (-mbf-argsyms ssexpand.e)
            (begins string.e "_") list.e)
    cons (case car.e
           quote nil
           ; TODO: quasiquote & unquote
           (mappend -mbf-argsyms e))))

; better make-br-fn
(mac make-br-fn (body)
  (withs (astab (counts:-mbf-argsyms body)
          args  (awhen astab!__ (wipe astab!__) '__))
    (each s (sort > (if (or args keys.astab) keys.astab '(_)))
      (push s args))
    `(fn ,args ,body)))
