; explicit currying
; by AmkG, with syntax suggested by cchooper
; Use [] without any _ in the brackets to create a
; curried function

(let oldmac make-br-fn
  (= make-br-fn
    (annotate 'mac
      (fn (body)
        (let vars (*mbf-all-vars body)
          (if (some
                [and (or (is _ '_) (re-match (re "^_[_123456789]") (string _))) (*mbf-free? body _)]
                vars)
            ((rep oldmac) body)
            (w/uniq args
              `(fn ,args (apply ,@body ,args)))))))))
