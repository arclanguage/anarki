(def argmap (symf annf args)
  (map (afn (arg)
         (if (isa arg 'sym) (symf arg)
             (atom (cadr arg)) (annf arg)
             (annf `(,(car arg) ,(self (cadr arg)) ,@(cddr arg)))))
       args))

(mac defm (name args . body)
  `(redef ,name ,(argmap idfn [if (is (car _) 't) (cadr _) _]
                         args)
     (if (and ,@(map (fn (decl) `(isa ,(car decl) ',(cadr decl)))
                     (trues idfn
                            (argmap [idfn nil]
                                    [if (is (car _) 't) (cdr _) (cadr _)]
                                    args))))
         (do ,@body)
         (old ,@(argmap idfn [cadr _] args)))))

