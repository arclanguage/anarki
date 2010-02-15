(extend ac (s env) (and (errsafe:acons s) (is (car s) 'scheme))
  `(begin ,@(cdr s)))

(= ac-denil       scheme.ac-denil)
(= ac-global-name scheme.ac-global-name)
(= ac-niltree     scheme.ac-niltree)

(mac ac-set-global (name val)
  (w/uniq (gname v)
    `(with (,gname (ac-global-name ,name)
            ,v ,val)
       (scheme (namespace-set-variable-value! ,gname ,v))
       nil)))

(= scheme-f (read "#f"))
(= scheme-t (read "#t"))
