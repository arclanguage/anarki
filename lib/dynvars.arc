;; Save-based dynamic variables
(mac with* (parms . body)
  (let uparms (mapeach e pair.parms `(,(uniq) ,(uniq) ,@e))
    `(with ,(mappend (fn ((save temp var val)) `(,temp ,val ,save ,var)) uparms)
       (protect
         (fn ()
           ;; update the variables
           ,@(mapeach (save temp var val) uparms
               `(= ,var ,temp))
           ,@body)
         (fn ()
           ;; restore the variables
           ,@(mapeach (save temp var val) uparms
               `(= ,var ,save)))))))

(mac let* (var val . body)
  `(with* (,var ,val) ,@body))

(mac withs* (parms . body)
  (if no.parms `(do ,@body)
    (let (var val . rest) parms
      `(let* ,var ,val (withs* ,rest ,@body)))))

;; Stack-based dynamic variables
(mac swith (parms . body)
  (let uparms (mapeach e pair.parms `(,(uniq) ,@e))
    `(with ,(mappend (fn ((temp var val)) `(,temp ,val)) uparms)
       (protect
         (fn ()
           ;; update the variables
           ,@(mapeach (temp var val) uparms
               `(push ,temp ,var))
           ,@body)
         (fn ()
           ;; restore the variables
           ,@(mapeach (temp var val) uparms
               `(pop ,var)))))))

(mac slet (var val . body)
  `(swith (,var ,val) ,@body))

(mac swiths (parms . body)
  (if no.parms `(do ,@body)
    (let (var val . rest) parms
      `(slet ,var ,val (swiths ,rest ,@body)))))

;; Thread-local vars to use with the above
(mac dynvar (name (o init-val))
  `(do
     (set ,name (thread-local))
     (= (,name) ,init-val)))

(mac dynvars vars
  `(do ,@(mapeach var vars
           (if atom.var `(dynvar ,var) `(dynvar ,@var)))))
