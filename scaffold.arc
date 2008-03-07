(mac addtemscaff (scaff name body)
  `(addtem ,scaff (scaff . ,name)
	   ',body))

(def inst-entity1 (temname scaff rinf)
  (do (= scaffold (eval scaff))
      (= sname (fn ((o suff nil))
		   (sym:string scaff suff)))
      (= entity rinf)
      (= ename (fn ((o suff nil))
		 (sym:string temname suff)))
      (each v tablist.scaffold
	    (when (and (acons (car v)) (is 'scaff (caar v)))
	      (do (prn "scaffolding " (ename) ":" (car v))
		  (eval (eval (cadr v))))))))

(mac inst-entity (temname scaff . rinf)
     `(inst-entity1 ,temname ',scaff ',rinf))