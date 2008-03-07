(= scaff-maxid* 0)

(mac addtemscaff (scaff name body)
  `(addtem ,scaff (scaff ,(++ scaff-maxid*) ,name)
	   (fn (scaffold sname entity ename)
	       ,body)))

(def inst-entity1 (scaffold sname entity ename)
  (let ordered-scaffs (sort (fn (a b) (< (cadr:car a) (cadr:car b))) (keep (fn ((k v)) (and (acons k) (is 'scaff (car k)))) tablist.scaffold))
    (each ((x y fname) v) ordered-scaffs
	  (prn "scaffolding " ename ": " fname)
	  (eval (v scaffold
		   (fn ((o suff nil))
		       (sym:string sname suff))
		   entity
		   (fn ((o suff nil))
		       (sym:string ename suff)))))))

(mac inst-entity (ename scaffold . entity)
     `(inst-entity1 ,scaffold ',scaffold ',entity ,ename))