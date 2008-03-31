
;------------------------------------------------------------------------------
; closure conversion

(def convert (ast self-var free-vars)
	(let cc (rfn cc (ast)
		(if
			(or (alit ast) (aquote ast))
				ast
			(aref ast)
				(aif (pos ast!var free-vars)
					(make-prim (list (make-ref '() self-var) (make-lit '() (+ it 1))) '%closure-ref)
					ast)
			(aset ast)
				(make-set (map cc ast!subx) ast!var)
			(acnd ast)
				(make-cnd (map cc ast!subx))
			(aprim ast)
				(make-prim (map cc ast!subx) ast!op)
			(anapp ast)
				(with
					(fun (car ast!subx)
					 args (map cc (cdr ast!subx)))
					(if (alam fun)
						(make-app:cons (make-lam (list:cc (car fun!subx)) fun!params) args)
						(let f (cc fun)
							(make-app:cons (make-prim (list f (make-lit '() 0)) '%closure-ref) (cons f args)))))
			(alam ast)
				(with
					(new-free-vars (keep [~aglobal _] (fv ast))
					 new-self-var (new-var 'self))
					(make-prim
						(cons
							(make-lam
								(list:convert (car ast!subx) new-self-var new-free-vars)
								(cons new-self-var ast!params))
							(map [cc (make-ref '() _)] new-free-vars))
						'%closure))
			(aseq ast) ; Impossible after CPS
				(make-seq (map cc ast!subx))
				(err "unknown ast" ast)))

		(cc ast)))

(def closure-convert (ast)
	(make-lam (list (convert ast nil '())) '()))

