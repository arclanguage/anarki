; xe = expand expression     xe :: (expr,cte) -> ast

(def xe (e cte)
	(if
		(const-expr? e)
			(xe-const-expr e cte)
		(ident-expr? e)
			(xe-ident-expr e cte)
		(form-expr? e)
			(xe-form-expr e cte)
			(err "syntax-error" e)))

(def const-expr? (e)
	(or (in e t nil) (in (type e) 'int 'num)))

(def ident-expr? (e)
	(isa e 'sym))

(def form-expr? (e)
	(and e (alist e))) ; a non-empty list

(def xe-const-expr (e cte)
	(make-lit '() e))

(def xe-ident-expr (e cte)
	(let b (xe-lookup e cte)
		(if (avar b)
			(make-ref '() b)
			(err "can't reference a nonvariable" e))))

(def xe-form-expr (e cte)
	(let h (car e)
		(let b (and (ident-expr? h) (xe-lookup h cte))
			(if (amacro b)
				(b!expander e cte)
				(make-app (xe-exprs e cte))))))

(def xe-exprs (le cte)
  (map [xe _ cte] le))

(= macis* (make-macro 'is
	(fn (e cte)
		(if (is (len (cdr e)) 2)
			(make-prim (xe-exprs (cdr e) cte) '%is)
			(err "is : expects 2 args")))))

(= macisnt* (make-macro 'isnt
	(fn (e cte)
		(if (is (len (cdr e)) 2)
			(make-prim (xe-exprs (cdr e) cte) '%isnt)
			(err "isnt : expects 2 args")))))

(= mac<* (make-macro '<
	(fn (e cte)
		(if (is (len (cdr e)) 2)
			(make-prim (xe-exprs (cdr e) cte) '%<)
			(err "< expects 2 args")))))

(= mac>* (make-macro '>
	(fn (e cte)
		(if (is (len (cdr e)) 2)
			(make-prim (xe-exprs (cdr e) cte) '%>)
			(err "> expects 2 args")))))

(= mac<=* (make-macro '<=
	(fn (e cte)
		(if (is (len (cdr e)) 2)
			(make-prim (xe-exprs (cdr e) cte) '%<=)
			(err "<= expects 2 args")))))

(= mac>=* (make-macro '>=
	(fn (e cte)
		(if (is (len (cdr e)) 2)
			(make-prim (xe-exprs (cdr e) cte) '%>=)
			(err ">= expects 2 args")))))

(= mac+* (make-macro '+
	(fn (e cte)
		(if (is (len (cdr e)) 2)
			(make-prim (xe-exprs (cdr e) cte) '%+)
			(err "+ expects 2 args")))))

(= mac-* (make-macro '-   ; could have used %- instead
	(fn (e cte)
		(if (is (len (cdr e)) 2)
			(make-prim (xe-exprs (cdr e) cte) '%-)
			(err "- expects 2 args")))))

(= mac** (make-macro '*
	(fn (e cte)
		(if (is (len (cdr e)) 2)
			(make-prim (xe-exprs (cdr e) cte) '%*)
			(err "* expects 2 args")))))

(= macquote* (make-macro 'quote
	(fn (e cte)
		(if (and (is (len (cdr e)) 1) (~acons e.1))
			(make-quote (cdr e))
			(err "quote expects 1 atomic arg")))))

(= macprn* (make-macro 'prn
	(fn (e cte)
		(if (is (len (cdr e)) 1)
			(make-prim (xe-exprs (cdr e) cte) '%prn)
			(err "prn expects 1 arg")))))

(= macset* (make-macro 'set
	(fn (e cte)
		(if (is (len (cdr e)) 2)
			(let b (xe-lookup (cadr e) '())
				(if (avar b)
					(make-set (xe-exprs (cddr e) cte) b)
					(err "can't set a nonvariable" e)))
			(err "set expects 2 args")))))

(= mac=* (make-macro '=
	(fn (e cte)
		(xe (cons 'set (cdr e)) cte))))

(= macif* (make-macro 'if
	(fn (e cte)
		(if
			(is (len (cdr e)) 3)
				(make-cnd (xe-exprs (cdr e) cte))
			(is (len (cdr e)) 2)
				(xe `(if ,(cadr e) ,(car:cddr e) nil) cte)
				(err "if expects 2 or 3 args")))))

(= macfn* (make-macro 'fn
	(fn (e cte)
		(if (>= (len (cdr e)) 1)
			(withs
				(params (map new-var (cadr e))
				 new-cte (extend params cte))
				(make-lam (list:xe (cons 'do (cddr e)) new-cte) params))
			(err "fn expects a parameter list")))))

(= macdo* (make-macro 'do
	(fn (e cte)
		(if
			(is (len (cdr e)) 0)
				(xe nil cte)
			(is (len (cdr e)) 1)
				(xe (cadr e) cte)
				(make-seq (xe-exprs (cdr e) cte))))))

(= maclet* (make-macro 'let
	(fn (e cte)
		(if (>= (len (cdr e)) 1)
			(xe (list (+ (list 'fn (list e.1)) (cut e 3)) e.2) cte)
			(err "let expects a binding")))))

(= macor* (make-macro 'or
	(fn (e cte)
		(if
			(is (len (cdr e)) 0)
				(xe nil cte)
			(is (len (cdr e)) 1)
				(xe (cadr e) cte)
				(xe `((lambda (t1 t2) (if t1 t1 (t2))) ,(cadr e) (lambda () (or ,@(cddr e)))) cte)))))

(= macand* (make-macro 'and
	(fn (e cte)
		(if
			(is (len (cdr e)) 0)
				(xe t cte)
			(is (len (cdr e)) 1)
				(xe (cadr e) cte)
				(xe `((lambda (t1 t2) (if t1 (t2) t1)) ,(cadr e) (lambda () (and ,@(cddr e)))) cte)))))

(def make-initial-cte ()
	(list macis* macisnt* mac<* mac>* mac<=* mac>=* mac+* mac-* mac** macquote* macprn* macset* mac=* macif* macfn* macdo* maclet* macor* macand*))

(def xe-lookup (id cte)
	(or
		(lookup id cte)
		(lookup id xe-global-cte*)
		(let v (new-global id)
			(push v xe-global-cte*)
			v)))

(= xe-global-cte* '())

(def parse-file (filename)
	(= xe-global-cte* (make-initial-cte))
	(xe
		(w/infile f filename
			(let res '()
				(whilet it (read f) (push it res))
				(cons 'do (rev res))))
		'()))

