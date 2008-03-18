;; Interpreter with state. 

(= *primitive-procedures*
   '(((+ - * / cons car cdr no is nil t)
      (&primop +) (&primop -) (&primop *) (&primop /)
      (&primop cons) (&primop car) (&primop cdr) (&primop no)
      (&primop is) '() t)))

(def driver ()
  (driver-loop *primitive-procedures* nil (prn "|LITHP ITH LITHENING|")))

(def driver-loop (env hunoz hukairz)
  (driver-loop-1 env (read)))

(def driver-loop-1 (env form)
  (if (atom form) (driver-loop env nil (prn (eval* form env)))
      (iso form '(quit)) nil
      (is (car form) 'def)
      (driver-loop env (evset (cadr form)
			      (list '&procedure
				    (car:cddr form)
				    (cadr:cddr form) env) env)
		   (prn (cadr form)))
      (driver-loop env nil (prn (eval* form env)))))

(def eval* (exp env)
  (if (atom exp) (if (number exp) exp
		     (value exp env))
      (is (car exp) 'quote) (cadr exp)
      (is (car exp) 'fn) (list '&procedure (cadr exp) (car:cddr exp) env)
      (is (car exp) 'set) (evset (cadr exp) (eval* (cadr:cdr exp) env) env)
      (is (car exp) 'do) (evdo (cdr exp) env nil)
      (is (car exp) 'if) (evif (cdr exp) env)
      (apply* (value (car exp) env)
	      (evlis (cdr exp) env)
	      env)))

(def apply* (fun args env)
  (if (primop fun) (primop-apply fun args)
      (is (car fun) '&procedure)
      (eval* (car:cddr fun) (bind (cadr fun) args (cadr:cddr fun)))
      (err "cannot apply " fun)))

(def evset (var val env)
  ((fn (slot)
       (if (is slot '&unbound) (ev-top-level-set var val env)
	   (scar slot val))) (lookup var env)))

(def ev-top-level-set (var val env)
  (if (no (cdr env))
      (cadr:car (scar env
		      (cons (cons var (caar env))
			    (cons val (cdr:car env)))))
      (ev-top-level-set var val (cdr env))))

(def evdo (exps env hunoz)
  (if (no (cdr exps)) (eval* (car exps) env)
      (evdo (cdr exps) env (eval* (car exps) env))))
	  

(def evif (clauses env)
  (if (no clauses) nil
      (no (cadr clauses)) (eval* (car clauses) env)
      (eval* (car clauses) env) (eval* (cadr clauses) env)
      (evif (cddr clauses) env)))

(def evlis (arglist env)
  (if (no arglist) nil
      (cons (eval* (car arglist) env)
	    (evlis (cdr arglist) env))))

(def bind (vars args env)
  (if (is (len vars) (len args))
      (cons (cons vars args) env)
      (err "bind vars different length from args")))

(def value (name env)
  (value1 name (lookup name env)))

(def value1 (name slot)
  (if (is slot '&unbound) (err "unbound symbol" name)
      (car slot)))

(def lookup (name env)
  (if (no env) '&unbound
      (lookup1 name (caar env) (cdr:car env) env)))

(def lookup1 (name vars vals env)
  (if (no vars) (lookup name (cdr env))
      (is name (car vars)) vals
      (lookup1 name (cdr vars) (cdr vals) env)))

(def primop (fun)
  (is (car fun) '&primop))

(def primop-apply (fun args)
  (apply (eval (cadr fun)) args))