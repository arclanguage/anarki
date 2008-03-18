;; Evaluator with static binding

(= *primitive-procedures*
   '(((+ - * / cons car cdr no is nil t)
      (&primop +) (&primop -) (&primop *) (&primop /)
      (&primop cons) (&primop car) (&primop cdr) (&primop no)
      (&primop is) '() t)))

(def driver ()
  (driver-loop *primitive-procedures* (prn "|LITHP ITH LITHENING|")))

(def driver-loop (env hunoz)
  (driver-loop-1 env (read)))

(def driver-loop-1 (env form)
  (if (is (car form) 'quit) nil
      (atom form) (driver-loop env (prn (eval* form env)))
      (is (car form) 'def)
      (driver-loop (list (cons (cons (cadr form) (caar env))
			       (cons (list '&labeled
					   (car:cddr form)
					   (cadr:cddr form))
				     (cdr:car env))))
		   (prn (cadr form)))
      (driver-loop env (prn (eval* form env)))))

(def eval* (exp env)
  (if (atom exp) (if (number exp) exp
		     (value exp env))
      (is (car exp) 'quote) (cadr exp)
      (is (car exp) 'fn) (list '&procedure (cadr exp) (car:cddr exp) env)
      (is (car exp) 'if) (evif (cdr exp) env)
      (apply* (value (car exp) env)
	      (evlis (cdr exp) env)
	      env)))

(def apply* (fun args env)
  (if (primop fun) (primop-apply fun args)
      (is (car fun) '&procedure)
      (eval* (car:cddr fun) (bind (cadr fun) args (cadr:cddr fun)))
      (err "cannot apply " fun)))

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
      (is name (car vars))
      (if (atom (car vals))
	  vals
	  (is (caar vals) '&labeled)
	  (list (list '&procedure (cadr:car vals) (car:cddr:car vals) env))
	  vals)
      (lookup1 name (cdr vars) (cdr vals) env)))

(def primop (fun)
  (is (car fun) '&primop))

(def primop-apply (fun args)
  (apply (eval (cadr fun)) args))