
(= *primitive-procedures*
   '(((+ - * / cons car cdr no is)
      (&primop +) (&primop -) (&primop *) (&primop /)
      (&primop cons) (&primop car) (&primop cdr) (&primop no)
      (&primop is))))

(def driver ()
  (driver-loop *primitive-procedures* (prn "|LITHP ITH LITHENING|")))

(def driver-loop (procedures hunoz)
  (driver-loop-1 procedures (read)))

(def driver-loop-1 (procedures form)
  (if (is (car form) 'quit) nil
      (atom form)
      (driver-loop procedures (prn (eval* form '() procedures)))
      (is (car form) 'def)
      (driver-loop (bind (list (cadr form))
			 (list (list (car:cddr form) (cadr:cddr form)))
			 procedures)
		   (prn (cadr form)))
      (driver-loop procedures (prn (eval* form nil procedures)))))

(def eval* (exp env procedures)
  (if (atom exp)
      (if (is exp 'nil) 'nil
	  (is exp 't) 't
	  (number exp) exp
	  (value exp env))
      (is (car exp) 'quote)
      (cadr exp)
      (is (car exp) 'if)
      (evif (cdr exp) env procedures)
      (apply* (value (car exp) procedures)
	      (evlis (cdr exp) env procedures)
	      procedures)))

(def apply* (fun args procedures)
  (if (primop fun) (primop-apply fun args)
      (eval* (cadr fun) (bind (car fun) args '()) procedures)))

(def evif (clauses env procedures)
  (if (no clauses) nil
      (no (cadr clauses)) (eval* (car clauses) env procedures)
      (eval* (car clauses) env procedures) (eval* (cadr clauses) env procedures)
      (evif (cddr clauses) env procedures)))

(def evlis (arglist env procedures)
  (if (no arglist) nil
      (cons (eval* (car arglist) env procedures)
	    (evlis (cdr arglist) env procedures))))

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
  (eval (cons (cadr fun) args)))
