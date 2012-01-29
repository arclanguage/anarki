;;;
; utils
;;;

; heuristics to generate valid JS identifier
(defmemo symbol-name-to-js-string (symb)
	 (with (name (string symb)
		     lowercase t
		     all-uppercase nil)
	   (if (and (len> name 1) (endmatch "*" name))
	       (= all-uppercase t
		  name (butlast name)))
	   (tostring
	     (each c name
	       (aif (is c #\-)
		    (= lowercase (no lowercase))
		    (posmatch (string c) "!?#@%+*/=:<>")
		    (pr ('("bang" "what" "hash" "at" "percent" "plus" "star" "slash" "equals" "colon" "lessthan" "greaterthan") it))
		    (do
		      (pr (if (and lowercase (no all-uppercase))
			      (downcase c)
			      (upcase c)))
		      (= lowercase t)))))))

;;;
; compiler
;;;

(= as-reserved-symbol-names*
   (list "break" "case" "catch" "continue" "default" "delete" "do" "else"
	 "finally" "for" "function" "if" "in" "instanceof" "new" "return"
	 "switch" "this" "throw" "try" "typeof" "var" "void" "while" "with"
	 "abstract" "boolean" "byte" "char" "class" "const" "debugger" "double"
	 "enum" "export" "extends" "final" "float" "goto" "implements" "import"
	 "int" "interface" "long" "native" "package" "private" "protected"
	 "public" "short" "static" "super" "synchronized" "throws" "transient"
	 "volatile"))

(def add-as-reserved-symbol (name)
  (pushnew (symbol-name-to-js-string name) as-reserved-symbol-names*))

(def is-as-reserved-symbol (symb)
  (if (isa symb 'sym)
      (mem (sym-to-js-string symb) as-reserved-symbol-names*)))

(= as-special-forms* {})

(def get-as-special-form (name)
  (as-special-forms* name))

; XXX what is the point of the dstructuring bind in the original?
(mac define-as-special-form (name lambda-list . body)
  `(= (as-special-forms* ',name)
      (fn ,lambda-list
	,@body)))

(def is-as-special-form (form)
  (and (acons form)
       (isa (car form) 'sym)
       (as-special-forms* (car form))))

(def is-comparison-form (form)
  (mem (car form) '(< > <= >= is isnt)))

(def is-funcall-form (form)
  (and form
       (acons form)
       (no (is-op-form form))
       (no (is-as-special-form form))))

(= as-macro-toplevel* {}
   as-macro-env* (list as-macro-toplevel*)
   as-symbol-macro-toplevel {}
   as-symbol-macro-env* (list as-symbol-macro-toplevel)
   as-special-forms* {}
   as-local-function-names* nil
   as-enclosing-lexicals nil
   )

(def lookup-macro-def (name env)
  (aif (find [_ name] env) (it name)))

(def as-macroexpand (form)
  (aif (or (and (isa form 'sym) (lookup-macro-def form as-symbol-macro-env*))
	   (and (acons form) (lookup-macro-def (car form) as-macro-env*)))
       (list (car (as-macroexpand (apply it form))) t)
       (list form nil)))

;;; operators

; XXX fix pipe symbol
(with (precedence-table {} i 0)
  (each level '((js:new js:getprop js:aref)
		(postfix++ postfix--)
		(delete void typeof ++ -- unary+ unary- ~ !)
		(* / %)
		(+ -)
		(<< >> >>>)
		(< > <= >= js:instanceof js:in)
		(== != === !==)
		(&)
		(^)
		(\|)
		(&& and)
		(\|\| or)
		(js:?)
		(= *= /= %= += -= <<= >>= >>>= &= ^= \|=)
		(comma))
    (map
      (fn (op)
	(= (precedence-table op) i))
      level)
    (++ i))
  (def op-precedence (op)
    (precedence-table op)))

;;;

(def as-convert-op-name (op)
  (case op
    and '&&
    or '\|\|
    no '!
    is '==
    isnt '!=
    op))

(def maybe-fix-nary-comparison-form (form)
  (if (len> (cdr form) 2)
      (list
	(withs (operator (car form)
			 tmp-var-forms (butlast (cddr form))
			 tmp-vars (accum collect (repeat (len tmp-var-forms) (collect (as-gensym "_cmp"))))
			 all-comparisons (+ (list (cadr form)) tmp-vars (list (last form))))
	  `(with (,@(accum collect (each pair (zip tmp-vars tmp-var-forms) (collect (car pair)) (collect (cadr pair)))))
	     (and ,@(accum collect
		      (each (x1 x2) (zip (butlast all-comparisons) (cdr all-comparisons))
			(collect (list operator x1 x2)))))))
	t)
      (list
	form
	nil)))

(def compile-op-form (form)
  (prn "compiling op form: " form)
  `(js:operator ,(as-convert-op-name (as-compile-symbol (car form)))
		,@(map [as-compile-expression (car (as-macroexpand _))] (cdr form))))

(def is-op-form (form)
  (and (or (acons form) (no form))
       (no (is-as-special-form form))
       (op-precedence (form 0))))

(def adjust-as-compilation-level (form level)
  ;(if (or (and (acons form) (mem (car form) '(
  (if (and (isa form 'sym) (is 'toplevel level))
      level
      (is 'toplevel level)
      'inside-toplevel))

(def as-compile-symbol (form)
  (let exp (as-compile-expression form)
    (when (is (exp 0) 'js:variable)
      (= exp (exp 1)))
    (assert (isa exp 'sym))
    exp))

(= as-compilation-level* 'toplevel)

(def compile-funcall-form (form)
  (prn "compiling funcall form: " form)
  `(js:funcall
     ,(if (isa (car form) 'sym)
	  `(js:variable ,(maybe-rename-local-function (car form)))
	  (as-compile-expression (car (as-macroexpand (car form)))))
     ,@(map as-compile-expression (cdr form))))

(def as-compile (expr)
  (prn "compiling: " expr)
  (case (type expr)
    int expr
    num expr
    string expr
    char (as-compile (string expr))
    sym
    (let (expansion did-expand) (as-macroexpand expr)
      (if did-expand
	  (as-compile expansion)
	  (is-as-special-form (list expr))
	  (if (is-as-reserved-symbol expr)
	      ((get-as-aspecial-form expr))
	      (err "Attempting to use ArcScript special form " expr " as variable"))
	  `(js:variable ,expr)))
    cons
    (let (expansion did-expand) (as-macroexpand expr)
      (let as-compilation-level*
	  (if did-expand
	      as-compilation-level*
	      (adjust-as-compilation-level expr as-compilation-level*))
	(if did-expand
	    (as-compile expr)
	    (is-as-special-form expr)
	    (apply (get-as-special-form (car expr)) (cdr expr))
	    (is-comparison-form expr)
	    (let (form fixed) (maybe-fix-nary-comparison-form expr)
	      (prn "is comparison!")
	      (if fixed
		  (as-compile form)
		  (compile-op-form form)))
	    (is-op-form expr)
	    (compile-op-form expr)
	    (is-funcall-form expr)
	    (compile-funcall-form expr)
	    (err "Cannot compile " expr " to an ArcScript form.")
    )))))

(def as-compile-statement (form)
  (let compiling-expression nil
    (as-compile form)))

(def as-compile-expression (form)
  (prn "compiling expression: " form)
  (let compiling-expression t
    (as-compile form)))

(def as-gensym ((o prefix "_js"))
  (let prefix (if (isa prefix 'string) prefix (symbol-to-js-string prefix nil))
    ; XXX original is more complex
    (uniq prefix)))

;;;
; printer
;;;

(def as-print (expr)
  (case (type expr)
    sym (asw (symbol-to-js-string expr))
    )
  )

;;;
; special forms
;;;

(mac defasliteral (name string)
  `(do
     (add-as-reserved-symbol ',name)
     (define-as-special-form ,name ()
			     (list 'js:literal ,string))))

(def maybe-rename-local-function (fun-name)
  (prn "maybe-rename-local-function: " fun-name)
  (aif (assoc fun-name as-local-function-names*)
       it
       fun-name))

(defasliteral this "this")
(defasliteral t "true")
(defasliteral true "true")
(defasliteral false "false")
(defasliteral f "false")
(defasliteral nil "null")
(defasliteral undefined "undefined")

; XXX this was a macrolet in original version
(mac def-for-literal (name printer)
  `(do
     (add-as-reserved-symbol ',name)
     (define-as-special-form ,name ((o label))
			     (list ',printer label))))
(def-for-literal break js:break)
(def-for-literal continue js:continue)

(define-as-special-form quote (x)
			(let quote% (fn (expr) (when expr `',expr))
			  (as-compile-expression
			    (case (type x)
			      cons `(list ,@(map quote% x))
			      nil '(list)
			      sym (symbol-to-js-string x)
			      num x
			      int x
			      string x))))

; XXX was macrolet
(mac def-unary-ops ops
  `(do
     ,@(map
	 (fn (op)
	   (with (op (if (acons op) (car op) op)
		     is-space (if (acons op) (op 1)))
	     `(define-as-special-form ,op (x)
				      (list 'js:unary-operator ',op
					    (as-compile-expression (as-macroexpand x))
					    t ,is-space))))
	 ops)))

(def-unary-ops ~ ! (new t) (delete t) (void t) (typeof t))
