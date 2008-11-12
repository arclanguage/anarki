; scheme48
; ,open tables sockets extended-ports c-system-function ascii i/o-internal 
; ,open posix-files handle random pp simple-conditions

; to do:
; select, perhaps with threads, or pltt events
; check argument count for complex arguments

; refs.arc, first 300 lines of x, total CPU time including startup. on powerbook.
; scheme48: 31.944u 0.518s 2:13.65 24.2%    0+0k 5+7io 0pf+0w
; mzscheme: 16.425u 0.489s 0:52.61 32.1%    0+0k 26+22io 0pf+0w

; dynamic creation of local variables with =
;   can you create globals inside a procedure? 
;   does action of = depend on whether, at run time,
;     the variable has a global definition?
;   what's the scope of such a variable?
;   though a.lisp seems to create a global, not a local!
; run-time expansion of macros
; how do I know if something is a macro at compile time?
; macros have lexical scope. so how do i know if a lexical
;   variable is going to be bound to a procedure? or to a macro?
; what is annotate doing to symbols?
; tests.arc implies that '(a b) produces a mutable list
;   so (fn () '(a)) produces a list that can be modified,
;     and future calls will reflect the modifications
;   oy. clisp works this way too.
;   it's not that easy to simulate this.
; what is this? (def foo1 (x (o y x)) (list x y))

; it's not clear I translate NILs in the outputs of macros correctly.
;   I translate (NIL . NIL) to ('NIL . '())
; I use Scheme apply to call macros.
;   Scheme apply demands a '()-terminated list.
;   most macros have a . body argument.
;   so body is '()-terminated, not NIL-terminated.
;   solution: ar-false? knows about '()
; this doesn't work, since var isn't a variable name:
; (mac or args
;   (and args
;        (let var (tag 'symbol (list 'or))
;          (list 'let var (car args)
;                (list 'if var var (cons 'or (cdr args)))))))

(module ac mzscheme

(provide (all-defined))
(require (lib "port.ss"))
(require (lib "process.ss"))
(require (lib "pretty.ss"))
(require (lib "file.ss"))

; compile an Arc expression into a Scheme expression,
; both represented as s-expressions.
; env is a list of lexically bound variables, which we
; need in order to decide whether set should create a global.

(define (ac s env)
  (let ((head (xcar s)))
    (cond ((string? s) (string-copy s))  ; to avoid immutable strings
          ((literal? s) s)
          ((eqv? s 'nil) (list 'quote 'nil))
          ((ssyntax? s) (ac (expand-ssyntax s) env))
          ((symbol? s) (ac-var-ref s env))
          ((ssyntax? head) (ac (cons (expand-ssyntax head) (cdr s)) env))
          ((eq? head 'quote) (list 'quote (ac-niltree (cadr s))))
          ((eq? head 'quasiquote) (ac-qq (cadr s) env))
          ((eq? head 'quasisyntax) (ac-qs (cadr s) env))
          ((eq? head 'if) (ac-if (cdr s) env))
			 ((eq? head 'compile) (ac-compile (cdr s)))
          ((eq? head 'fn) (ac-fn (cadr s) (cddr s) env))
          ((eq? head 'set) (ac-set (cdr s) env))
          ((eq? head 'lset) (ac-lset (cdr s) env))
          ; this line could be removed without changing semantics
          ((eq? (xcar head) 'compose) (ac (decompose (cdar s) (cdr s)) env))
          ((pair? s) (ac-call (car s) (cdr s) env))
          ((eof-object? s) (exit))
          ((syntax-closure? s) (ac-closure s env))
          (#t (err "Bad object in expression" s)))))

(define (literal? x)
  (or (boolean? x)
      (char? x)
      (string? x)
      (number? x)
      (procedure? x) ; to allow (eval `(,+ 3 4))
      (eq? x '())))

;;; Compilation issues


(define *defs* (make-hash-table))

(define *ret-types* (make-hash-table))

(define (ac-compile instrs)
	(let*
		((name (ac-global-name (car instrs)))
		 (def (hash-table-get *defs* name))
		 (typs (cdr instrs)))
		(if (symbol? (xcar typs))
			(begin
				(hash-table-put! *ret-types* name (car typs))
				(set! typs (cdr typs))))
		(let ((htyps (make-immutable-hash-table (map (lambda (x) (cons (car x) (cadr x))) typs))))
			(display "<< ") (print def) (newline)
			(namespace-set-variable-value! name (eval (gen-ad-hoc htyps def)))
			(display ">> ") (print (gen-ad-hoc htyps def)) (newline)
			''t)))

(define sp-fns (make-hash-table))

(define (all-num-int parms typ-parms)
	(if (null? parms)
		#t
		(if (memq (bound-type (car parms) typ-parms) '(int num))
			(all-num-int (cdr parms) typ-parms)
			#f)))

(hash-table-put! sp-fns '__+ (lambda (parms typ-parms) ; Returns form annotated by type of result (if known)
	(cond
		((null? parms)
			0)
		((all-num-int parms typ-parms)
			(ar-tag 'num `(+ ,@(map (lambda (x) (car (ar-rep (gen-ad-hoc typ-parms (list x))))) parms))))
		(#t
			`(ar-apply __+ (list ,@(map (lambda (x) (car (ar-rep (gen-ad-hoc typ-parms (list x))))) parms)))))))

(hash-table-put! sp-fns '__- (lambda (parms typ-parms) ; Returns form annotated by type of result (if known)
	(if (all-num-int parms typ-parms)
		(ar-tag 'num `(- ,@(map (lambda (x) (car (ar-rep (gen-ad-hoc typ-parms (list x))))) parms)))
		`(ar-apply __- (list ,@(map (lambda (x) (car (ar-rep (gen-ad-hoc typ-parms (list x))))) parms))))))

(hash-table-put! sp-fns '__* (lambda (parms typ-parms) ; Returns form annotated by type of result (if known)
	(if (all-num-int parms typ-parms)
		(ar-tag 'num `(* ,@(map (lambda (x) (car (ar-rep (gen-ad-hoc typ-parms (list x))))) parms)))
		`(ar-apply __* (list ,@(map (lambda (x) (car (ar-rep (gen-ad-hoc typ-parms (list x))))) parms))))))

(hash-table-put! sp-fns '__/ (lambda (parms typ-parms) ; Returns form annotated by type of result (if known)
	(if (all-num-int parms typ-parms)
		(ar-tag 'num `(/ ,@(map (lambda (x) (car (ar-rep (gen-ad-hoc typ-parms (list x))))) parms)))
		`(ar-apply __/ (list ,@(map (lambda (x) (car (ar-rep (gen-ad-hoc typ-parms (list x))))) parms))))))

(hash-table-put! sp-fns '__< (lambda (parms typ-parms) ; Returns form annotated by type of result (if known)
	(if (all-num-int parms typ-parms)
		(ar-tag 'num `(< ,@(map (lambda (x) (car (ar-rep (gen-ad-hoc typ-parms (list x))))) parms)))
		`(ar-apply __< (list ,@(map (lambda (x) (car (ar-rep (gen-ad-hoc typ-parms (list x))))) parms))))))

(hash-table-put! sp-fns '__> (lambda (parms typ-parms) ; Returns form annotated by type of result (if known)
	(if (all-num-int parms typ-parms)
		(ar-tag 'num `(> ,@(map (lambda (x) (car (ar-rep (gen-ad-hoc typ-parms (list x))))) parms)))
		`(ar-apply __> (list ,@(map (lambda (x) (car (ar-rep (gen-ad-hoc typ-parms (list x))))) parms))))))

(hash-table-put! sp-fns '__<= (lambda (parms typ-parms) ; Returns form annotated by type of result (if known)
	(if (all-num-int parms typ-parms)
		(ar-tag 'num `(<= ,@(map (lambda (x) (car (ar-rep (gen-ad-hoc typ-parms (list x))))) parms)))
		`(ar-apply __<= (list ,@(map (lambda (x) (car (ar-rep (gen-ad-hoc typ-parms (list x))))) parms))))))

(hash-table-put! sp-fns '__>= (lambda (parms typ-parms) ; Returns form annotated by type of result (if known)
	(if (all-num-int parms typ-parms)
		(ar-tag 'num `(>= ,@(map (lambda (x) (car (ar-rep (gen-ad-hoc typ-parms (list x))))) parms)))
		`(ar-apply __>= (list ,@(map (lambda (x) (car (ar-rep (gen-ad-hoc typ-parms (list x))))) parms))))))

(hash-table-put! sp-fns '__is (lambda (parms typ-parms) ; Returns form annotated by type of result (if known)
	(if (all-num-int parms typ-parms)
		(ar-tag 'num `(= ,@(map (lambda (x) (car (ar-rep (gen-ad-hoc typ-parms (list x))))) parms)))
		`(ar-apply __is (list ,@(map (lambda (x) (car (ar-rep (gen-ad-hoc typ-parms (list x))))) parms))))))

(hash-table-put! sp-fns '__isnt (lambda (parms typ-parms) ; Returns form annotated by type of result (if known)
	(if (all-num-int parms typ-parms)
		(ar-tag 'num `(not (= ,@(map (lambda (x) (car (ar-rep (gen-ad-hoc typ-parms (list x))))) parms))))
		`(ar-apply __isnt (list ,@(map (lambda (x) (car (ar-rep (gen-ad-hoc typ-parms (list x))))) parms))))))

(define (bound-type x typ-parms)
	(cond
		((and (eq? (ar-type x) 'sym) (hash-table-get typ-parms x #f))
			(hash-table-get typ-parms x))
		((and (list? x) (>= (length x) 2))
			(let ((it (hash-table-get sp-fns (cadr x) #f)))
				(cond
					(it (ar-type (it (cddr x) typ-parms)))
					((hash-table-get *ret-types* (cadr x) #f) (hash-table-get *ret-types* (cadr x)))
					(#t (ar-type x)))))
		(#t
			(ar-type x))))

(define (is-funcall sym)
	(if (eq? sym 'ar-apply) ; Args are a list
		caddr
		(if (memq sym '(ar-funcall0 ar-funcall1 ar-funcall2 ar-funcall3 ar-funcall4))
			cddr
			#f)))

(define (gen-ad-hoc typ-parms forms)
	(let ((res '()))
		(for-each (lambda (form)
			(if (pair? form)
				(let*
					((head (car form))
					 (args-fn (is-funcall head))
					 (it (and args-fn (hash-table-get sp-fns (cadr form) #f))))
					(if it
						(set! res (cons (ar-rep (it (args-fn form) typ-parms)) res))
						(set! res (cons (gen-ad-hoc typ-parms form) res))))
				(set! res (cons form res))))
			forms)
		(reverse res)))

(define (gen-fn parms typ-parms form)
	(display `(lambda ,parms (list ,(gen-ad-hoc typ-parms form))))
	`(lambda ,parms (list ,(gen-ad-hoc typ-parms form))))

;;; End of compilation issues

(define (ssyntax? x)
  (if (eqv? ((eval '__ssyntax) x) 'nil)
      #f
      #t))

(define (default-ssyntax? x)
  (and (symbol? x)
       (not (or (eqv? x '+) (eqv? x '++)))
       (let ((name (symbol->string x)))
         (has-ssyntax-char? name (- (string-length name) 1)))))

(define (has-ssyntax-char? string i)
  (and (>= i 0)
       (or (let ((c (string-ref string i)))
             (or (eqv? c #\:) (eqv? c #\~) (eqv? c #\.) (eqv? c #\!)))
           (has-ssyntax-char? string (- i 1)))))

(define (read-from-string str)
  (let ((port (open-input-string str)))
    (let ((val (read port)))
      (close-input-port port)
      val)))

(define (expand-ssyntax sym)
  (ac-denil ((eval '__ssexpand) sym)))

(define (default-expand-ssyntax sym)
  ((cond ((or (insym? #\: sym) (insym? #\~ sym)) expand-compose)
         ((or (insym? #\. sym) (insym? #\! sym)) expand-sexpr)
         (#t (error "Unknown ssyntax" sym)))
   sym))

(define (expand-compose sym)
  (let ((elts (map (lambda (tok)
                     (if (eqv? (car tok) #\~)
                         (if (null? (cdr tok))
                             'no
                             `(complement ,(chars->value (cdr tok))))
                         (chars->value tok)))
                   (tokens (lambda (c) (eqv? c #\:))
                           (symbol->chars sym) 
                           '() 
                           '() 
                           #f))))
    (if (null? (cdr elts))
        (car elts)
        (cons 'compose elts))))

(define (expand-sexpr sym)
  (build-sexpr (tokens (lambda (c) (or (eqv? c #\.) (eqv? c #\!)))
                       (symbol->chars sym)
                       '()
                       '()
                       #t)))

; no error-checking!

(define (build-sexpr toks)
  (cond ((null? toks) 
         '())
        ((eqv? (car toks) #\.)
         (cons (chars->value (cadr toks)) 
               (build-sexpr (cddr toks))))
        ((eqv? (car toks) #\!)
         (cons (list 'quote (chars->value (cadr toks)))
               (build-sexpr (cddr toks))))
        (#t
         (cons (chars->value (car toks))
               (build-sexpr (cdr toks))))))
                      

(define (insym? char sym) (member char (symbol->chars sym)))

(define (symbol->chars x) (string->list (symbol->string x)))

(define (chars->value chars) (read-from-string (list->string chars)))

; result will contain || if separator at end of symbol; could use
; that to mean something

(define (tokens test source token acc keepsep?)
  (cond ((null? source)
         (reverse (cons (reverse token) acc)))
        ((test (car source))
         (tokens test
                 (cdr source)
                 '()
                 (let ((rec (cons (reverse token) acc)))
                   (if keepsep?
                       (cons (car source) rec)
                       rec))
                 keepsep?))
        (#t
         (tokens test
                 (cdr source)
                 (cons (car source) token)
                 acc
                 keepsep?))))

; Purely an optimization.  Could in principle do it with a preprocessor
; instead of adding a line to ac, but only want to do it for evaluated
; subtrees, and much easier to figure those out in ac.

(define (decompose fns args)
  (cond ((null? fns) `((fn vals (car vals)) ,@args))
        ((null? (cdr fns)) (cons (car fns) args))
        (#t (list (car fns) (decompose (cdr fns) args)))))


(define (ac-global-name s)
  (if (equal? s (string->symbol (symbol->string s)))
      (string->symbol (string-append "__" (symbol->string s)))
      s))

(define (ac-var-ref s env)
  (or (lex s env) (ac-global-name s)))

; quasiquote

(define (ac-qq args env)
  (list 'quasiquote (ac-qq1 1 args env)))

(define (ac-qs args env)
  `(vector 'closure #t (make-parameter '()) ,(ac-qq args env)))

; process the argument of a quasiquote. keep track of
; depth of nesting. handle unquote only at top level (level = 1).
; complete form, e.g. x or (fn x) or (unquote (fn x))
(define (ac-qq1 level x env)
  (cond
    ((= level 0) (ac x env))
    ((pair? x)
      (let ((head (car x)))
        (cond
          ((eqv? head 'unquote)
            (list 'unquote (ac-qq1 (- level 1) (cadr x) env)))
          ((eqv? head 'unquote-splicing)
            (list 'unquote-splicing
              (list 'ar-nil-terminate (ac-qq1 (- level 1) (cadr x) env))))
          ((eqv? head 'quasiquote)
            (list 'quasiquote (ac-qq1 (+ level 1) (cadr x) env)))
          ((eqv? head 'unsyntax)
            (list 'unquote `(ar-callee-closure ,(ac-qq1 (- level 1) (cadr x) env))))
          ((eqv? head 'unsyntax-splicing)
            (list 'unquote-splicing
              `(map ar-callee-closure
                 (ar-nil-terminate ,(ac-qq1 (- level 1) (cadr x) env)))))
          (#t
            (let ((t (lambda (f) (ac-qq1 level (f x) env))))
              (cons (t car) (t cdr)))))))
    (#t x)))

; (if) -> nil
; (if x) -> x
; (if t a ...) -> a
; (if nil a b) -> b
; (if nil a b c) -> (if b c)

(define (ac-if args env)
  (cond ((null? args) ''nil)
        ((null? (cdr args)) (ac (car args) env))
        (#t `(if (not (ar-false? ,(ac (car args) env)))
;(not (eq? 'nil ,(ac (car args) env)))
                 ,(ac (cadr args) env)
                 ,(ac-if (cddr args) env)))))


; translate fn directly into a lambda if it has ordinary
; parameters, otherwise use a rest parameter and parse it.
(define (ac-fn args body env)
  ((if (ac-simple-args? args) ac-simple-fn ac-complex-fn)
    args body env))

; does an fn arg list use optional parameters or destructuring?
; a rest parameter is not complex
(define (ac-simple-args? args)
  (cond
    ((syntax-closure? args) (ac-simple-args? (closure-expr args)))
    ((pair? args)
      (and (or (symbol? (car args))
               (and (syntax-closure? (car args))
                    (ac-simple-args? (closure-expr (car args)))))
        (ac-simple-args? (cdr args))))
    (#t (or (null? args) (symbol? args)))))

(define (ac-simple-fn args body env)
  (ac-simple-parms (ac-denil-cdr args) env
    (lambda (args env)
      `(lambda ,args ,@(ac-body* body env)))))

(define (ac-simple-parms parm env cont)
  (cond
    ((null? parm) (cont '() env))
    
    ((symbol? parm)
      (let ((psym (if (hygienic?) (gensym) parm)))
        (cont psym (cons (cons parm psym) env))))
    
    ((syntax-closure? parm)
      (let* ((pname (closure-expr parm))
             (penv-param (closure-env-param parm))
             (psym (if (closure-hygienic? parm) (gensym) pname)))
        (parameterize ((penv-param (cons (cons pname psym) (penv-param))))
          (cont psym env))))
    
    ((pair? parm)
      (let ((parm (car parm))
            (parms (cdr parm)))
        (ac-simple-parms parms env
          (lambda (args env)
            (cond
              ((symbol? parm)
                (let ((psym (if (hygienic?) (gensym) parm)))
                  (cont (cons psym args) (cons (cons parm psym) env))))
              
              ((syntax-closure? parm)
                (let* ((pname (closure-expr parm))
                       (penv-param (closure-env-param parm))
                       (psym (if (closure-hygienic? parm) (gensym) (closure-expr parm))))
                  (parameterize ((penv-param (cons (cons pname psym) (penv-param))))
                    (cont (cons psym args) env))))
              
              (#t (err "Can't understand fn arg list" (cons parm parms))))))))
    
    (#t (err "Can't understand fn arg list" parm))))

; translate a fn with optional or destructuring args
; (fn (x (o y x) (o z 21) (x1 x2) . rest) ...)
; arguments in top-level list are mandatory (unless optional),
; but it's OK for parts of a list you're destructuring to
; be missing.
(define (ac-complex-fn args body env)
  (let ((ra (gensym)))
    `(lambda ,ra
       ,@(ac-complex-parms ra args (hygienic?) env
           (lambda (env) (ac-body* body env))))))

; destructures a parameter list
(define (ac-complex-parms arg parm hygiene env cont)
  (cond
    ((or (null? parm) (eq? parm 'nil))
      ; this is doing it wrong, since it means fns with complex parms but no
      ; rest parm will accept more args than they have parms, but it's how
      ; anarki does it, so in the name of backwards compatibility...
      (cont env))
    
    ((symbol? parm)
      (if hygiene
        (cont (cons (cons parm arg) env))
        `((let ((,parm ,arg))
            ,@(cont (cons (cons parm parm) env))))))
    
    ((syntax-closure? parm)
      (ac-complex-parms arg
        (closure-expr parm) (closure-hygienic? parm) ((closure-env-param parm))
        (lambda (closure-env)
          (parameterize (((closure-env-param parm) closure-env))
            (cont env)))))
    
    ((pair? parm)
      (ac-complex-parm arg (car parm) hygiene env
        (lambda (arg env)
          (ac-complex-parms arg (cdr parm) hygiene env cont))))
    
    (#t (err "Can't understand fn arg list" parm))))

; destructures a single parameter
(define (ac-complex-parm arg parm hygiene env cont)
  (cond
    ((or (null? parm) (eq? parm 'nil))
      ; as above, this is also doing it wrong but backwards-compatible
      (let ((rest-gs (gensym)))
        `((let ((,rest-gs (cdr ,arg)))
            ,@(cont rest-gs env)))))
    
    ((symbol? parm)
      (if hygiene
        (let ((head-gs (gensym))
              (rest-gs (gensym)))
          `((let ((,head-gs (car ,arg))
                  (,rest-gs (cdr ,arg)))
              ,@(cont rest-gs (cons (cons parm head-gs) env)))))
        (let ((rest-gs (gensym)))
          `((let ((,parm (car ,arg))
                  (,rest-gs (cdr ,arg)))
              ,@(cont rest-gs (cons (cons parm parm) env)))))))
    
    ((syntax-closure? parm)
      (ac-complex-parm arg
        (closure-expr parm) (closure-hygienic? parm) ((closure-env-param parm))
        (lambda (arg closure-env)
          (parameterize (((closure-env-param parm) closure-env))
            (cont arg env)))))
    
    ((pair? parm)
      (if (eq? (car parm) 'o)
        ; optional argument
        (let ((opt-parm (cadr parm))
              (opt-default (if (pair? (cddr parm)) (caddr parm) 'nil))
              (arg-gs (gensym))
              (opt-gs (gensym))
              (rest-gs (gensym)))
          `((let* ((,arg-gs (if (pair? ,arg) ,arg
                              (cons ,(ac opt-default env) ,arg)))
                   (,opt-gs (car ,arg-gs))
                   (,rest-gs (cdr ,arg-gs)))
              ,@(ac-complex-pattern opt-gs opt-parm hygiene env
                  (lambda (env) (cont rest-gs env))))))
        ; destructuring argument
        (let ((arg-gs (gensym))
              (rest-gs (gensym)))
          `((let ((,arg-gs (car ,arg))
                  (,rest-gs (cdr ,arg)))
              ,@(ac-complex-pattern arg-gs parm hygiene env
                  (lambda (env) (cont rest-gs env))))))))
    
    (#t (err "Can't understand fn arg list" parm))))

; destructures a single pattern (no optional parameters)
(define (ac-complex-pattern arg pat hygiene env cont)
  (cond
    ((or (null? pat) (eq? pat 'nil))
      ; as above, doing it wrong
      (cont env))
    
    ((symbol? pat)
      (if hygiene (cont (cons (cons pat arg) env))
        `((let ((,pat ,arg))
            ,@(cont (cons (cons pat pat) env))))))
    
    ((syntax-closure? pat)
      (ac-complex-pattern arg
        (closure-expr pat) (closure-hygienic? pat) ((closure-env-param pat))
        (lambda (closure-env)
          (parameterize (((closure-env-param pat) closure-env))
            (cont env)))))
    
    ((pair? pat)
      (let ((car-gs (gensym))
            (cdr-gs (gensym)))
        `((let ((,car-gs (car ,arg))
                (,cdr-gs (cdr ,arg)))
            ,@(ac-complex-pattern car-gs (car pat) hygiene env
                (lambda (env)
                  (ac-complex-pattern cdr-gs (cdr pat) hygiene env cont)))))))
    
    (#t (err "Can't understand fn arg list" pat))))

; (a b . c) -> (a b c)
; a -> (a)
(define (ac-arglist a)
  (cond ((null? a) '())
        ((symbol? a) (list a))
        ((symbol? (cdr a)) (list (car a) (cdr a)))
        (#t (cons (car a) (ac-arglist (cdr a))))))

(define (ac-body body env)
  (map (lambda (x) (ac x env)) body))

;; like ac-body, but spits out a nil expression if empty
(define (ac-body* body env)
  (if (null? body)
      (list (list 'quote 'nil))
      (ac-body body env)))

; (set v1 expr1 v2 expr2 ...)

(define (ac-set x env)
  `(begin ,@(ac-setn x env)))

(define (ac-setn x env)
  (if (null? x)
      '()
      (cons (ac-set1 (ac-macex (car x)) (ac (cadr x) env) env)
            (ac-setn (cddr x) env))))

; = replaced by set, which is only for vars
; = now defined in arc (is it?)
; name is to cause fns to have their arc names for debugging

(define (ac-set1 a b env)
  (if (symbol? a)
      (let ((name (string->symbol (string-append " " (symbol->string a)))))
        (list 'let `((,name ,b))
               (cond ((eqv? a 'nil) (err "Can't rebind nil"))
                     ((eqv? a 't) (err "Can't rebind t"))
                     ((lex a env) => (lambda (v) `(set! ,v ,name)))
                     (#t
								`(begin
									(namespace-set-variable-value! ',(ac-global-name a) ,name)
									(hash-table-put! *defs* ',(ac-global-name a) ',b))))
               name))
      (err "First arg to set must be a symbol" a)))
      
(define (ac-lset x env)
  (if (null? x) '()
      `(define ,(ac-macex (ac-global-name (car x)))
         ,(ac (cadr x) env))))

; compile a function call
; special cases for speed, to avoid compiled output like
;   (ar-apply __pr (list 1 2))
; which results in 1/2 the CPU time going to GC. Instead:
;   (ar-funcall2 __pr 1 2)
(define (ac-call fn args env)
  (let ((macfn (ac-macro? fn)))
    (cond (macfn
           (ac-mac-call macfn args env))
          ((and (pair? fn) (eqv? (car fn) 'fn))
           `(,(ac fn env) ,@(map (lambda (x) (ac x env)) args)))
          ((= (length args) 0)
           `(ar-funcall0 ,(ac fn env) ,@(map (lambda (x) (ac x env)) args)))
          ((= (length args) 1)
           `(ar-funcall1 ,(ac fn env) ,@(map (lambda (x) (ac x env)) args)))
          ((= (length args) 2)
           `(ar-funcall2 ,(ac fn env) ,@(map (lambda (x) (ac x env)) args)))
          ((= (length args) 3)
           `(ar-funcall3 ,(ac fn env) ,@(map (lambda (x) (ac x env)) args)))
          ((= (length args) 4)
           `(ar-funcall4 ,(ac fn env) ,@(map (lambda (x) (ac x env)) args)))
          (#t
           `(ar-apply ,(ac fn env)
                      (list ,@(map (lambda (x) (ac x env)) args)))))))

(define (ar-mac-call m args env)
  (parameterize ((callee-info (cons (hygienic?) (make-parameter env))))
    (apply m (map ac-niltree args))))

(define (ac-mac-call m args env)
  (ac (ac-denil (ar-mac-call m args env)) env))

; syntactic closures for (semi-)hygienic macros

(define callee-info (make-parameter #f))
(define hygienic? (make-parameter #f))

(define (syntax-closure? x)
  (and (vector? x) (eq? (vector-ref x 0) 'closure)))

(define (ac-closure x env)
  (parameterize ((hygienic? (closure-hygienic? x)))
    (ac (ac-denil (closure-expr x)) ((closure-env-param x)))))

; closure structure:
; #(closure <is-hygienic> <env-param> <expr>)

(define (closure-hygienic? x) (vector-ref x 1))
(define (closure-env-param x) (vector-ref x 2))
(define (closure-expr x) (vector-ref x 3))

(define (ar-callee-closure exp)
  (if (callee-info)
    (vector 'closure (car (callee-info)) (cdr (callee-info)) exp)
    (err "ar-callee-closure: not in macro, hygienic unquotation is meaningless")))

; returns #f or the macro function

(define (ac-macro? fn)
  (let ((fn (if (pair? fn) (ac-macex fn) fn)))
    (cond
     ((symbol? fn)
      (let ((v (namespace-variable-value (ac-global-name fn) 
                                         #t 
                                         (lambda () #f))))
        (if (and v
                 (ar-tagged? v)
                 (eq? (ar-type v) 'mac))
            (ar-rep v)
            #f)))
     ((and fn
           (ar-tagged? fn)
           (eq? (ar-type fn) 'mac))
      (ar-rep fn))
     (#t #f))))

; macroexpand the outer call of a form as much as possible

;; FIXME?: Actually a design point. This currently macroexpands at the top
;; level. If we made the current env a parameter, we could make it expand in the
;; current env. What semantic difference would this entail? Which is preferable?

(define (ac-macex e . once)
  (let ((m (ac-macro? (xcar e))))
    (if m
      (let ((expansion (ac-denil (ar-mac-call m (cdr e) '()))))
        (if (null? once) (ac-macex expansion) expansion))
      e)))

; macros return Arc lists, ending with NIL.
; but the Arc compiler expects Scheme lists, ending with '().
; what to do with (is x nil . nil) ?
;   the first nil ought to be replaced with 'NIL
;   the second with '()
; so the rule is: NIL in the car -> 'NIL, NIL in the cdr -> '().
;   NIL by itself -> NIL

(define (ac-denil x)
  (cond ((pair? x) (cons (ac-denil-car (car x)) (ac-denil-cdr (cdr x))))
        (#t x)))

(define (ac-denil-car x)
  (if (eq? x 'nil)
      'nil
      (ac-denil x)))

(define (ac-denil-cdr x)
  (if (eq? x 'nil)
      '()
      (ac-denil x)))

; is v lexically bound?
(define (lex v env)
  (let ((binding (assq v env)))
    (and binding (cdr binding))))

(define (xcar x)
  (and (pair? x) (car x)))

; #f and '() -> nil for a whole quoted list/tree.

(define (ac-niltree x)
  (cond ((pair? x) (cons (ac-niltree (car x)) (ac-niltree (cdr x))))
        ((or (eq? x #f) (eq? x '())) 'nil)
        (#t x)))

;(define (err msg . args)
;  (display msg)
;  (map (lambda (a) (display " ") (write a))  args)
;  (newline)
;  (xxundefined))

(define err error)  ; eli says need to remove xxundefined for speed

; run-time primitive procedures

(define (xdef a b)
  (namespace-set-variable-value! (ac-global-name a) b)
  b)

(define fn-signatures (make-hash-table 'equal))

; This is a replacement for xdef that stores opeator signatures.
; Haven't started using it yet.

(define (odef a parms b)
  (namespace-set-variable-value! (ac-global-name a) b)
  (hash-table-put! fn-signatures a (list parms))
  b)

(xdef 'sig fn-signatures)

; versions of car and cdr for parsing arguments for optional
; parameters, that yield nil for nil. maybe we should use
; full Arc car and cdr, so we can destructure more things

(define (ar-xcar x)
  (if (or (eqv? x 'nil) (eqv? x '()))
      'nil
      (car x)))
      
(define (ar-xcdr x)
  (if (or (eqv? x 'nil) (eqv? x '()))
      'nil
      (cdr x)))

; convert #f from a Scheme predicate to NIL.

(define (ar-nill x)
  (if (or (eq? x '()) (eq? x #f))
      'nil
      x))

; definition of falseness for Arc if.
; must include '() since sometimes Arc functions see
; Scheme lists (e.g. . body of a macro).

(define (ar-false? x)
  (or (eq? x 'nil) (eq? x '()) (eq? x #f)))

#|
   (if (eq? x 'nil) #t
      (if (eq? x '()) #t
          (not x)))
|#

; call a function or perform an array ref, hash ref, &c

; Non-fn donstants in functional position are valuable real estate, so
; should figure out the best way to exploit it.

(define (ar-apply fn args)
  (if (procedure? fn) (apply fn args)
      (begin
        (let ((call (hash-table-get (eval '__call*) (ar-type fn) #f)))
          (if call (apply call (cons (ar-rep fn) args))
              (err "Function call on inappropriate object" fn args))))))

(xdef 'apply (lambda (fn . args)
               (ar-apply fn (ar-apply-args args))))

; special cases of ar-apply for speed and to avoid consing arg lists
(define (ar-funcall0 fn)
  (if (procedure? fn)
      (fn)
      (ar-apply fn (list))))

(define (ar-funcall1 fn arg1)
  (if (procedure? fn)
      (fn arg1)
      (ar-apply fn (list arg1))))

(define (ar-funcall2 fn arg1 arg2)
  (if (procedure? fn)
      (fn arg1 arg2)
      (ar-apply fn (list arg1 arg2))))

(define (ar-funcall3 fn arg1 arg2 arg3)
  (if (procedure? fn)
      (fn arg1 arg2 arg3)
      (ar-apply fn (list arg1 arg2 arg3))))

(define (ar-funcall4 fn arg1 arg2 arg3 arg4)
  (if (procedure? fn)
      (fn arg1 arg2 arg3 arg4)
      (ar-apply fn (list arg1 arg2 arg3 arg4))))

; replace the nil at the end of a list with a '()

(define (ar-nil-terminate l)
  (if (or (eqv? l '()) (eqv? l 'nil))
      '()
      (cons (car l) (ar-nil-terminate (cdr l)))))

; turn the arguments to Arc apply into a list.
; if you call (apply fn 1 2 '(3 4))
; then args is '(1 2 (3 4 . nil) . ())
; that is, the main list is a scheme list.
; and we should return '(1 2 3 4 . ())
; was once (apply apply list (ac-denil args))
; but that didn't work for (apply fn nil)

(define (ar-apply-args args)
  (cond ((null? args) '())
        ((null? (cdr args)) (ar-nil-terminate (car args)))
        (#t (cons (car args) (ar-apply-args (cdr args))))))

(xdef 'cons cons)

(xdef 'car (lambda (x)
             (cond ((pair? x)     (car x))
                   ((eqv? x 'nil) 'nil)
                   ((eqv? x '())  'nil)
                   (#t            (err "Can't take car of" x)))))

(xdef 'cdr (lambda (x)
             (cond ((pair? x)     (cdr x))
                   ((eqv? x 'nil) 'nil)
                   ((eqv? x '())  'nil)
                   (#t            (err "Can't take cdr of" x)))))

; reduce? 

(define (pairwise pred args base)
  (let ((n (length args)))
    (cond ((< n 2) base)
          ((= n 2) (apply pred args))
          (#t (and (pred (car args) (cadr args))
                   (pairwise pred (cdr args) base))))))

(define (tnil x) (if x 't 'nil))

; not quite right, because behavior of underlying eqv unspecified
; in many cases according to r5rs
; do we really want is to ret t for distinct strings?

(xdef 'is (lambda args
            (tnil (or (all (lambda (a) (eqv? (car args) a)) (cdr args))
                    (and (all string? args)
                         (apply string=? args))
                    (all ar-false? args)))))

(xdef 'err err)
(xdef 'nil 'nil)
(xdef 't   't)

(define (all test seq)
  (or (null? seq) 
      (and (test (car seq)) (all test (cdr seq)))))

(define (arc-list? x) (or (pair? x) (eqv? x 'nil) (eqv? x '())))
      
; generic +: strings, lists, numbers.
; problem with generic +: what to return when no args?
; could even coerce based on type of first arg...

(xdef '+ (lambda args
           (cond ((null? args) 0)
                 ((all string? args) 
                  (apply string-append args))
                 ((all arc-list? args) 
                  (ac-niltree (apply append (map ar-nil-terminate args))))
                 (#t (apply + args)))))

(xdef '- -)
(xdef '* *)
(xdef '/ /)
(xdef 'mod modulo)
(xdef 'quotient quotient)
(xdef 'expt expt)
(xdef 'sqrt sqrt)
(xdef 'log log)

; generic comparison

(define (arc> . args)
  (cond ((all number? args) (apply > args))
        ((all string? args) (pairwise string>? args #f))
        ((all symbol? args) (pairwise (lambda (x y)
                                        (string>? (symbol->string x) 
                                                  (symbol->string y)))
                                      args
                                      #f))
        ((all char?   args) (pairwise char>?   args #f))
        (#t                 (apply > args))))

(xdef '>  (lambda args (tnil (apply arc> args))))

(define (arc< . args)
  (cond ((all number? args) (apply < args))
        ((all string? args) (pairwise string<? args #f))
        ((all symbol? args) (pairwise (lambda (x y)
                                        (string<? (symbol->string x) 
                                                  (symbol->string y)))
                                      args
                                      #f))
        ((all char?   args) (pairwise char<?   args #f))
        (#t                 (apply < args))))

(xdef '<  (lambda args (tnil (apply arc< args))))

(xdef 'len (lambda (x)
             (cond ((string? x) (string-length x))
                   ((vector? x) (vector-length x))
                   ((hash-table? x) (hash-table-count x))
                   (#t (length (ar-nil-terminate x))))))

(define (ar-tagged? x)
  (and (vector? x) (eq? (vector-ref x 0) 'tagged)))

(define (ar-tag type rep)
  (cond ((eqv? (ar-type rep) type) rep)
        (#t (vector 'tagged type rep))))

(xdef 'annotate ar-tag)

; (type nil) -> sym

(define (ar-type x)
  (cond ((ar-tagged? x)     (vector-ref x 1))
        ((pair? x)          'cons)
        ((symbol? x)        'sym)
        ((null? x)          'sym)
        ((procedure? x)     'fn)
        ((char? x)          'char)
        ((string? x)        'string)
        ((integer? x)       'int)
        ((number? x)        'num)     ; unsure about this
        ((vector? x)        'vec)
        ((hash-table? x)    'table)
        ((output-port? x)   'output)
        ((input-port? x)    'input)
        ((tcp-listener? x)  'socket)
        ((exn? x)           'exception)
        ((regexp? x)        're)
        ((thread? x)        'thread)
        ((thread-cell? x)   'thread-local)
        ((semaphore? x)     'sema)
        ((bytes? x)         'bytes)
        (#t                 (err "Type: unknown type" x))))
(xdef 'type ar-type)

(define (ar-rep x)
  (if (ar-tagged? x)
      (vector-ref x 2)
      x))

(xdef 'rep ar-rep)

(xdef 'uniq gensym)

(xdef 'ccc call-with-current-continuation)

(xdef 'infile  open-input-file)

(xdef 'outfile (lambda (f . args) 
                 (open-output-file f 
                                   'text
                                   (if (equal? args '(append))
                                       'append
                                       'truncate))))

(xdef 'instring  open-input-string)
(xdef 'outstring open-output-string)

; use as general fn for looking inside things

(xdef 'inside get-output-string)

(xdef 'close (lambda args
               (map (lambda (p)
                      (cond ((input-port? p)   (close-input-port p))
                            ((output-port? p)  (close-output-port p))
                            ((tcp-listener? p) (tcp-close p))
                            (#t (err "Can't close " p))))
                    args)
               'nil))

(xdef 'stdout current-output-port)  ; should be a vars
(xdef 'stdin  current-input-port) 
(xdef 'stderr current-error-port)

(xdef 'call-w/stdout
      (lambda (port thunk)
        (parameterize ((current-output-port port)) (thunk))))

(xdef 'call-w/stdin
      (lambda (port thunk)
        (parameterize ((current-input-port port)) (thunk))))

; (readc stream)
; nil stream means stdout
; returns nil on eof

(xdef 'readc (lambda (str) 
               (let ((p (if (ar-false? str)
                            (current-input-port)
                            str)))
                 (let ((c (read-char p)))
                   (if (eof-object? c) 'nil c)))))

(xdef 'readb (lambda (str)
               (let ((p (if (ar-false? str)
                            (current-input-port)
                            str)))
                 (let ((c (read-byte p)))
                   (if (eof-object? c) 'nil c)))))

(xdef 'peekc (lambda (str) 
               (let ((p (if (ar-false? str)
                            (current-input-port)
                            str)))
                 (let ((c (peek-char p)))
                   (if (eof-object? c) 'nil c)))))

(xdef 'writec (lambda (c . args) 
                (write-char c 
                            (if (pair? args) 
                                (car args) 
                                (current-output-port)))
                c))

(xdef 'writeb (lambda (b . args) 
                (write-byte b 
                            (if (pair? args) 
                                (car args) 
                                (current-output-port)))
                b))

(define (printwith f args)
  (let ((port (if (> (length args) 1)
                  (cadr args)
                  (current-output-port))))
    (when (pair? args)
      (f (ac-denil (car args)) port))
    (flush-output port))
    'nil)

(xdef 'write (lambda args (printwith write   args)))
(xdef 'disp  (lambda args (printwith display args)))

; patch: allows the reader to be changed at run time
(define current-reader* read)

(define (set-reader reader) 
    (set! current-reader* 
          (lambda args 
            (flush-output) 
            (ac-denil (if (eq? 1 (length args)) 
                          (reader (car args)) 
                          (reader))))))

; sread = scheme read. eventually replace by writing read

(xdef 'sread (lambda (p eof)
               (let ((expr (current-reader* p)))
                 (if (eof-object? expr) eof expr))))

; these work in PLT but not scheme48

(define char->ascii char->integer)
(define ascii->char integer->char)

(xdef 'coerce (lambda (x type . args)
                (cond 
                  ((ar-tagged? x) (err "Can't coerce annotated object"))
                  ((eqv? type (ar-type x)) x)

                  ((char? x)      (case type
                                    ((int)    (char->ascii x))
                                    ((string) (string x))
                                    ((sym)    (string->symbol (string x)))
                                    (else     (err "Can't coerce" x type))))
                  ((integer? x)   (case type
                                    ((char)   (ascii->char x))
                                    ((string) (apply number->string x args))
                                    (else     (err "Can't coerce" x type))))
                  ((number? x)    (case type
                                    ((int)    (round x))
                                    ((char)   (ascii->char (round x)))
                                    ((string) (apply number->string x args))
                                    (else     (err "Can't coerce" x type))))
                  ((string? x)    (case type
                                    ((sym)    (string->symbol x))
                                    ((cons)   (ac-niltree (string->list x)))
                                    ((int)    (or (apply string->number x args)
                                                  (err "Can't coerce" x type)))
                                    (else     (err "Can't coerce" x type))))
                  ((pair? x)      (case type
                                    ((string) (list->string
                                               (ar-nil-terminate x)))   
                                    (else     (err "Can't coerce" x type))))
                  ((eqv? x 'nil)  (case type
                                    ((string) "")
                                    (else     (err "Can't coerce" x type))))
                  ((symbol? x)    (case type 
                                    ((string) (symbol->string x))
                                    (else     (err "Can't coerce" x type))))
                  (#t             x))))

(xdef 'open-socket  (lambda (num) (tcp-listen num 50 #t))) 

; the 2050 means http requests currently capped at 2 meg
; http://list.cs.brown.edu/pipermail/plt-scheme/2005-August/009414.html

(xdef 'socket-accept (lambda (s)
                       (call-with-values
                         (lambda () (tcp-accept s))
                         (lambda (in out)
                           (list (make-limited-input-port in 100000 #t)
                                 out
                                 (let-values (((us them) (tcp-addresses out)))
                                   them))))))

(xdef 'new-thread thread)
(xdef 'kill-thread kill-thread)
(xdef 'break-thread break-thread)

(define (wrapnil f) (lambda args (apply f args) 'nil))

(xdef 'sleep (wrapnil sleep))

; Will system "execute" a half-finished string if thread killed
; in the middle of generating it?  

(xdef 'system (wrapnil system))

(xdef 'pipe-from (lambda (cmd)
                   (let ((tf (ar-tmpname)))
                     (system (string-append cmd " > " tf))
                     (let ((str (open-input-file tf)))
                       (system (string-append "rm -f " tf))
                       str))))
                   
(define (ar-tmpname)
  (call-with-input-file "/dev/urandom"
    (lambda (rstr)
      (do ((s "/tmp/")
           (c (read-char rstr) (read-char rstr))
           (i 0 (+ i 1)))
          ((>= i 16) s)
        (set! s (string-append s
                               (string
                                 (integer->char
                                   (+ (char->integer #\a)
                                      (modulo
                                        (char->integer (read-char rstr))
                                        26))))))))))

(xdef 'vec (lambda (n) (make-vector n 'nil)))

(xdef 'vec-ref vector-ref)
  
(xdef 'vec-set vector-set!)

; PLT scheme provides only eq? and equal? hash tables,
; we need the latter for strings.

(xdef 'table (lambda () (make-hash-table 'equal)))

;(xdef 'table (lambda args
;               (fill-table (make-hash-table 'equal) 
;                           (if (pair? args) (ac-denil (car args)) '()))))
                   
(define (fill-table h pairs)
  (if (eq? pairs '())
      h
      (let ((pair (car pairs)))
        (begin (hash-table-put! h (car pair) (cadr pair))
               (fill-table h (cdr pairs))))))

(xdef 'maptable (lambda (fn table)               ; arg is (fn (key value) ...)
                  (hash-table-for-each table fn)
                  table))

(xdef 'protect (lambda (during after)
                  (dynamic-wind (lambda () #t) during after)))

; need to use a better seed

(xdef 'rand random)

(xdef 'dir (lambda (name) (map path->string (directory-list name))))

(xdef 'file-exists (lambda (name)
                     (if (file-exists? name) name 'nil)))

(xdef 'dir-exists (lambda (name)
                     (if (directory-exists? name) name 'nil)))

(xdef 'rmfile (wrapnil delete-file))

; top level read-eval-print
; tle kept as a way to get a break loop when a scheme err

(define (arc-eval expr) 
  (eval (ac expr '()) (interaction-environment)))

(define (tle)
  (display "Arc> ")
  (let ((expr (current-reader*)))
    (when (not (eqv? expr ':a))
      (write (arc-eval expr))
      (newline)
      (tle))))

(define last-condition* #f)

(define (tl)
  (display "Use (quit) to quit, (tl) to return here after an interrupt.\n")
  (tl2))

(define (tl2)
  (display "arc> ")
  (on-err (lambda (c) 
            (set! last-condition* c)
            (display "Error: ")
            (write (exn-message c))
            (newline)
            (tl2))
    (lambda ()
      (let ((expr (current-reader*)))
        (if (eqv? expr ':a)
            'done
            (let ((val (arc-eval expr)))
              (arc-eval `(input-history-update ',expr))
              (arc-eval `(output-history-update ',val))
              (write (ac-denil val))
              (namespace-set-variable-value! '__that val)
              (namespace-set-variable-value! '__thatexpr expr)
              (newline)
              (tl2)))))))

(define (aload1 p)
  (let ((x (current-reader* p)))
    (if (eof-object? x)
        #t
        (begin
          (arc-eval x)
          (aload1 p)))))

(define (atests1 p)
  (let ((x (current-reader* p)))
    (if (eof-object? x)
        #t
        (begin
          (write x)
          (newline)
          (let ((v (arc-eval x)))
            (if (ar-false? v)
                (begin
                  (display "  FAILED")
                  (newline))))
          (atests1 p)))))

(define (aload filename)
  (call-with-input-file filename aload1))

(define (test filename)
  (call-with-input-file filename atests1))

(define (acompile1 ip op)
  (let ((x (current-reader* ip)))
    (if (eof-object? x)
        #t
        (let ((scm (ac x '())))
          (eval scm (interaction-environment))
          (pretty-print scm op)
          (newline op)
          (newline op)
          (acompile1 ip op)))))

; compile xx.arc to xx.arc.scm
; useful to examine the Arc compiler output
(define (acompile inname)
  (let ((outname (string-append inname ".scm")))
    (if (file-exists? outname)
        (delete-file outname))
    (call-with-input-file inname
      (lambda (ip)
        (call-with-output-file outname 
          (lambda (op)
            (acompile1 ip op)))))))

(xdef 'macex (lambda (e) (ac-macex (ac-denil e))))

(xdef 'macex1 (lambda (e) (ac-macex (ac-denil e) 'once)))

(xdef 'eval (lambda (e)
              (eval (ac (ac-denil e) '()) (interaction-environment))))

; If an err occurs in an on-err expr, no val is returned and code
; after it doesn't get executed.  Not quite what I had in mind.

(define (on-err errfn f) 
  ((call-with-current-continuation 
     (lambda (k) 
       (lambda () 
         (with-handlers ((exn:fail? (lambda (c) 
                                      (k (lambda () (errfn c)))))) 
                        (f)))))))
(xdef 'on-err on-err)

(define (disp-to-string x)
  (let ((o (open-output-string)))
    (display x o)
    (close-output-port o)
    (get-output-string o)))

(xdef 'details (lambda (c)
                 (disp-to-string (exn-message c))))

(xdef 'scar (lambda (x val) 
              (if (string? x) 
                  (string-set! x 0 val)
                  (set-car! x val))
              val))

(xdef 'scdr (lambda (x val) 
              (if (string? x)
                  (err "Can't set cdr of a string" x)
                  (set-cdr! x val))
              val))

; When and if cdr of a string returned an actual (eq) tail, could
; say (if (string? x) (string-replace! x val 1) ...) in scdr, but
; for now would be misleading to allow this, because fails for cddr.

(define (string-replace! str val index)
  (if (eqv? (string-length val) (- (string-length str) index))
      (do ((i index (+ i 1)))
          ((= i (string-length str)) str)
        (string-set! str i (string-ref val (- i index))))
      (err "Length mismatch between strings" str val index)))

(xdef 'sref (lambda (com val ind) ; later make ind rest arg
              (cond ((hash-table? com)  (if (eqv? val 'nil)
                                            (hash-table-remove! com ind)
                                            (hash-table-put! com ind val)))
                    ((vector? com) (vector-set! com ind val))
                    ((string? com) (string-set! com ind val))
                    ((pair? com)   (nth-set! com ind val))
                    (#t (err "Can't set reference " com ind val)))
              val))

(xdef 'ref (lambda (com ind)
             (cond ((hash-table? com) (hash-table-get com ind 'nil))
                   ((vector? com) (vector-ref com ind))
                   ((string? com) (string-ref com ind))
                   ((pair? com)   (list-ref   com ind))
                   (#t (err "Can't get reference " com ind)))))

(define (nth-set! lst n val)
  (set-car! (list-tail lst n) val))

; rewrite to pass a (true) gensym instead of #f in case var bound to #f

(define (bound? arcname)
  (namespace-variable-value (ac-global-name arcname)
                            #t
                            (lambda () #f)))

(xdef 'bound (lambda (x) (tnil (bound? x))))

(xdef 'newstring make-string)

(xdef 'trunc (lambda (x) (inexact->exact (truncate x))))

(xdef 'exact (lambda (x) 
               (tnil (and (integer? x) (exact? x)))))

(xdef 'msec                         current-milliseconds)
(xdef 'current-process-milliseconds current-process-milliseconds)
(xdef 'current-gc-milliseconds      current-gc-milliseconds)

(xdef 'seconds current-seconds)

(print-hash-table #t)

(xdef 'client-ip (lambda (port) 
                   (let-values (((x y) (tcp-addresses port)))
                     y)))

; make sure only one thread at a time executes anything
; inside an atomic-invoke. atomic-invoke is allowed to
; nest within a thread; the thread-cell keeps track of
; whether this thread already holds the lock.

(define ar-the-sema (make-semaphore 1))

(define ar-sema-cell (make-thread-cell #f))

(xdef 'atomic-invoke
  (lambda (f)
    (if (thread-cell-ref ar-sema-cell)
        (ar-apply f '())
        (dynamic-wind
            (lambda () (thread-cell-set! ar-sema-cell #t))
            (lambda ()
              (call-with-semaphore ar-the-sema
                                   (lambda () (ar-apply f '()))))
            (lambda () (thread-cell-set! ar-sema-cell #f))))))

(xdef 'dead (lambda (x) (tnil (thread-dead? x))))

; Added because Mzscheme buffers output.  Not sure if want as official
; part of Arc.

;(xdef 'flushout (lambda () (flush-output) 't))

(xdef 'ssyntax (lambda (x) (tnil (default-ssyntax? x))))

(xdef 'ssexpand (lambda (x)
                  (if (symbol? x) (default-expand-ssyntax x) x)))

(xdef 'which-os system-type)

(xdef 'make-directory make-directory)
(xdef 'make-directory* make-directory*)
(xdef 'datetbl
  (lambda (t)
    (let ((dat (seconds->date t))
          (tbl (make-hash-table 'equal)))
      (hash-table-put! tbl 'year  (date-year dat))
      (hash-table-put! tbl 'month (date-month dat))
      (hash-table-put! tbl 'day   (date-day dat))
      tbl)))

(xdef 'seval (lambda (x) (eval (ac-denil x))))

(xdef 'quit exit)

; Added outgoing tcp/ip ports
; (= socket (connect-socket host port))
; (= outport (car (cdr socket))
; (= inport (car socket))
; (write "hello" outport)
; (read inport)
(xdef 'connect-socket (lambda (host port)
       (let-values ([(in out) (tcp-connect host port)]) (list in out))))
(xdef 'flush-socket (lambda (s) (flush-output s)))

(xdef 'pipe (lambda ()
              (call-with-values make-pipe
                (lambda (x y)
                  (cons x (cons y 'nil))))))
(xdef 'pipe-len pipe-content-length)

(xdef 'thread-local (lambda ()
      (make-thread-cell 'nil #t)))
; incompatible with current sref and ref, since they both
; expect an argument.
(xdef 'thread-local-ref thread-cell-ref)
(xdef 'thread-local-set (lambda (c v)
                          (thread-cell-set! c v)
                          v))

(xdef 'sema (lambda () (make-semaphore)))
(xdef 'sema-wait (wrapnil semaphore-wait))
(xdef 'sema-post (wrapnil semaphore-post))
(xdef 'sync sync)
(xdef 'synct (lambda (timeout . events)
               (if (eqv? timeout 'nil)
                   (apply sync events)
                   (let ((rv (apply sync/timeout timeout events)))
                     (if rv rv 'nil)))))

)
