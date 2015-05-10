; Arc Compiler.

(module ac mzscheme

(provide (all-defined))
(require openssl)
(require (lib "port.ss"))
(require (lib "process.ss"))
(require (lib "pretty.ss"))
(require (lib "foreign.ss"))
(unsafe!)

(define main-namespace (current-namespace))

(define (ac-global-name s)
  (string->symbol (string-append "_" (symbol->string s))))

(define-syntax defarc
  (syntax-rules ()
    ((defarc (name . args) body ...)
     (defarc name (name . args) body ...))
    ((defarc arc-name (scheme-name . args) body ...)
     (begin
       (xdef arc-name (lambda args body ...))
       (defarc arc-name scheme-name)))
    ((defarc arc-name scheme-name)
     (define (scheme-name . args)
       ; support arc-exec
       (apply (parameterize ((current-namespace main-namespace))
                (namespace-variable-value (ac-global-name 'arc-name)))
              args)))
    ((defarc name)
     (defarc name name))))

; compile an Arc expression into a Scheme expression,
; both represented as s-expressions.
; env is a list of lexically bound variables, which we
; need in order to decide whether set should create a global.

(defarc (ac s env)
  (cond ((string? s) (ac-string s env))
        ((literal? s) s)
        ((keyword? s) s)
        ((eqv? s 'nil) (list 'quote 'nil))
        ((ssyntax? s) (ac (expand-ssyntax s) env))
        ((symbol? s) (ac-var-ref s env))
        ((ssyntax? (xcar s)) (ac (cons (expand-ssyntax (car s)) (cdr s)) env))
        ((eq? (xcar s) '$) (ac-$ (cadr s) env))
        ((eq? (xcar s) 'quote) (list 'quote (ac-niltree (cadr s))))
        ((and (eq? (xcar s) 'quasiquote)
              (not (ac-macro? 'quasiquote)))
         (ac-qq (cadr s) env))
        ((eq? (xcar s) 'if) (ac-if (cdr s) env))
        ((eq? (xcar s) 'fn) (ac-fn (cadr s) (cddr s) env))
        ((eq? (xcar s) 'assign) (ac-set (cdr s) env))
        ; the next three clauses could be removed without changing semantics
        ; ... except that they work for macros (so prob should do this for
        ; every elt of s, not just the car)
        ((eq? (xcar (xcar s)) 'compose) (ac (decompose (cdar s) (cdr s)) env))
        ((eq? (xcar (xcar s)) 'complement)
         (ac (list 'no (cons (cadar s) (cdr s))) env))
        ((eq? (xcar (xcar s)) 'andf) (ac-andf s env))
        ((pair? s) (ac-call (car s) (cdr s) env))
        (#t (err "Bad object in expression" s))))

(define (ac-string s env)
  (if (ar-bflag 'atstrings)
      (if (atpos s 0)
          (ac (cons 'string (map (lambda (x)
                                   (if (string? x)
                                       (unescape-ats x)
                                       x))
                                 (codestring s)))
              env)
          (list string-copy (unescape-ats s)))
      (list string-copy s)))     ; avoid immutable strings

(defarc ac-literal (literal? x)
  (or (boolean? x)
      (char? x)
      (string? x)
      (number? x)
      (eq? x '())))

(define (ssyntax? x)
  (and (symbol? x)
       (not (or (eqv? x '+) (eqv? x '++) (eqv? x '_)))
       (let ((name (symbol->string x)))
         (has-ssyntax-char? name (- (string-length name) 1)))))

(define (has-ssyntax-char? string i)
  (and (>= i 0)
       (or (let ((c (string-ref string i)))
             (or (eqv? c #\:) (eqv? c #\~)
                 (eqv? c #\&)
                 ;(eqv? c #\_)
                 (eqv? c #\.)  (eqv? c #\!)))
           (has-ssyntax-char? string (- i 1)))))

(define (read-from-string str)
  (let ((port (open-input-string str)))
    (let ((val (read port)))
      (close-input-port port)
      val)))

; Though graphically the right choice, can't use _ for currying
; because then _!foo becomes a function.  Maybe use <>.  For now
; leave this off and see how often it would have been useful.

; Might want to make ~ have less precedence than &, because
; ~foo&bar prob should mean (andf (complement foo) bar), not
; (complement (andf foo bar)).

(define (expand-ssyntax sym)
  ((cond ((eqv? (car (symbol->chars sym)) #\:) expand-keyword)
         ((or (insym? #\: sym) (insym? #\~ sym)) expand-compose)
         ((or (insym? #\. sym) (insym? #\! sym)) expand-sexpr)
         ((insym? #\& sym) expand-and)
     ;   ((insym? #\_ sym) expand-curry)
         (#t (error "Unknown ssyntax" sym)))
   sym))

; turn common-lisp style :keywords into racket #:keywords
(define (expand-keyword sym)
  (string->keyword (list->string (cdr (symbol->chars sym)))))

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

(define (expand-and sym)
  (let ((elts (map chars->value
                   (tokens (lambda (c) (eqv? c #\&))
                           (symbol->chars sym)
                           '()
                           '()
                           #f))))
    (if (null? (cdr elts))
        (car elts)
        (cons 'andf elts))))

; How to include quoted arguments?  Can't treat all as quoted, because
; never want to quote fn given as first.  Do we want to allow quote chars
; within symbols?  Could be ugly.

; If release, fix the fact that this simply uses v0... as vars.  Should
; make these vars gensyms.

(define (expand-curry sym)
  (let ((expr (exc (map (lambda (x)
                          (if (pair? x) (chars->value x) x))
                        (tokens (lambda (c) (eqv? c #\_))
                                (symbol->chars sym)
                                '()
                                '()
                                #t))
                    0)))
    (list 'fn
          (keep (lambda (s)
                  (and (symbol? s)
                       (eqv? (string-ref (symbol->string s) 0)
                             #\v)))
                expr)
          expr)))

(define (keep f xs)
  (cond ((null? xs) '())
        ((f (car xs)) (cons (car xs) (keep f (cdr xs))))
        (#t (keep f (cdr xs)))))

(define (exc elts n)
  (cond ((null? elts)
         '())
        ((eqv? (car elts) #\_)
         (cons (string->symbol (string-append "v" (number->string n)))
               (exc (cdr elts) (+ n 1))))
        (#t
         (cons (car elts) (exc (cdr elts) n)))))

(define (expand-sexpr sym)
  (build-sexpr (reverse (tokens (lambda (c) (or (eqv? c #\.) (eqv? c #\!)))
                                (symbol->chars sym)
                                '()
                                '()
                                #t))
               sym))

(define (build-sexpr toks orig)
  (cond ((null? toks)
         'get)
        ((null? (cdr toks))
         (chars->value (car toks)))
        (#t
         (list (build-sexpr (cddr toks) orig)
               (if (eqv? (cadr toks) #\!)
                 (list 'quote (chars->value (car toks)))
                 (if (or (eqv? (car toks) #\.) (eqv? (car toks) #\!))
                   (err "Bad ssyntax" orig)
                   (chars->value (car toks))))))))

(define (insym? char sym) (member char (symbol->chars sym)))

(define (symbol->chars x) (string->list (symbol->string x)))

(define (chars->value chars) (read-from-string (list->string chars)))

(define (tokens test source token acc keepsep?)
  (cond ((null? source)
         (reverse (if (pair? token)
                      (cons (reverse token) acc)
                      acc)))
        ((test (car source))
         (tokens test
                 (cdr source)
                 '()
                 (let ((rec (if (null? token)
                              acc
                              (cons (reverse token) acc))))
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

(defarc (ac-defined-var? s)
  #f)

(define (ac-var-ref s env)
  (cond ((lex? s env)        s)
        ((ac-defined-var? s) (list (ac-global-name s)))
        (#t                  (ac-global-name s))))

; lowering into mzscheme, with (unquote <foo>) lifting us back into arc

(define (ac-$ args env)
  (ac-qqx args
    (lambda (x) (ac x env))
    (lambda (x) (error 'ac-$ "Can't use ,@ from within $ in: ~a" args))))

; quasiquote

(define (ac-qq args env)
  (list 'quasiquote (ac-qqx args
                      (lambda (x) (list 'unquote (ac x env)))
                      (lambda (x) (list 'unquote-splicing
                                    (list 'ar-denil-last (ac x env)))))))

; process the argument of a quasiquote. keep track of
; depth of nesting. handle unquote only at top level (level = 1).
; complete form, e.g. x or (fn x) or (unquote (fn x))

(define (ac-qqx x unq splice)
  (cond
    ((not (pair? x)) x)
    ((eqv? (car x) 'unquote) (unq (cadr x)))
    ((eqv? (car x) 'unquote-splicing) (splice (cadr x)))
    ((eqv? (car x) 'quasiquote)
      (list 'quasiquote
        (ac-qqx (cadr x)
          (lambda (e) (list 'unquote (ac-qqx e unq splice)))
          (lambda (e) (list 'unquote-splicing (ac-qqx e unq splice))))))
    (#t (imap (lambda (e) (ac-qqx e unq splice)) x))))

; like map, but don't demand '()-terminated list

(define (imap f l)
  (cond ((pair? l)
         (cons (f (car l)) (imap f (cdr l))))
        ((null? l)
         '())
        (#t (f l))))

; (if) -> nil
; (if x) -> x
; (if t a ...) -> a
; (if nil a b) -> b
; (if nil a b c) -> (if b c)

(define (ac-if args env)
  (cond ((null? args) ''nil)
        ((null? (cdr args)) (ac (car args) env))
        (#t `(if (not (ar-false? ,(ac (car args) env)))
                 ,(ac (cadr args) env)
                 ,(ac-if (cddr args) env)))))

(define (ac-dbname! name env)
  (if (symbol? name)
      (cons (list name) env)
      env))

(define (ac-dbname env)
  (cond ((null? env) #f)
        ((pair? (car env)) (caar env))
        (#t (ac-dbname (cdr env)))))

; translate fn directly into a lambda if it has ordinary
; parameters, otherwise use a rest parameter and parse it.

(define (ac-fn args body env)
  (if (ac-complex-args? args)
      (ac-complex-fn args body env)
      (ac-nameit
       (ac-dbname env)
       `(lambda ,(let ((a (ac-denil args))) (if (eqv? a 'nil) '() a))
          ,@(ac-body* body (append (ac-arglist args) env))))))

; does an fn arg list use optional parameters or destructuring?
; a rest parameter is not complex

(define (ac-complex-args? args)
  (cond ((eqv? args '()) #f)
        ((symbol? args) #f)
        ((and (pair? args) (symbol? (car args)))
         (ac-complex-args? (cdr args)))
        (#t #t)))

; translate a fn with optional or destructuring args
; (fn (x (o y x) (o z 21) (x1 x2) . rest) ...)
; arguments in top-level list are mandatory (unless optional),
; but it's OK for parts of a list you're destructuring to
; be missing.

(define (ac-complex-fn args body env)
  (let* ((ra (gensym))
         (z (ac-complex-args args env ra #t)))
    `(lambda ,ra
       (let* ,z
         ,@(ac-body* body (append (ac-complex-getargs z) env))))))

; returns a list of two-element lists, first is variable name,
; second is (compiled) expression. to be used in a let.
; caller should extract variables and add to env.
; ra is the rest argument to the fn.
; is-params indicates that args are function arguments
;   (not destructuring), so they must be passed or be optional.

(define (ac-complex-args args env ra is-params)
  (cond ((or (eqv? args '()) (eqv? args 'nil)) '())
        ((symbol? args) (list (list args ra)))
        ((pair? args)
         (let* ((x (if (and (pair? (car args)) (eqv? (caar args) 'o))
                       (ac-complex-opt (cadar args)
                                       (if (pair? (cddar args))
                                           (caddar args)
                                           'nil)
                                       env
                                       ra)
                       (ac-complex-args
                        (car args)
                        env
                        (if is-params
                            `(car ,ra)
                            `(ar-xcar ,ra))
                        #f)))
                (xa (ac-complex-getargs x)))
           (append x (ac-complex-args (cdr args)
                                      (append xa env)
                                      `(ar-xcdr ,ra)
                                      is-params))))
        (#t (err "Can't understand fn arg list" args))))

; (car ra) is the argument
; so it's not present if ra is nil or '()

(define (ac-complex-opt var expr env ra)
  (list (list var `(if (pair? ,ra) (car ,ra) ,(ac expr env)))))

; extract list of variables from list of two-element lists.

(define (ac-complex-getargs a)
  (map (lambda (x) (car x)) a))

; (a b . c) -> (a b c)
; a -> (a)

(define (ac-arglist a)
  (cond ((null? a) '())
        ((symbol? a) (list a))
        ((symbol? (cdr a)) (list (car a) (cdr a)))
        (#t (cons (car a) (ac-arglist (cdr a))))))

(define (ac-body body env)
  (map (lambda (x) (ac x env)) body))

; like ac-body, but spits out a nil expression if empty

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
      (cons (ac-set1 (ac-macex (car x)) (cadr x) env)
            (ac-setn (cddr x) env))))

; trick to tell Scheme the name of something, so Scheme
; debugging and profiling make more sense.

(define (ac-nameit name v)
  (if (symbol? name)
      (let ((n (string->symbol (string-append " " (symbol->string name)))))
        (list 'let `((,n ,v)) n))
      v))

; = replaced by set, which is only for vars
; = now defined in arc (is it?)
; name is to cause fns to have their arc names for debugging

(define (ac-set1 a b1 env)
  (if (symbol? a)
      (let ((b (ac b1 (ac-dbname! a env))))
        (list 'let `((zz ,b))
               (cond ((eqv? a 'nil) (err "Can't rebind nil"))
                     ((eqv? a 't) (err "Can't rebind t"))
                     ((lex? a env) `(set! ,a zz))
                     ((ac-defined-var? a) `(,(ac-global-name a) zz))
                     ; support arc-exec
                     (#t `(set! ,(ac-global-name a) zz)))
               'zz))
      (err "First arg to set must be a symbol" a)))

; given a list of Arc expressions, return a list of Scheme expressions.
; for compiling passed arguments.

(define (ac-args names exprs env)
  (if (null? exprs)
      '()
      (cons (ac (car exprs)
                (ac-dbname! (if (pair? names) (car names) #f) env))
            (ac-args (if (pair? names) (cdr names) '())
                     (cdr exprs)
                     env))))

; generate special fast code for ordinary two-operand
; calls to the following functions. this is to avoid
; calling e.g. ar-is with its &rest and apply.

(define ac-binaries
  '((is ar-is2)
    (< ar-<2)
    (> ar->2)
    (+ ar-+2)))

; (foo bar) where foo is a global variable bound to a procedure.

(define (ac-global-call fn args env)
  (cond ((and (assoc fn ac-binaries) (= (length args) 2))
         `(,(cadr (assoc fn ac-binaries)) ,@(ac-args '() args env)))
        (#t
         `(,(ac-global-name fn) ,@(ac-args '() args env)))))

; compile a function call
; special cases for speed, to avoid compiled output like
;   (ar-apply _pr (list 1 2))
; which results in 1/2 the CPU time going to GC. Instead:
;   (ar-funcall2 _pr 1 2)
; and for (foo bar), if foo is a reference to a global variable,
;   and it's bound to a function, generate (foo bar) instead of
;   (ar-funcall1 foo bar)

(define (ac-call fn args env)
  (let ((macfn (ac-macro? fn)))
    (cond (macfn
           (ac-mac-call macfn args env))
          ((and (pair? fn) (eqv? (car fn) 'fn))
           `(,(ac fn env) ,@(ac-args (cadr fn) args env)))
          ((and (ar-bflag 'direct-calls) (symbol? fn) (not (lex? fn env)) (bound? fn)
                ; support arc-exec
                (procedure? (arc-eval fn)))
           (ac-global-call fn args env))
          (#t
           `((ar-coerce ,(ac fn env) 'fn)
             ,@(map (lambda (x) (ac x env)) args))))))

(define (ac-mac-call m args env)
  (let ((x1 (apply m (map ac-niltree args))))
    (let ((x2 (ac (ac-denil x1) env)))
      x2)))

; returns #f or the macro function

(define (ac-macro? fn)
  (if (symbol? fn)
    ; support arc-exec
    (let ((v (and (bound? fn) (arc-eval fn))))
      (if (and v
               (ar-tagged? v)
               (eq? (ar-type v) 'mac))
          (ar-rep v)
          #f))
    #f))

; macroexpand the outer call of a form as much as possible

(define (ac-macex e . once)
  (if (pair? e)
      (let ((m (ac-macro? (car e))))
        (if m
            (let ((expansion (ac-denil (apply m (map ac-niltree (cdr e))))))
              (if (null? once) (ac-macex expansion) expansion))
            e))
      e))

; macros return Arc lists, ending with NIL.
; but the Arc compiler expects Scheme lists, ending with '().
; what to do with (is x nil . nil) ?
;   the first nil ought to be replaced with 'NIL
;   the second with '()
; so the rule is: NIL in the car -> 'NIL, NIL in the cdr -> '().
;   NIL by itself -> NIL

(define (ac-denil x)
  (cond ((pair? x) (cons (ac-denil-car (car x)) (ac-denil-cdr (cdr x))))
        ((hash-table? x)
         (let ((xc (make-hash-table 'equal)))
           (hash-table-for-each x
             (lambda (k v) (hash-table-put! xc (ac-denil k) (ac-denil v))))
           xc))
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

(define (lex? v env)
  (memq v env))

(define (xcar x)
  (and (pair? x) (car x)))

; #f and '() -> nil for a whole quoted list/tree.

; Arc primitives written in Scheme should look like:

; (xdef foo (lambda (lst)
;           (ac-niltree (scheme-foo (ar-denil-last lst)))))

; That is, Arc lists are NIL-terminated. When calling a Scheme
; function that treats an argument as a list, call ar-denil-last
; to change terminal NIL to '(). When returning any data created by Scheme
; to Arc, call ac-niltree to turn all '() into NIL.
; (hash-table-get doesn't use its argument as a list, so it doesn't
; need ar-denil-last).

(define (ac-niltree x)
  (cond ((pair? x)   (cons (ac-niltree (car x)) (ac-niltree (cdr x))))
        ((or (eq? x #f) (eq? x '()) (void? x))   'nil)
        (#t   x)))

; The next two are optimizations, except work for macros.

(define (decompose fns args)
  (cond ((null? fns) `((fn vals (car vals)) ,@args))
        ((null? (cdr fns)) (cons (car fns) args))
        (#t (list (car fns) (decompose (cdr fns) args)))))

(define (ac-andf s env)
  (ac (let ((gs (map (lambda (x) (gensym)) (cdr s))))
               `((fn ,gs
                   (and ,@(map (lambda (f) `(,f ,@gs))
                               (cdar s))))
                 ,@(cdr s)))
      env))

(define err error)

; run-time primitive procedures

;(define (xdef a b)
;  (namespace-set-variable-value! (ac-global-name a) b)
;  b)

(define-syntax xdef
  (syntax-rules ()
    ((xxdef a b)
     (let ((nm (ac-global-name 'a))
           (a b))
       (namespace-set-variable-value! nm a)
       a))))

(define sig* (make-hash-table 'equal))  ;; fn/macro name -> params
(xdef sig* sig*)

; This is a replacement for xdef that stores operator signatures.
; Haven't started using it yet.

(define (odef a parms b)
  (namespace-set-variable-value! (ac-global-name a) b)
  (hash-table-put! sig* a (list parms))
  b)

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

; call a function or perform an array ref, hash ref, &c

; Non-fn constants in functional position are valuable real estate, so
; should figure out the best way to exploit it.  What could (1 foo) or
; ('a foo) mean?  Maybe it should mean currying.

; For now the way to make the default val of a hash table be other than
; nil is to supply the val when doing the lookup.  Later may also let
; defaults be supplied as an arg to table.  To implement this, need: an
; eq table within scheme mapping tables to defaults, and to adapt the
; code in arc.arc that reads and writes tables to read and write their
; default vals with them.  To make compatible with existing written tables,
; just use an atom or 3-elt list to keep the default.

(define (ar-apply fn args)
  (apply (ar-coerce fn 'fn) args))

(xdef apply (lambda (fn . args)
               (ar-apply fn (ar-apply-args args))))

; replace the nil at the end of a list with a '()

(define (ar-denil-last l)
  (if (or (eqv? l '()) (eqv? l 'nil))
      '()
      (cons (car l) (ar-denil-last (cdr l)))))

; turn the arguments to Arc apply into a list.
; if you call (apply fn 1 2 '(3 4))
; then args is '(1 2 (3 4 . nil) . ())
; that is, the main list is a scheme list.
; and we should return '(1 2 3 4 . ())
; was once (apply apply list (ac-denil args))
; but that didn't work for (apply fn nil)

(define (ar-apply-args args)
  (cond ((null? args) '())
        ((null? (cdr args)) (ar-denil-last (car args)))
        (#t (cons (car args) (ar-apply-args (cdr args))))))





(xdef cons cons)

(xdef car (lambda (x)
             (cond ((pair? x)     (car x))
                   ((eqv? x 'nil) 'nil)
                   ((eqv? x '())  'nil)
                   (#t            (err "Can't take car of" x)))))

(xdef cdr (lambda (x)
             (cond ((pair? x)     (cdr x))
                   ((eqv? x 'nil) 'nil)
                   ((eqv? x '())  'nil)
                   (#t            (err "Can't take cdr of" x)))))

(define (tnil x) (if x 't 'nil))

; (pairwise pred '(a b c d)) =>
;   (and (pred a b) (pred b c) (pred c d))
; pred returns t/nil, as does pairwise
; reduce?

(define (pairwise pred lst)
  (cond ((null? lst) 't)
        ((null? (cdr lst)) 't)
        ((not (eqv? (pred (car lst) (cadr lst)) 'nil))
         (pairwise pred (cdr lst)))
        (#t 'nil)))

; not quite right, because behavior of underlying eqv unspecified
; in many cases according to r5rs
; do we really want is to ret t for distinct strings?

; for (is x y)

(define (ar-is2 a b)
  (tnil (or (eqv? a b)
            (and (string? a) (string? b) (string=? a b))
            (and (ar-false? a) (ar-false? b)))))

; for all other uses of is

(xdef is (lambda args (pairwise ar-is2 args)))

(xdef err err)
(xdef nil 'nil)
(xdef t   't)

(define (all test seq)
  (or (null? seq)
      (and (test (car seq)) (all test (cdr seq)))))

(define (arc-list? x) (or (pair? x) (eqv? x 'nil) (eqv? x '())))

; Generic +: strings, lists, numbers.
; Return val has same type as first argument.

(xdef + (lambda args
           (cond ((null? args) 0)
                 ((char-or-string? (car args))
                  (apply string-append
                         (map (lambda (a) (ar-coerce a 'string))
                              args)))
                 ((arc-list? (car args))
                  (ac-niltree (apply append (map ar-denil-last args))))
                 (#t (apply + args)))))

(define (char-or-string? x) (or (string? x) (char? x)))

(define (ar-+2 x y)
  (cond ((char-or-string? x)
         (string-append (ar-coerce x 'string) (ar-coerce y 'string)))
        ((and (arc-list? x) (arc-list? y))
         (ac-niltree (append (ar-denil-last x) (ar-denil-last y))))
        (#t (+ x y))))

(xdef - -)
(xdef * *)
(xdef / /)
(xdef mod modulo)
(xdef expt expt)
(xdef sqrt sqrt)
(xdef gcd gcd)

; generic comparison

(define (ar->2 x y)
  (tnil (cond ((and (number? x) (number? y)) (> x y))
              ((and (string? x) (string? y)) (string>? x y))
              ((and (symbol? x) (symbol? y)) (string>? (symbol->string x)
                                                       (symbol->string y)))
              ((and (char? x) (char? y)) (char>? x y))
              (#t (> x y)))))

(xdef > (lambda args (pairwise ar->2 args)))

(define (ar-<2 x y)
  (tnil (cond ((and (number? x) (number? y)) (< x y))
              ((and (string? x) (string? y)) (string<? x y))
              ((and (symbol? x) (symbol? y)) (string<? (symbol->string x)
                                                       (symbol->string y)))
              ((and (char? x) (char? y)) (char<? x y))
              (#t (< x y)))))

(xdef < (lambda args (pairwise ar-<2 args)))

(xdef len (lambda (x)
             (cond ((string? x) (string-length x))
                   ((hash-table? x) (hash-table-count x))
                   (#t (length (ar-denil-last x))))))

(define (ar-tagged? x)
  (and (vector? x) (eq? (vector-ref x 0) 'tagged)))

(define (ar-tag type rep)
  (cond ((eqv? (ar-type rep) type) rep)
        (#t (vector 'tagged type rep))))

(xdef annotate ar-tag)

; (type nil) -> sym

(define (exint? x) (and (integer? x) (exact? x)))

(define (ar-type x)
  (cond ((ar-tagged? x)     (vector-ref x 1))
        ((pair? x)          'cons)
        ((symbol? x)        'sym)
        ((null? x)          'sym)
        ((procedure? x)     'fn)
        ((char? x)          'char)
        ((string? x)        'string)
        ((exint? x)         'int)
        ((number? x)        'num)     ; unsure about this
        ((vector? x)        'vector)
        ((hash-table? x)    'table)
        ((output-port? x)   'output)
        ((input-port? x)    'input)
        ((tcp-listener? x)  'socket)
        ((exn? x)           'exception)
        ((thread? x)        'thread)
        ((thread-cell? x)   'thread-cell)
        ((keyword? x)       'keyword)
        (#t                 (err "Type: unknown type" x))))
(xdef type ar-type)

(define (ar-rep x)
  (if (ar-tagged? x)
      (vector-ref x 2)
      x))

(xdef rep ar-rep)

(xdef uniq gensym)

(xdef ccc call-with-current-continuation)

(xdef infile  open-input-file)

(xdef outfile (lambda (f . args)
                 (open-output-file f
                                   'text
                                   (if (equal? args '(append))
                                       'append
                                       'truncate))))

(xdef instring  open-input-string)
(xdef outstring open-output-string)

; use as general fn for looking inside things

(xdef inside get-output-string)

(xdef stdout current-output-port)  ; should be a vars
(xdef stdin  current-input-port)
(xdef stderr current-error-port)

(xdef call-w/stdout
      (lambda (port thunk)
        (parameterize ((current-output-port port)) (thunk))))

(xdef call-w/stdin
      (lambda (port thunk)
        (parameterize ((current-input-port port)) (thunk))))

(xdef readc (lambda str
              (let ((c (read-char (if (pair? str)
                                      (car str)
                                      (current-input-port)))))
                (if (eof-object? c) 'nil c))))

(xdef readchars (lambda (n . str)
                  (let ((cs (read-string n (if (pair? str)
                                              (car str)
                                              (current-input-port)))))
                    (if (eof-object? cs) 'nil (string->list cs)))))

(xdef readb (lambda str
              (let ((c (read-byte (if (pair? str)
                                      (car str)
                                      (current-input-port)))))
                (if (eof-object? c) 'nil c))))

(xdef readbytes (lambda (n . str)
                  (let ((bs (read-bytes n (if (pair? str)
                                              (car str)
                                              (current-input-port)))))
                    (if (eof-object? bs) 'nil (bytes->list bs)))))

(xdef peekc (lambda str
              (let ((c (peek-char (if (pair? str)
                                      (car str)
                                      (current-input-port)))))
                (if (eof-object? c) 'nil c))))

(xdef writec (lambda (c . args)
                (write-char c
                            (if (pair? args)
                                (car args)
                                (current-output-port)))
                c))

(xdef writeb (lambda (b . args)
                (write-byte b
                            (if (pair? args)
                                (car args)
                                (current-output-port)))
                b))

(xdef writebytes (lambda (bs . args)
                   (write-bytes (list->bytes (ac-denil bs))
                                (if (pair? args)
                                    (car args)
                                    (current-output-port)))
                   bs))

(define (printwith f args)
  (let ((port (if (> (length args) 1)
                  (cadr args)
                  (current-output-port))))
    (when (pair? args)
      (f (ac-denil (car args)) port))
    (unless (ar-bflag 'explicit-flush)
      (flush-output port)))
  'nil)

(xdef swrite (lambda args (printwith write args)))
(xdef disp  (lambda args (printwith display args)))

; sread = scheme read. eventually replace by writing read

(xdef sread (lambda (p eof)
               (let ((expr (read p)))
                 (if (eof-object? expr) eof expr))))

; these work in PLT but not scheme48

(define char->ascii char->integer)
(define ascii->char integer->char)

(define (iround x) (inexact->exact (round x)))

; look up first by target type, then by source type
(define coercions (make-hash-table 'equal))

(for-each (lambda (e)
            (let ((target-type (car e))
                  (conversions (make-hash-table 'equal)))
              (hash-table-put! coercions target-type conversions)
              (for-each
               (lambda (x) (hash-table-put! conversions (car x) (cadr x)))
               (cdr e))))
 `((fn      (cons   ,(lambda (l) (lambda (i) (list-ref l i))))
            (string ,(lambda (s) (lambda (i) (string-ref s i))))
            (table  ,(lambda (h) (case-lambda
                                  ((k) (hash-table-get h k 'nil))
                                  ((k d) (hash-table-get h k d)))))
            (vector ,(lambda (v) (lambda (i) (vector-ref v i)))))

   (string  (int    ,number->string)
            (num    ,number->string)
            (char   ,string)
            (cons   ,(lambda (l) (apply string-append
                                        (map (lambda (y) (ar-coerce y 'string))
                                             (ar-denil-last l)))))
            (sym    ,(lambda (x) (if (eqv? x 'nil) "" (symbol->string x)))))

   (sym     (string ,string->symbol)
            (char   ,(lambda (c) (string->symbol (string c)))))

   (int     (char   ,(lambda (c . args) (char->ascii c)))
            (num    ,(lambda (x . args) (iround x)))
            (string ,(lambda (x . args)
                       (let ((n (apply string->number x args)))
                         (if n (iround n)
                             (err "Can't coerce " x 'int))))))

   (num     (string ,(lambda (x . args)
                       (or (apply string->number x args)
                           (err "Can't coerce " x 'num))))
            (int    ,(lambda (x) x)))

   (cons    (string ,(lambda (x) (ac-niltree (string->list x)))))

   (char    (int    ,ascii->char)
            (num    ,(lambda (x) (ascii->char (iround x)))))))

(define (ar-coerce x type . args)
  (let ((x-type (ar-type x)))
    (if (eqv? type x-type) x
        (let* ((fail        (lambda () (err "Can't coerce " x type)))
               (conversions (hash-table-get coercions type fail))
               (converter   (hash-table-get conversions x-type fail)))
          (ar-apply converter (cons x args))))))

(xdef coerce ar-coerce)
(xdef coerce* coercions)

(xdef parameter make-parameter)
(xdef parameterize-sub
      (lambda (var val thunk)
        (parameterize ((var val)) (thunk))))

(xdef open-socket
      (lambda (port)
        (if (pair? port)
            (tcp-listen (cadr port) 50 #t (car port))
            (tcp-listen port 50 #t))))

(define (ar-init-socket init-fn . args)
  (let ((oc (current-custodian))
        (nc (make-custodian)))
    (current-custodian nc)
    (apply
      (lambda (in out . tail)
        (current-custodian oc)
        (associate-custodian nc in out)
        (list* in out tail))
      (call-with-values
        init-fn
        (if (pair? args)
            (car args)
            list)))))

; http://list.cs.brown.edu/pipermail/plt-scheme/2005-August/009414.html
(define upload-limit* (* 30 1000 1000))

(xdef socket-accept (lambda (s)
                      (ar-init-socket
                        (lambda () (tcp-accept s))
                        (lambda (in out)
                          (list (make-limited-input-port in upload-limit* #t)
                                out
                                (let-values (((us them) (tcp-addresses out)))
                                  them))))))

(xdef socket-connect (lambda (host port)
                       (ar-init-socket
                         (lambda () (tcp-connect host port)))))

(xdef ssl-connect (lambda (host port)
                    (ar-init-socket
                      (lambda () (ssl-connect host port)))))

; allow Arc to give up root privileges after it
; calls open-socket. thanks, Eli!
(define setuid (get-ffi-obj 'setuid #f (_fun _int -> _int)
                 ; If we're on Windows, there is no setuid, so we make
                 ; a dummy version. See "Arc 3.1 setuid problem on
                 ; Windows," http://arclanguage.org/item?id=10625.
                 (lambda () (lambda (x) 'nil))))
(xdef setuid setuid)

(xdef new-thread thread)
(xdef current-thread current-thread)

(define (wrapnil f) (lambda args (apply f args) 'nil))

(xdef sleep (wrapnil sleep))

; Will system "execute" a half-finished string if thread killed
; in the middle of generating it?

(xdef system (lambda (s) (tnil (system s))))

(let ((argv (current-command-line-arguments)))
  (namespace-set-variable-value! (ac-global-name 'argv)
                                 (vector->list argv)))

(xdef pipe-from (lambda (cmd)
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

; PLT scheme provides only eq? and equal? hash tables,
; we need the latter for strings.

(xdef table (lambda args
              (let ((h (make-hash-table 'equal)))
                (if (pair? args) ((car args) h))
                h)))

;(xdef table (lambda args
;               (fill-table (make-hash-table 'equal)
;                           (if (pair? args) (ac-denil (car args)) '()))))

(define (fill-table h pairs)
  (if (eq? pairs '())
      h
      (let ((pair (car pairs)))
        (begin (hash-table-put! h (car pair) (cadr pair))
               (fill-table h (cdr pairs))))))

(xdef maptable (lambda (fn table)               ; arg is (fn (key value) ...)
                  (hash-table-for-each table fn)
                  table))

(define (protect during after)
  (dynamic-wind (lambda () #t) during after))

(xdef protect protect)

; need to use a better seed

(xdef rand random)

(xdef dir (lambda (name)
            (ac-niltree (map path->string (directory-list name)))))

; Would def mkdir in terms of make-directory and call that instead
; of system in ensure-dir, but make-directory is too weak: it doesn't
; create intermediate directories like mkdir -p.

(xdef file-exists (lambda (name)
                     (if (file-exists? name) name 'nil)))

(xdef dir-exists (lambda (name)
                     (if (directory-exists? name) name 'nil)))

(xdef rmfile (wrapnil delete-file))

(xdef mvfile (lambda (old new)
                (rename-file-or-directory old new #t)
                'nil))

; top level read-eval-print
; tle kept as a way to get a break loop when a scheme err

; To make namespace and module handling more seamless (see
; lib/ns.arc), we use Racket's 'set! even for undefined variables,
; rather than using 'namespace-set-variable-value! for all Arc
; globals. This makes it possible to parameterize the value of
; 'current-namespace without getting odd behavior, and it makes it
; possible to assign to imported module variables and use
; assignment-aware syntax transformers (particularly those made with
; Racket's 'make-set!-transformer and 'make-rename-transformer).
;
; However, by default 'set! is disallowed when the variable is
; undefined, and we have to use the 'compile-allow-set!-undefined
; parameter to go against that default. Rather than sprinkling
; (parameterize ...) forms all over the code and trying to keep them
; in sync, we put them all in this function, and we use this function
; instead of 'eval when executing the output of 'ac.
;
; In the same spirit, several other uses of 'namespace-variable-value
; and 'namespace-set-variable-value! have been changed to more direct
; versions ((set! ...) forms and direct variable references) or less
; direct versions (uses of full 'arc-eval) depending on how their
; behavior should change when a module import or syntax obstructs the
; original meaning of the variable. Some have instead been kept
; around, but surrounded by (parameterize ...) forms so they're tied
; the main namespace. Another utility changed in this spirit is
; 'bound?, which should now be able to see variables which are bound
; as Racket syntax.
(define (arc-exec racket-expr)
  (eval (parameterize ((compile-allow-set!-undefined #t))
          (compile racket-expr))))

(define (arc-eval expr)
  (arc-exec (ac expr '())))

(define (tle)
  (display "Arc> ")
  (let ((expr (read)))
    (when (not (eqv? expr ':a))
      (write (arc-eval expr))
      (newline)
      (tle))))

(define last-condition* #f)

(define (tl)
  (let ((interactive? (terminal-port? (current-input-port))))
    (when interactive?
      (display
"
To quit:
  arc> (quit)
  (or press ctrl and 'd' at once)
For help on say 'string':
  arc> (help string)
For a list of differences with arc 3.1:
  arc> (incompatibilities)
To run all automatic tests:
  $ hg clone https://bitbucket.org/zck/unit-test.arc
  $ ./arc
  arc> (load \"tests.arc\")

If you have questions or get stuck, come to http://arclanguage.com/forum.
Arc 3.1 documentation: https://arclanguage.github.io/ref.
"))
    (tl2 interactive?)))

(define (tl2 interactive?)
  (when interactive? (display "arc> "))
  (on-err (lambda (c)
            (set! last-condition* c)
            (parameterize ((current-output-port (current-error-port)))
              ((error-display-handler) (exn-message c) c)
              (newline))
            (tl2 interactive?))
    (lambda ()
      (let ((expr (read)))
        (if (eof-object? expr)
             (begin (when interactive? (newline))
                    (exit)))
        (if (eqv? expr ':a)
            'done
            (let ((val (arc-eval expr)))
              (when interactive?
                (write (ac-denil val))
                (newline))
              ; support arc-exec
              (parameterize ((current-namespace main-namespace))
                (namespace-set-variable-value! '_that val)
                (namespace-set-variable-value! '_thatexpr expr))
              (tl2 interactive?)))))))

(define (aload1 p)
  (let ((x (read p)))
    (if (eof-object? x)
        #t
        (begin
          (arc-eval x)
          (aload1 p)))))

(define (atests1 p)
  (let ((x (read p)))
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
  (let ((x (read ip)))
    (if (eof-object? x)
        #t
        (let ((scm (ac x '())))
          (arc-exec scm)
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

(xdef macex (lambda (e) (ac-macex (ac-denil e))))

(xdef macex1 (lambda (e) (ac-macex (ac-denil e) 'once)))

(xdef eval (lambda (e)
              (arc-eval (ac-denil e))))

; If an err occurs in an on-err expr, no val is returned and code
; after it doesn't get executed.  Not quite what I had in mind.

(define (on-err errfn f)
  ((call-with-current-continuation
     (lambda (k)
       (lambda ()
         (with-handlers ((exn:fail? (lambda (c)
                                      (k (lambda () (errfn c))))))
                        (f)))))))
(xdef on-err on-err)

(define (disp-to-string x)
  (let ((o (open-output-string)))
    (display x o)
    (close-output-port o)
    (get-output-string o)))

(xdef details (lambda (c)
                 (disp-to-string (exn-message c))))

(xdef scar (lambda (x val)
              (if (string? x)
                  (string-set! x 0 val)
                  (x-set-car! x val))
              val))

(xdef scdr (lambda (x val)
              (if (string? x)
                  (err "Can't set cdr of a string" x)
                  (x-set-cdr! x val))
              val))

; waterhouse's code to modify mzscheme-4's immutable pairs.
; http://arclanguage.org/item?id=13616
(require racket/unsafe/ops)

(define x-set-car!
  (let ((fn (namespace-variable-value 'set-car! #t (lambda () #f))))
    (if (procedure? fn)
        fn
        (lambda (p x)
          (if (pair? p)
              (unsafe-set-mcar! p x)
              (raise-type-error 'set-car! "pair" p))))))

(define x-set-cdr!
  (let ((fn (namespace-variable-value 'set-cdr! #t (lambda () #f))))
    (if (procedure? fn)
        fn
        (lambda (p x)
          (if (pair? p)
              (unsafe-set-mcdr! p x)
              (raise-type-error 'set-cdr! "pair" p))))))

; When and if cdr of a string returned an actual (eq) tail, could
; say (if (string? x) (string-replace! x val 1) ...) in scdr, but
; for now would be misleading to allow this, because fails for cddr.

(define (string-replace! str val index)
  (if (eqv? (string-length val) (- (string-length str) index))
      (do ((i index (+ i 1)))
          ((= i (string-length str)) str)
        (string-set! str i (string-ref val (- i index))))
      (err "Length mismatch between strings" str val index)))

; Later may want to have multiple indices.

(xdef sref
  (lambda (com val ind)
    (cond ((hash-table? com)  (if (eqv? val 'nil)
                                  (hash-table-remove! com ind)
                                  (hash-table-put! com ind val)))
          ((string? com) (string-set! com ind val))
          ((pair? com)   (nth-set! com ind val))
          ((vector? com)  (vector-set! com ind val))
          (#t (err "Can't set reference " com ind val)))
    val))

(define (nth-set! lst n val)
  (x-set-car! (list-tail lst n) val))

(define (bound? arcname)
  (with-handlers ((exn:fail:syntax? (lambda (e) #t))
                  (exn:fail:contract:variable? (lambda (e) #f)))
    (namespace-variable-value (ac-global-name arcname))
    #t))

(xdef bound (lambda (x) (tnil (bound? x))))

(xdef newstring make-string)

(xdef trunc (lambda (x) (inexact->exact (truncate x))))

(xdef vec make-vector)

; bad name

(xdef exact (lambda (x) (tnil (exint? x))))

(xdef msec                         current-milliseconds)
(xdef current-process-milliseconds current-process-milliseconds)
(xdef current-gc-milliseconds      current-gc-milliseconds)

(xdef seconds current-seconds)

(print-hash-table #t)

(xdef client-ip (lambda (port)
                   (let-values (((x y) (tcp-addresses port)))
                     y)))

; make sure only one thread at a time executes anything
; inside an atomic-invoke. atomic-invoke is allowed to
; nest within a thread; the thread-cell keeps track of
; whether this thread already holds the lock.

(define ar-atomic-sema (make-semaphore 1))
(define ar-atomic-cell (make-thread-cell #f))
(xdef atomic-invoke (lambda (f)
                       (if (thread-cell-ref ar-atomic-cell)
                           (ar-apply f '())
                           (begin
                             (thread-cell-set! ar-atomic-cell #t)
                             (protect
                              (lambda ()
                                (call-with-semaphore
                                 ar-atomic-sema
                                 (lambda () (ar-apply f '()))))
                              (lambda ()
                                (thread-cell-set! ar-atomic-cell #f)))))))

(xdef dead (lambda (x) (tnil (thread-dead? x))))

; Added because Mzscheme buffers output.  Not a permanent part of Arc.
; Only need to use when declare explicit-flush optimization.

(xdef flushout (lambda args (flush-output (if (pair? args)
                                              (car args)
                                              (current-output-port)))
                            't))

(xdef ssyntax (lambda (x) (tnil (ssyntax? x))))

(xdef ssexpand (lambda (x)
                  (if (symbol? x) (expand-ssyntax x) x)))

(xdef quit exit)

; there are two ways to close a TCP output port.
; (close o) waits for output to drain, then closes UNIX descriptor.
; (force-close o) discards buffered output, then closes UNIX desc.
; web servers need the latter to get rid of connections to
; clients that are not reading data.
; mzscheme close-output-port doesn't work (just raises an error)
; if there is buffered output for a non-responsive socket.
; must use custodian-shutdown-all instead.

(define custodians (make-hash-table 'equal))

(define (associate-custodian c i o)
  (hash-table-put! custodians i c)
  (hash-table-put! custodians o c))

; if a port has a custodian, use it to close the port forcefully.
; also get rid of the reference to the custodian.
; sadly doing this to the input port also kills the output port.

(define (try-custodian p)
  (let ((c (hash-table-get custodians p #f)))
    (if c
        (begin
          (custodian-shutdown-all c)
          (hash-table-remove! custodians p)
          #t)
        #f)))

(define (ar-close . args)
  (map (lambda (p)
         (cond ((input-port? p)   (close-input-port p))
               ((output-port? p)  (close-output-port p))
               ((tcp-listener? p) (tcp-close p))
               (#t (err "Can't close " p))))
       args)
  (map (lambda (p) (try-custodian p)) args) ; free any custodian
  'nil)

(xdef close ar-close)

(xdef force-close (lambda args
                       (map (lambda (p)
                              (if (not (try-custodian p))
                                  (ar-close p)))
                            args)
                       'nil))

(xdef memory current-memory-use)

(define ar-declarations (make-hash-table))

(define (ar-bflag key)
  (not (ar-false? (hash-table-get ar-declarations key 'nil))))

(xdef declarations* ar-declarations)

(putenv "TZ" ":GMT")

(define (gmt-date sec) (seconds->date sec))

(xdef timedate
  (lambda args
    (let ((d (gmt-date (if (pair? args) (car args) (current-seconds)))))
      (ac-niltree (list (date-second d)
                        (date-minute d)
                        (date-hour d)
                        (date-day d)
                        (date-month d)
                        (date-year d))))))

(xdef utf-8-bytes
  (lambda (str)
    (bytes->list (string->bytes/utf-8 str))))

(xdef sin sin)
(xdef cos cos)
(xdef tan tan)
(xdef asin asin)
(xdef acos acos)
(xdef atan atan)
(xdef log log)

(xdef lor bitwise-ior)
(xdef land bitwise-and)
(xdef lxor bitwise-xor)
(xdef lnot bitwise-not)
(xdef shl arithmetic-shift)

(define (codestring s)
  (let ((i (atpos s 0)))
    (if i
        (cons (substring s 0 i)
              (let* ((rest (substring s (+ i 1)))
                     (in (open-input-string rest))
                     (expr (read in))
                     (i2 (let-values (((x y z) (port-next-location in))) z)))
                (close-input-port in)
                (cons expr (codestring (substring rest (- i2 1))))))
        (list s))))

; First unescaped @ in s, if any.  Escape by doubling.

(define (atpos s i)
  (cond ((eqv? i (string-length s))
         #f)
        ((eqv? (string-ref s i) #\@)
         (if (and (< (+ i 1) (string-length s))
                  (not (eqv? (string-ref s (+ i 1)) #\@)))
             i
             (atpos s (+ i 2))))
        (#t
         (atpos s (+ i 1)))))

(define (unescape-ats s)
  (list->string (letrec ((unesc (lambda (cs)
                                  (cond
                                    ((null? cs)
                                     '())
                                    ((and (eqv? (car cs) #\@)
                                          (not (null? (cdr cs)))
                                          (eqv? (cadr cs) #\@))
                                     (unesc (cdr cs)))
                                    (#t
                                     (cons (car cs) (unesc (cdr cs))))))))
                  (unesc (string->list s)))))

)
