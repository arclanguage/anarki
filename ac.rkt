#lang racket/base

; Arc Compiler.


(require

  ; This defines names like _list, so it would conflict with the
  ; naming convention for Arc global variables if we didn't prefix it.
  (prefix-in ffi: ffi/unsafe)
  (only-in racket/unsafe/ops unsafe-set-mcar! unsafe-set-mcdr!)

  (only-in racket/contract/base -> any any/c)
  (only-in racket/contract/region define/contract)
  racket/file
  racket/path
  racket/pretty
  racket/port
  racket/format
  (only-in racket/promise delay force)
  (only-in racket/runtime-path define-runtime-path)
  racket/system
  racket/tcp
  openssl
  racket/string
  racket/random

  racket/struct

  (only-in "brackets.rkt" bracket-readtable)

  (for-syntax racket/base))

(provide (all-defined-out))


(define-runtime-path ac-rkt-path "ac.rkt")
(define-runtime-path arc-arc-path "arc.arc")
(define-runtime-path libs-arc-path "libs.arc")

(define-namespace-anchor main-namespace-anchor)
(define main-namespace
  (namespace-anchor->namespace main-namespace-anchor))

(define (ac-global-name s)
  (string->symbol (string-append "_" (symbol->string s))))

(define init-steps-reversed* (list))

(define-syntax-rule (add-init-step step)
  (set! init-steps-reversed*
    (cons (lambda () step) init-steps-reversed*)))

(define-syntax-rule (xdef a b)
  (add-init-step
    (let ([a b])
      (namespace-set-variable-value! (ac-global-name 'a) a))))

(define (run-init-steps)
  (for ([step (reverse init-steps-reversed*)])
    (step)))

(define-syntax defarc
  (syntax-rules ()
    [(defarc (name . args) body ...)
     (defarc name (name . args) body ...)]
    [(defarc arc-name (scheme-name . args) body ...)
     (begin
       (xdef arc-name (lambda args body ...))
       (defarc arc-name scheme-name))]
    [(defarc arc-name scheme-name)
     (define (scheme-name . args)
       (apply (namespace-variable-value (ac-global-name 'arc-name))
              args))]
    [(defarc name)
     (defarc name name)]))

(define (anarki-init)
  (namespace-require 'racket/base)
  (namespace-require ac-rkt-path)
  (run-init-steps)
  (parameterize ([current-directory (path-only arc-arc-path)]
                 [current-readtable bracket-readtable])
    (aload arc-arc-path)
    (aload libs-arc-path)))

(define anarki-init-in-main-namespace-func
  (make-parameter anarki-init))

(define anarki-init-in-main-namespace-promise
  (delay
    (parameterize ([current-namespace main-namespace])
      ((anarki-init-in-main-namespace-func)))))

(define (anarki-init-in-main-namespace)
  (force anarki-init-in-main-namespace-promise))

(define (anarki-init-verbose)
  (parameterize ([current-output-port (current-error-port)])
    (displayln "initializing arc.. (may take a minute)"))
  (anarki-init))

(define (anarki-init-in-main-namespace-verbose)
  (parameterize
    ([anarki-init-in-main-namespace-func anarki-init-verbose])
    (anarki-init-in-main-namespace)))


(struct ar-tagged (type rep) #:prefab)

; compile an Arc expression into a Scheme expression,
; both represented as s-expressions.
; env is a list of lexically bound variables, which we
; need in order to decide whether set should create a global.

(define (stx-map proc stxl)
  (map proc (stx->list stxl)))

(defarc (ac* e s env)
  (cond [(string? s) (ac-string s env)]
        [(keyword? s) s]
        [(literal? s) (list 'quote (ac-quoted s))]
        [(eqv? s 'nil) (list 'quote 'nil)]
        [(ssyntax? s) (ac (expand-ssyntax s) env)]
        [(symbol? s) (ac-var-ref s env)]
        [(eq? (xcar s) 'syntax) (cadr (syntax-e e))]
        [(eq? (xcar (xcar s)) 'syntax) (stx-map ac e)]
        [(ssyntax? (xcar s)) (ac (cons (expand-ssyntax (car s)) (cdr s)) env)]
        [(eq? (xcar s) '$) (ac-$ (cadr s) env)]
        [(eq? (xcar s) 'quote) (list 'quote (ac-quoted (ac-niltree (cadr s))))]
        ((eq? (xcar s) 'lexenv) (ac-lenv (cdr s) env))
        [(and (eq? (xcar s) 'quasiquote)
              (not (ac-macro? 'quasiquote)))
         (ac-qq (cadr s) env)]
        [(eq? (xcar s) 'if) (ac-if (cdr s) env)]
        [(eq? (xcar s) 'fn) (ac-fn (cadr s) (cddr s) env)]
        [(eq? (xcar s) 'assign) (ac-set (cdr s) env)]
        ; the next three clauses could be removed without changing semantics
        ; ... except that they work for macros (so prob should do this for
        ; every elt of s, not just the car)
        [(eq? (xcar (xcar s)) 'compose) (ac (decompose (cdar s) (cdr s)) env)]
        [(eq? (xcar (xcar s)) 'complement)
         (ac (list 'no (cons (cadar s) (cdr s))) env)]
        [(eq? (xcar (xcar s)) 'andf) (ac-andf s env)]
        [(pair? s) (ac-call (car s) (cdr s) env)]
        [(syntax? s) s]
        [#t (err "Bad object in expression" s)]))

(defarc (ac stx (env (env*)) (ns main-namespace))
  (parameterize ((env* env))
    (let* ((s (syn stx))
           (e (syntax->datum s))
           (expr (ac* s e env)))
      (parameterize ((current-namespace ns))
        (namespace-syntax-introduce (syn expr stx))))))

(define (ac-string s env)
  (if (ar-bflag 'atstrings)
      (if (atpos s 0)
        ; evaluate at-expressions without further expanding atstrings and
        ; interpolate them in
        (let ([target-expression
                (cons 'string (map (lambda (x)
                                     (if (string? x)
                                         (unescape-ats x)
                                         x))
                                   (codestring s)))])
          (hash-set! (ar-declarations) 'atstrings 'nil)
          (let ([result (ac target-expression env)])
            (hash-set! (ar-declarations) 'atstrings 't)
            result))
        (list 'string-copy (unescape-ats s)))
      (list 'string-copy s)))     ; avoid immutable strings

(defarc ac-literal (literal? x)
  (or (boolean? x)
      (char? x)
      (string? x)
      (number? x)
      (eq? x '())
      (hash? x)
      (keyword? x)))

(define (ssyntax? x)
  (and (symbol? x)
       (not (or (eqv? x '+) (eqv? x '++) (eqv? x '_)))
       (let ([name (symbol->string x)])
         (has-ssyntax-char? name (- (string-length name) 2)))))

(define (has-ssyntax-char? string i)
  (and (>= i 0)
       (or (let ([c (string-ref string i)])
             (or (eqv? c #\:) (eqv? c #\~)
                 (eqv? c #\&)
                 ;(eqv? c #\_)
                 (eqv? c #\.)  (eqv? c #\!)))
           (has-ssyntax-char? string (- i 1)))))

(define (read-from-string str)
  (let ([port (open-input-string str)])
    (let ([val (read port)])
      (close-input-port port)
      val)))

; Though graphically the right choice, can't use _ for currying
; because then _!foo becomes a function.  Maybe use <>.  For now
; leave this off and see how often it would have been useful.

; Might want to make ~ have less precedence than &, because
; ~foo&bar prob should mean (andf (complement foo) bar), not
; (complement (andf foo bar)).

(define (expand-ssyntax sym)
  ((cond [(eqv? (car (symbol->chars sym)) #\:) expand-keyword]
         [(or (insym? #\: sym) (insym? #\~ sym)) expand-compose]
         [(or (insym? #\. sym) (insym? #\! sym)) expand-sexpr]
         [(insym? #\& sym) expand-and]
     ;   [(insym? #\_ sym) expand-curry]
         [#t (error "Unknown ssyntax" sym)])
   sym))

; turn common-lisp style :keywords into racket #:keywords
(define (expand-keyword sym)
  (string->keyword (list->string (cdr (symbol->chars sym)))))

(define (expand-compose sym)
  (let ([elts (map (lambda (tok)
                     (if (eqv? (car tok) #\~)
                         (if (null? (cdr tok))
                             'no
                             `(complement ,(chars->value (cdr tok))))
                         (chars->value tok)))
                   (tokens (lambda (c) (eqv? c #\:))
                           (symbol->chars sym)
                           '()
                           '()
                           #f))])
    (if (null? (cdr elts))
        (car elts)
        (cons 'compose elts))))

(define (expand-and sym)
  (let ([elts (map chars->value
                   (tokens (lambda (c) (eqv? c #\&))
                           (symbol->chars sym)
                           '()
                           '()
                           #f))])
    (if (null? (cdr elts))
        (car elts)
        (cons 'andf elts))))

; How to include quoted arguments?  Can't treat all as quoted, because
; never want to quote fn given as first.  Do we want to allow quote chars
; within symbols?  Could be ugly.

; If release, fix the fact that this simply uses v0... as vars.  Should
; make these vars gensyms.

(define (expand-curry sym)
  (let ([expr (exc (map (lambda (x)
                          (if (pair? x) (chars->value x) x))
                        (tokens (lambda (c) (eqv? c #\_))
                                (symbol->chars sym)
                                '()
                                '()
                                #t))
                    0)])
    (list 'fn
          (keep (lambda (s)
                  (and (symbol? s)
                       (eqv? (string-ref (symbol->string s) 0)
                             #\v)))
                expr)
          expr)))

(define (keep f xs)
  (cond [(null? xs) '()]
        [(f (car xs)) (cons (car xs) (keep f (cdr xs)))]
        [#t (keep f (cdr xs))]))

(define (exc elts n)
  (cond [(null? elts)
         '()]
        [(eqv? (car elts) #\_)
         (cons (string->symbol (string-append "v" (number->string n)))
               (exc (cdr elts) (+ n 1)))]
        [#t
         (cons (car elts) (exc (cdr elts) n))]))

(define (expand-sexpr sym)
  (build-sexpr (reverse (tokens (lambda (c) (or (eqv? c #\.) (eqv? c #\!)))
                                (symbol->chars sym)
                                '()
                                '()
                                #t))
               sym))

(define (build-sexpr toks orig)
  (cond [(null? toks)
         'get]
        [(null? (cdr toks))
         (chars->value (car toks))]
        [#t
         (list (build-sexpr (cddr toks) orig)
               (if (eqv? (cadr toks) #\!)
                 (list 'quote (chars->value (car toks)))
                 (if (or (eqv? (car toks) #\.) (eqv? (car toks) #\!))
                   (err "Bad ssyntax" orig)
                   (chars->value (car toks)))))]))

(define (insym? char sym) (member char (reverse (cdr (reverse (symbol->chars sym))))))

(define (symbol->chars x) (string->list (symbol->string x)))

(define (chars->value chars) (read-from-string (list->string chars)))

(define (tokens test source token acc keepsep?)
  (cond [(null? source)
         (reverse (if (pair? token)
                      (cons (reverse token) acc)
                      acc))]
        [(test (car source))
         (tokens test
                 (cdr source)
                 '()
                 (let ([rec (if (null? token)
                              acc
                              (cons (reverse token) acc))])
                   (if keepsep?
                       (cons (car source) rec)
                       rec))
                 keepsep?)]
        [#t
         (tokens test
                 (cdr source)
                 (cons (car source) token)
                 acc
                 keepsep?)]))

(defarc (ac-defined-var? s)
  #f)

(define (ac-var-ref s env)
  (cond [(ac-boxed? 'get s) (ac-boxed-get s)]
        [(lex? s env)        s]
        [(ac-defined-var? s) (list (ac-global-name s))]
        [#t                  (ac-global-name s)]))

; lowering into Racket with (unquote <foo>) lifting us back into arc

(define (ac-$ args env)
  (ac-qqx args
    (lambda (x) (ac x env))
    (lambda (x) (error 'ac-$ "Can't use ,@ from within $ in: ~a" args))))

; quote

(define (ac-quoted x)
  (cond ((pair? x)
         (let ((x (imap ac-quoted x)))
           (if (eqv? (xcar x) '%braces)
               ((arc-eval 'listtab:pair) (ac-denil (cdr x)))
               x)))
        (#t x)))

(xdef quoted ac-quoted)

(define (ac-unquoted x)
  (cond ((hash? x)
         (cons '%braces (imap ac-unquoted (tabflat x))))
        ((pair? x)
         (imap ac-unquoted x))
        ((ar-nil? x)
         'nil)
        ((eqv? x 't)
         't)
        (#t x)))

(xdef unquoted ac-unquoted)

(define (tabflat h (l (list)))
    (hash-for-each h
       (lambda (k v) 
         (set! l (cons k l))
         (set! l (cons (ac-unquoted v) l))))
    l)

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
    [(not (pair? x)) x]
    [(eqv? (car x) '%braces) (ac-quoted x)]
    [(eqv? (car x) 'unquote) (unq (cadr x))]
    [(eqv? (car x) 'unquote-splicing) (splice (cadr x))]
    [(eqv? (car x) 'quasiquote)
      (list 'quasiquote
        (ac-qqx (cadr x)
          (lambda (e) (list 'unquote (ac-qqx e unq splice)))
          (lambda (e) (list 'unquote-splicing (ac-qqx e unq splice)))))]
    [#t (imap (lambda (e) (ac-qqx e unq splice)) x)]))

; like map, but don't demand '()-terminated list

(define (imap f l)
  (cond [(pair? l)
         (cons (f (car l)) (imap f (cdr l)))]
        [(null? l)
         '()]
        [#t (f l)]))

; (if) -> nil
; (if x) -> x
; (if t a ...) -> a
; (if nil a b) -> b
; (if nil a b c) -> (if b c)

(define (ac-if args env)
  (cond [(null? args) ''nil]
        [(null? (cdr args)) (ac (car args) env)]
        [#t `(if (not (ar-false? ,(ac (car args) env)))
                 ,(ac (cadr args) env)
                 ,(ac-if (cddr args) env))]))

(define (ac-dbname! name env)
  (if (symbol? name)
      (cons (list name) env)
      env))

(define (ac-dbname env)
  (cond [(null? env) #f]
        [(pair? (car env)) (caar env)]
        [#t (ac-dbname (cdr env))]))

; translate fn directly into a lambda if it has ordinary
; parameters, otherwise use a rest parameter and parse it.

(define (ac-fn args body env)
  (if (ac-complex-args? args)
      (ac-complex-fn args body env)
      (ac-nameit
       (ac-dbname env)
       `(lambda ,(let ([a (ac-denil args)]) (if (eqv? a 'nil) '() a))
          ,@(ac-body* body (append (ac-arglist args) env))))))

; does an fn arg list use optional parameters or destructuring?
; a rest parameter is not complex

(define (ac-complex-args? args)
  (cond [(eqv? args '()) #f]
        [(symbol? args) #f]
        [(and (pair? args) (symbol? (car args)))
         (ac-complex-args? (cdr args))]
        [#t #t]))

; translate a fn with optional or destructuring args
; (fn (x (o y x) (o z 21) (x1 x2) . rest) ...)
; arguments in top-level list are mandatory (unless optional),
; but it's OK for parts of a list you're destructuring to
; be missing.

(define (ac-complex-fn args body env)
  (let* ([ra (gensym)]
         [z (ac-complex-args args env ra #t)])
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
  (cond [(ar-nil? args) '()]
        [(symbol? args) (list (list args ra))]
        [(pair? args)
         (let* ([x (if (and (pair? (car args)) (eqv? (caar args) 'o))
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
                            `(ar-car ,ra))
                        #f))]
                [xa (ac-complex-getargs x)])
           (append x (ac-complex-args (cdr args)
                                      (append xa env)
                                      `(ar-cdr ,ra)
                                      is-params)))]
        [#t (err "Can't understand fn arg list" args)]))

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
  (cond [(null? a) '()]
        [(symbol? a) (list a)]
        [(symbol? (cdr a)) (list (car a) (cdr a))]
        [#t (cons (car a) (ac-arglist (cdr a)))]))

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
      (let ([n (string->symbol (string-append " " (symbol->string name)))])
        (list 'let `([,n ,v]) n))
      v))

; = replaced by set, which is only for vars
; = now defined in arc (is it?)
; name is to cause fns to have their arc names for debugging

(define (ac-set1 a b1 env)
  (if (symbol? a)

      (let ([b (ac b1 (ac-dbname! a env))])
        (list 'let `([zz ,b])
               (cond [(eqv? a 'nil) (err "Can't rebind nil")]
                     [(eqv? a 't) (err "Can't rebind t")]
                     [(ac-boxed? 'set a)  `(begin ,(ac-boxed-set a b) ,(ac-boxed-get a))]
                     [(lex? a env) `(set! ,a zz)]
                     [(ac-defined-var? a) `(,(ac-global-name a) zz)]
                     [#t `(set! ,(ac-global-name a) zz)])
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

(define (ac-lexname env)
  (let ((name (ac-dbname env)))
    (if (eqv? name #f)
        'fn
        (apply string-append
               (map (lambda (x) (string-append (symbol->string x) "-"))
                    (apply append (keep pair? env)))))))

(define (ac-lenv args env)
  (ac-lexenv (ac-lexname env) env))

(define (ac-lexenv name env)
  `(list (list '*name ',name)
         ,@(imap (lambda (var)
                   (let ((val (gensym)))
                     `(list ',var
                            (lambda ,val ,var)
                            (lambda (,val) (set! ,var ,val)))))
                 (filter (lambda (x) (not (or (ar-false? x) (pair? x)))) env))))

(define boxed* '())

(define (ac-boxed? op name)
  (let ((result
    (when (not (ar-false? name))
      (when (not (ar-false? boxed*))
        (let ((slot (assoc name boxed*)))
          (case op
            ((get) (when (and slot (>= (length slot) 2)) (cadr slot)))
            ((set) (when (and slot (>= (length slot) 3)) (caddr slot)))
            (else (err "ac-boxed?: bad op" name op))))))))
    (if (void? result) #f result)))

(define (ac-boxed-set name val)
  (let ((setter (ac-boxed? 'set name)))
     (if (procedure? setter)
       `(,setter ,val)
       (err "invalid setter" name val setter))))

(define (ac-boxed-get name)
  (let ((getter (ac-boxed? 'get name)))
    (if (procedure? getter)
      `(,getter 'nil)
      getter)))


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
  (cond [(and (assoc fn ac-binaries) (= (length args) 2))
         `(,(cadr (assoc fn ac-binaries)) ,@(ac-args '() args env))]
        [#t
         `(,(ac-global-name fn) ,@(ac-args '() args env))]))

; compile a function call
; special cases for speed, to avoid compiled output like
;   (ar-apply _pr (list 1 2))
; which results in 1/2 the CPU time going to GC. Instead:
;   (ar-funcall2 _pr 1 2)
; and for (foo bar), if foo is a reference to a global variable,
;   and it's bound to a function, generate (foo bar) instead of
;   (ar-funcall1 foo bar)

(define (ac-call fn args env)
  (let ([macfn (ac-macro? fn)])
    (cond [macfn
           (ac-mac-call macfn args env)]
          [(and (pair? fn) (eqv? (car fn) 'fn))
           `(,(ac fn env) ,@(ac-args (cadr fn) args env))]
          [(and (ar-bflag 'direct-calls) (symbol? fn) (not (lex? fn env)) (bound? fn)
                (procedure? (arc-eval fn)))
           (ac-global-call fn args env)]
          [#t
           `((ar-coerce ,(ac fn env) 'fn)
             ,@(map (lambda (x) (ac x env)) args))])))

(define (ac-mac-call m args env)
  (let ([x1 (apply m (map ac-niltree args))])
    (let ([x2 (ac (ac-denil x1) env)])
      x2)))

; returns #f or the macro function

(define (ac-macro? fn)
  (if (symbol? fn)
    (let ([v (and (bound? fn) (bound fn))])
      (if (and v
               (ar-tagged? v)
               (eq? (ar-type v) 'mac))
          (ar-rep v)
          #f))
    #f))

; macroexpand the outer call of a form as much as possible

(define (ac-macex e . once)
  (if (pair? e)
      (let ([m (ac-macro? (car e))])
        (if m
            (let ([expansion (ac-denil (apply m (map ac-niltree (cdr e))))])
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
  (cond [(pair? x) (cons (ac-denil-car (car x)) (ac-denil-cdr (cdr x)))]
        [(hash? x)
         (let ([xc (make-hash)])
           (hash-for-each x
             (lambda (k v) (hash-set! xc (ac-denil k) (ac-denil v))))
           xc)]
        [#t x]))

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
; (hash-ref doesn't use its argument as a list, so it doesn't
; need ar-denil-last).

(define (ac-niltree x)
  (cond [(pair? x)   (cons (ac-niltree (car x)) (ac-niltree (cdr x)))]
        [(or (eq? x #f) (eq? x '()) (void? x))   'nil]
        [#t   x]))

; The next two are optimizations, except work for macros.

(define (decompose fns args)
  (cond [(null? fns) `((fn vals (car vals)) ,@args)]
        [(null? (cdr fns)) (cons (car fns) args)]
        [#t (list (car fns) (decompose (cdr fns) args))]))

(define (ac-andf s env)
  (ac (let ([gs (map (lambda (x) (gensym)) (cdr s))])
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

(define sig* (make-hash))  ;; fn/macro name -> params
(xdef sig* sig*)

; This is a replacement for xdef that stores operator signatures.
; Haven't started using it yet.

(define (odef a parms b)
  (namespace-set-variable-value! (ac-global-name a) b)
  (hash-set! sig* a (list parms))
  b)

; convert #f from a Scheme predicate to NIL.

(define (ar-nill x)
  (if (or (eq? x '()) (eq? x #f) (void? x))
      'nil
      x))

; definition of falseness for Arc if.
; must include '() since sometimes Arc functions see
; Scheme lists (e.g. . body of a macro).

(define (ar-false? x)
  (or (ar-nil? x) (eq? x #f)))

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
  (cond [(null? args) '()]
        [(null? (cdr args)) (ar-denil-last (car args))]
        [#t (cons (car args) (ar-apply-args (cdr args)))]))





(xdef cons cons)

(define (ar-car x)
  (cond [(pair? x)     (car x)]
        [(eqv? x 'nil) 'nil]
        [(eqv? x '())  'nil]
        [#t            (err "Can't take car of" x)]))
(xdef car ar-car)

(define (ar-cdr x)
  (cond [(pair? x)     (cdr x)]
        [(eqv? x 'nil) 'nil]
        [(eqv? x '())  'nil]
        [#t            (err "Can't take cdr of" x)]))
(xdef cdr ar-cdr)

(define (ar-nthcdr n xs)
  (cond [(ar-false? xs)  xs]
        [(> n 0)  (ar-nthcdr (- n 1) (cdr xs))]
        [#t  xs]))
(xdef nthcdr ar-nthcdr)

(define (tnil x) (if x 't 'nil))

; (pairwise pred '(a b c d)) =>
;   (and (pred a b) (pred b c) (pred c d))
; pred returns t/nil, as does pairwise
; reduce?

(define (pairwise pred lst)
  (cond [(null? lst) 't]
        [(null? (cdr lst)) 't]
        [(not (eqv? (pred (car lst) (cadr lst)) 'nil))
         (pairwise pred (cdr lst))]
        [#t 'nil]))

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

(xdef raise raise)
(xdef err err)
(xdef nil 'nil)
(xdef t   't)

(define (ar-nil? x)
  (or (eq? x 'nil) (null? x)))

(define (all test seq)
  (or (null? seq)
      (and (test (car seq)) (all test (cdr seq)))))

(define (arc-list? x) (or (pair? x) (ar-nil? x)))

; Generic +: strings, lists, numbers.
; Return val has same type as first argument.

(xdef + (lambda args
           (cond [(null? args) 0]
                 [(char-or-string? (car args))
                  (apply string-append
                         (map (lambda (a) (ar-coerce a 'string))
                              args))]
                 [(andmap arc-list? args)
                  (ac-niltree (apply append (map ar-denil-last args)))]
                 [(evt? (car args))
                  (apply choice-evt args)]
                 [#t (apply + args)])))

(define (char-or-string? x) (or (string? x) (char? x)))

(define (ar-+2 x y)
  (cond [(char-or-string? x)
         (string-append (ar-coerce x 'string) (ar-coerce y 'string))]
        [(and (arc-list? x) (arc-list? y))
         (ac-niltree (append (ar-denil-last x) (ar-denil-last y)))]
        [#t (+ x y)]))

(xdef - -)
(xdef * *)
(xdef / /)
(xdef mod modulo)
(xdef expt expt)
(xdef sqrt sqrt)
(xdef gcd gcd)

; generic comparison

(define (ar->2 x y)
  (tnil (cond [(and (number? x) (number? y)) (> x y)]
              [(and (string? x) (string? y)) (string>? x y)]
              [(and (symbol? x) (symbol? y)) (string>? (symbol->string x)
                                                       (symbol->string y))]
              [(and (char? x) (char? y)) (char>? x y)]
              [#t (> x y)])))

(xdef > (lambda args (pairwise ar->2 args)))

(define (ar-<2 x y)
  (tnil (cond [(and (number? x) (number? y)) (< x y)]
              [(and (string? x) (string? y)) (string<? x y)]
              [(and (symbol? x) (symbol? y)) (string<? (symbol->string x)
                                                       (symbol->string y))]
              [(and (char? x) (char? y)) (char<? x y)]
              [#t (< x y)])))

(xdef < (lambda args (pairwise ar-<2 args)))

(xdef len (lambda (x)
             (cond [(string? x) (string-length x)]
                   [(hash? x) (hash-count x)]
                   [#t (length (ar-denil-last x))])))

(define (ar-tag type rep)
  (cond [(eqv? (ar-type rep) type) rep]
        [#t (ar-tagged type rep)]))

(xdef annotate ar-tag)
(xdef annotated? ar-tagged?)

; (type nil) -> sym

(define (exint? x) (and (integer? x) (exact? x)))

(define (ar-type x)
  (cond [(ar-tagged? x)     (ar-tagged-type x)]
        [(pair? x)          'cons]
        [(symbol? x)        'sym]
        [(null? x)          'sym]
        [(boolean? x)       'sym]
        [(eof-object? x)    'sym]
        [(procedure? x)     'fn]
        [(char? x)          'char]
        [(string? x)        'string]
        [(exint? x)         'int]
        [(number? x)        'num]     ; unsure about this
        [(vector? x)        'vector]
        [(hash? x)          'table]
        [(output-port? x)   'output]
        [(input-port? x)    'input]
        [(tcp-listener? x)  'socket]
        [(exn? x)           'exception]
        [(thread? x)        'thread]
        [(thread-cell? x)   'thread-cell]
        ((channel? x)       'channel)
        ((async-channel? x) 'channel)
        ((evt? x)           'event)
        [(keyword? x)       'keyword]
        [(syntax? x)        'syntax]
        [#t                 (err "Type: unknown type" x)]))
(xdef type ar-type)

(define (ar-rep x)
  (if (ar-tagged? x)
      (ar-tagged-rep x)
      x))

(xdef rep ar-rep)

(xdef uniq gensym)

(xdef ccc call-with-current-continuation)

(xdef call/ec call-with-escape-continuation)

(xdef infile  open-input-file)

(xdef outfile (lambda (f . args)
                 (open-output-file f
                                   ;#:mode 'text
                                   #:exists (if (equal? args '(append))
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
        (parameterize ([current-output-port port]) (thunk))))

(xdef call-w/stdin
      (lambda (port thunk)
        (parameterize ([current-input-port port]) (thunk))))

(xdef readc (lambda str
              (let ([c (read-char (if (pair? str)
                                      (car str)
                                      (current-input-port)))])
                (if (eof-object? c) 'nil c))))

(xdef readchars (lambda (n . str)
                  (let ([cs (read-string n (if (pair? str)
                                              (car str)
                                              (current-input-port)))])
                    (if (eof-object? cs) 'nil cs))))

(xdef readb (lambda str
              (let ([c (read-byte (if (pair? str)
                                      (car str)
                                      (current-input-port)))])
                (if (eof-object? c) 'nil c))))

(xdef readbytes (lambda (n . str)
                  (let ([bs (read-bytes n (if (pair? str)
                                              (car str)
                                              (current-input-port)))])
                    (if (eof-object? bs) 'nil bs))))

(xdef peekc (lambda str
              (let ([c (peek-char (if (pair? str)
                                      (car str)
                                      (current-input-port)))])
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
                   (write-bytes bs
                                (if (pair? args)
                                    (car args)
                                    (current-output-port)))
                   bs))

(define (printwith f args)
  (let ([port (if (> (length args) 1)
                  (cadr args)
                  (current-output-port))])
    (when (pair? args)
      (f (ac-denil (car args)) port))
    (unless (ar-bflag 'explicit-flush)
      (flush-output port)))
  'nil)

(xdef swrite (lambda args (printwith write args)))
(xdef disp  (lambda args (printwith display args)))

; a special end-of-file uninterned symbol guaranteed never to be equal to the
; result of any call to `(read)` or similar.
(xdef eof eof)

; sread = scheme read. eventually replace by writing read


(xdef sread (lambda (p)
               (let ([expr (read p)])
                 expr)))


; these work in PLT but not scheme48

(define char->ascii char->integer)
(define ascii->char integer->char)

(define (iround x) (inexact->exact (round x)))

; look up first by target type, then by source type
(xdef coerce* (make-hash))
(define (coercions)
  (namespace-variable-value (ac-global-name 'coerce*)))

(define (keyword->symbol x) (string->symbol (keyword->string x)))
(define (symbol->keyword x) (string->keyword (symbol->string x)))

(for-each (lambda (e)
            (add-init-step
              (let ([target-type (car e)]
                    [conversions (make-hash)])
                (hash-set! (coercions) target-type conversions)
                (for-each
                  (lambda (x) (hash-set! conversions (car x) (cadr x)))
                  (cdr e)))))
 `((fn      (cons   ,(lambda (l) (lambda (i)
                                   (ar-car (ar-nthcdr (if (< i 0)
                                                        (let* ([l (ar-denil-last l)]
                                                               [len (length l)]
                                                               [i (+ len i)])
                                                          (if (< i 0) len i))
                                                        i)
                                                      l)))))
            (string ,(lambda (s) (lambda (i) (string-ref s i))))
            (table  ,(lambda (h) (case-lambda
                                  [(k) (hash-ref h k 'nil)]
                                  [(k d) (hash-ref h k d)])))
            (vector ,(lambda (v) (lambda (i) (vector-ref v i)))))

   (keyword (sym    ,symbol->keyword)
            (string ,string->keyword))

   (string  (int    ,number->string)
            (num    ,number->string)
            (char   ,string)
            (cons   ,(lambda (l . utf8?)
                       (if (byte? (xcar l))
                           (if (null? utf8?)
                               (bytes->string/latin-1 (list->bytes (ar-denil-last l)))
                               (bytes->string/utf-8 (list->bytes (ar-denil-last l))))
                           (apply string-append
                                          (map (lambda (y) (ar-coerce y 'string))
                                               (ar-denil-last l))))))
            (keyword ,keyword->string)
            (sym    ,(lambda (x) (if (ar-nil? x) "" (symbol->string x)))))

   (sym     (string ,string->symbol)
            (keyword ,keyword->symbol)
            (char   ,(lambda (c) (string->symbol (string c)))))

   (int     (char   ,(lambda (c . args) (char->ascii c)))
            (num    ,(lambda (x . args) (iround x)))
            (string ,(lambda (x . args)
                       (let ([n (apply string->number x args)])
                         (if n (iround n)
                             (err "Can't coerce " x 'int))))))

   (num     (string ,(lambda (x . args)
                       (or (apply string->number x args)
                           (err "Can't coerce " x 'num))))
            (int    ,(lambda (x) x)))

   (cons    (string ,(lambda (x) (ac-niltree (string->list x)))))
   (bytes   (string ,(lambda (x . utf8?)
                       (if (null? utf8?)
                           (bytes->list (string->bytes/latin-1 x))
                           (bytes->list (string->bytes/utf-8 x))))))
   (char    (int    ,ascii->char)
            (num    ,(lambda (x) (ascii->char (iround x)))))))

(define (ar-coerce x type . args)
  (let ([x-type (ar-type x)])
    (if (eqv? type x-type) x
        (let* ([fail        (lambda () (err "Can't coerce " x type))]
               [conversions (hash-ref (coercions) type fail)]
               [converter   (hash-ref conversions x-type fail)])
          (ar-apply converter (cons x args))))))

(xdef coerce ar-coerce)

(xdef parameter make-parameter)
(xdef parameterize-sub
      (lambda (var val thunk)
        (parameterize ([var val]) (thunk))))

(xdef open-socket
      (lambda (port)
        (if (pair? port)
            (tcp-listen (cadr port) 50 #t (car port))
            (tcp-listen port 50 #t))))

(define (ar-init-socket init-fn . args)
  (let ([oc (current-custodian)]
        [nc (make-custodian)])
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
                                (let-values ([(us them) (tcp-addresses out)])
                                  them))))))

(xdef socket-connect (lambda (host port)
                       (ar-init-socket
                         (lambda () (tcp-connect host port)))))

(xdef ssl-connect (lambda (host port)
                    (ar-init-socket
                      (lambda () (ssl-connect host port)))))

; allow Arc to give up root privileges after it
; calls open-socket. thanks, Eli!
(define setuid (ffi:get-ffi-obj 'setuid #f
                 (ffi:_fun ffi:_int ffi:-> ffi:_int)
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

(add-init-step
  (let ([argv (current-command-line-arguments)])
    (namespace-set-variable-value! (ac-global-name 'argv)
                                   (vector->list argv))))

(xdef pipe-from (lambda (cmd)
                   (let ([tf (ar-tmpname)])
                     (system (string-append cmd " > " tf))
                     (let ([str (open-input-file tf)])
                       (system (string-append "rm -f " tf))
                       str))))

(define (ar-tmpname)
  (string-append "/tmp/"
    (list->string
      (map (lambda (byte)
             (integer->char (+ (char->integer #\a) (modulo byte 26))))
        (bytes->list (crypto-random-bytes 17))))))

; PLT scheme provides only eq? and equal? hash tables,
; we need the latter for strings.

(xdef table (lambda args
              (let ([h (make-hash)])
                (when (pair? args)
                  ((car args) h))
                h)))

;(xdef table (lambda args
;               (fill-table (hash 'equal)
;                           (if (pair? args) (ac-denil (car args)) '()))))

(define (fill-table h pairs)
  (if (eq? pairs '())
      h
      (let ([pair (car pairs)])
        (begin (hash-set! h (car pair) (cadr pair))
               (fill-table h (cdr pairs))))))

(xdef maptable (lambda (fn table)               ; arg is (fn (key value) ...)
                  (hash-for-each table fn)
                  table))

(define (protect during after)
  (dynamic-wind (lambda () #t) during after))

(xdef protect protect)

; need to use a better seed

(xdef rand random)

(xdef dir (lambda (name)
            (ac-niltree (map path->string (directory-list name)))))

(xdef ensure-dir (wrapnil make-directory*))

(xdef file-exists (lambda (name)
                     (if (file-exists? name) name 'nil)))

(xdef dir-exists (lambda (name)
                     (if (directory-exists? name) name 'nil)))

(xdef basename (lambda (name)
                 (path->string (file-name-from-path (string->path name)))))
(xdef dirname (lambda (name)
                (or (path->string (path-only (string->path name)))
                    ".")))
; normalize-path will only work for a path that actually exists
(xdef absolute-path (lambda (name)
                      (path->string (normalize-path (string->path name)))))

(xdef rmfile (wrapnil delete-file))

(xdef mvfile (lambda (old new)
                (rename-file-or-directory old new #t)
                'nil))

; top level read-eval-print
; tle kept as a way to get a break loop when a scheme err

; Unlike the official releases of Arc, Anarki uses Racket's 'set! even
; for undefined Arc globals, rather than using
; 'namespace-set-variable-value! for all Arc globals. This makes
; global variables resolve according to the namespace the code was
; compiled in rather than the value of 'current-namespace at run time,
; and it makes it possible to get the special assignment behavior of
; Racket 'make-set!-transformer and 'make-rename-transformer syntaxes.
;
; In order to allow 'set! to compile even for undefined variables, we
; configure 'compile-allow-set!-undefined. Rather than sprinkling
; (parameterize ...) forms all over the code and trying to keep them
; in sync, we put them all in this function, and we use this function
; instead of 'eval when executing the output of 'ac.
;
; In the same spirit, several other uses of 'namespace-variable-value
; and 'namespace-set-variable-value! have been changed to more direct
; versions ((set! ...) forms and direct variable references) or less
; direct versions (uses of full 'arc-eval) depending on how their
; behavior should change when a module import or syntax obstructs the
; original meaning of the variable. Another utility changed in this
; spirit is 'bound?, which should now be able to see variables which
; are bound as Racket syntax.
;
(define (arc-exec racket-expr)
  (eval (parameterize ([compile-allow-set!-undefined #t])
          (if (syntax? racket-expr)
              (compile-syntax (namespace-syntax-introduce racket-expr))
              (compile racket-expr)))))


(define (arc-eval expr . args)
  (if (null? args)
      (arc-exec (ac expr '()))
      (apply arc-eval-boxed expr args)))

(define-syntax w/restore
  (syntax-rules ()
    ((_ var val body ...)
     (let ((w/restore-prev var)
           (w/restore-val  val))
       (dynamic-wind (lambda () (set! var w/restore-val))
                     (lambda () body ...)
                     (lambda () (set! var w/restore-prev)))))))

(define (arc-eval-boxed expr lexenv)
  (w/restore boxed* (if (or (ar-false? boxed*)
                            (ar-false? lexenv))
                      lexenv
                      (append lexenv boxed*))
    (arc-eval expr)))

(define (tle)
  (display "Arc> ")
  (let ([expr (read)])
    (when (not (eqv? expr ':a))
      (pretty-print (arc-eval expr))
      (newline)
      (tle))))

(define (tl)
  (define input (current-input-port))
  ; With default settings, the Racket REPL loads the XREPL module,
  ; which loads the Readline module, which replaces
  ; `current-input-port` with something that technically doesn't
  ; satisfy `terminal-port?`. It gives us some trouble with flushing
  ; the output port too. The way that XREPL detects whether Readline
  ; is already installed is to check that the port's name is
  ; `readline-input`, so that's how we determine that information here
  ; too.
  (define readline? (eq? (object-name input) 'readline-input))
  (define interactive? (or readline? (terminal-port? input)))
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
  $ ./arc.sh
  arc> (load \"tests.arc\")

If you have questions or get stuck, come to http://arclanguage.org/forum.
Arc 3.1 documentation: https://arclanguage.github.io/ref.
"))
  (tl2 interactive?))

(define (tl-with-main-settings)
  (parameterize ([current-namespace main-namespace]
                 [current-readtable bracket-readtable])
    (tl)))


(define (trash-whitespace)
  (when (and (char-ready?) (char-whitespace? (peek-char)))
    (read-char)
    (trash-whitespace)))

(define (tl2 interactive?)
  (when interactive? (display "arc> "))
  (on-err (lambda (c)
            (parameterize ([current-output-port (current-error-port)])
              ((error-display-handler) (exn-message c) c)
              (newline))
            (namespace-set-variable-value!
              (ac-global-name 'last-condition*)
              c)
            (tl2 interactive?))
    (lambda ()
      (let ([expr (read)])
        (trash-whitespace)          ; throw away until we hit non-white or leading newline
        (when (eof-object? expr)
          (when interactive?
            (newline))
          (exit))
        (if (eqv? expr ':a)
            'done
            (let ([val (arc-eval expr)])
              (when interactive?
                (pretty-print (ac-denil val))
                (newline))
              (namespace-set-variable-value!
                (ac-global-name 'that)
                val)
              (namespace-set-variable-value!
                (ac-global-name 'thatexpr)
                expr)
              (tl2 interactive?)))))))

(let ([current-function '()])
  (define (set-current-fn name)
    (set! current-function name))
  (define (current-fn)
    current-function)
  (xdef set-current-fn set-current-fn)
  (xdef current-fn current-fn))

(define (aload1 p)
  (let ([x (sread p)])
    (if (eof-object? x)
        (void)
        (begin
          (arc-eval x)
          (aload1 p)))))

(define (atests1 p)
  (let ([x (sread p)])
    (if (eof-object? x)
        #t
        (begin
          (pretty-print x)
          (newline)
          (let ([v (arc-eval x)])
            (when (ar-false? v)
              (display "  FAILED")
              (newline)))
          (atests1 p)))))

(define (call-with-line-counting-input-file filename body)
  (call-with-input-file filename
    (lambda (p)
      (port-count-lines! p)
      (body p))))

(define (aload filename)
  (call-with-line-counting-input-file filename aload1))

(define (aload-with-main-settings filename)
  (parameterize ([current-namespace main-namespace]
                 [current-readtable bracket-readtable])
    (aload filename)))

(xdef required-files* (make-hash))
(define (arc-required-files)
  (namespace-variable-value (ac-global-name 'required-files*)))

; create a normalized, absolute path from an Arc base path, where
; p is a relative path from that base path. 
(define (arc-normalize-path p [b arc-arc-path])
  (path->string (normalize-path p (path-only b))))

;'canonicalize' an absolute path by making it lowercase
(define (arc-path-key p [b arc-arc-path])
  (string-downcase (arc-normalize-path p b)))

;replicates the former behavior of require.arc by keeping a hash of
;loaded filepaths, preventing multiple reloads. 

(define (list-required-files)
  (hash-values (arc-required-files)))

(define (aload-unique p [b arc-arc-path])
  (let* ([np (~a (arc-normalize-path p b)) ]
         [k  (arc-path-key np b)])
    (and (file-exists? np) (not (hash-has-key? (arc-required-files) k))
      (begin
        (hash-set! (arc-required-files) k np)
        (aload np)
))))

(define (test filename)
  (call-with-line-counting-input-file filename atests1))

(define (acompile1 ip op)
  (let ([x (sread ip)])
    (if (eof-object? x)
        #t
        (let ([scm (ac x)])
          (arc-exec scm)
          (pretty-print scm op)
          (newline op)
          (newline op)
          (acompile1 ip op)))))

; compile xx.arc to xx.arc.scm
; useful to examine the Arc compiler output
(define (acompile inname)
  (let ([outname (string-append inname ".scm")])
    (when (file-exists? outname)
      (delete-file outname))
    (call-with-line-counting-input-file inname
      (lambda (ip)
        (call-with-output-file outname
          (lambda (op)
            (acompile1 ip op)))))))

(xdef macex (lambda (e) (ac-macex (ac-denil e))))

(xdef macex1 (lambda (e) (ac-macex (ac-denil e) 'once)))

(xdef eval (lambda (e)
              (arc-eval (ac-denil e))))

(xdef seval (lambda (e)
               (arc-exec (ac-denil e))))

; If an err occurs in an on-err expr, no val is returned and code
; after it doesn't get executed.  Not quite what I had in mind.

(define (on-err errfn f)
  ((call-with-current-continuation
     (lambda (k)
       (lambda ()
         (with-handlers ([exn:fail? (lambda (c)
                                      (k (lambda () (errfn c))))])
                        (f)))))))
(xdef on-err on-err)

(define (disp-to-string x)
  (let ([o (open-output-string)])
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
;
; We could almost use `unsafe-set-mcar!` and `unsafe-set-mcdr!`
; directly, but these do a sanity check to make sure we don't unsafely
; set the car or cdr of something that isn't an immutable pair.
(require ffi/unsafe/vm)
(define/contract x-set-car!
  (-> pair? any/c any)
  (if (eq? 'chez-scheme (system-type 'vm))
      (let ([f (vm-primitive 'set-car!)])
        (lambda (p x) (f p x)))
      (lambda (p x) (unsafe-set-mcar! p x))))

(define/contract x-set-cdr!
  (-> pair? any/c any)
  (if (eq? 'chez-scheme (system-type 'vm))
      (let ([f (vm-primitive 'set-cdr!)])
        (lambda (p x) (f p x)))
      (lambda (p x) (unsafe-set-mcdr! p x))))

; When and if cdr of a string returned an actual (eq) tail, could
; say (if (string? x) (string-replace! x val 1) ...) in scdr, but
; for now would be misleading to allow this, because fails for cddr.

(define (string-replace! str val index)
  (if (eqv? (string-length val) (- (string-length str) index))
      (do ([i index (+ i 1)])
          ((= i (string-length str)) str)
        (string-set! str i (string-ref val (- i index))))
      (err "Length mismatch between strings" str val index)))

; Later may want to have multiple indices.

(xdef sref
  (lambda (com val ind)
    (cond [(hash? com)  (if (ar-nil? val)
                                  (hash-remove! com ind)
                                  (hash-set! com ind val))]
          [(string? com) (string-set! com ind val)]
          [(pair? com)   (nth-set! com ind val)]
          [(vector? com)  (vector-set! com ind val)]
          [#t (err "Can't set reference " com ind val)])
    val))

(define (nth-set! lst n val)
  (x-set-car! (list-tail lst
                         (if (< n 0)
                           (+ (length (ar-denil-last lst)) n)
                           n))
              val))

(define (bound? arcname)
  (with-handlers ([exn:fail:syntax? (lambda (e) #t)]
                  [exn:fail:contract:variable? (lambda (e) #f)])
    (namespace-variable-value (ac-global-name arcname))
    #t))

(define (bound arcname)
  (with-handlers ([exn:fail:syntax? (lambda (e) #t)]
                  [exn:fail:contract:variable? (lambda (e) #f)])
    (namespace-variable-value (ac-global-name arcname))))

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

(xdef client-ip (lambda (port)
                   (let-values ([(x y) (tcp-addresses port)])
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

(require racket/async-channel)

(xdef chan (lambda args
             (cond
               ((null? args)           (make-channel))
               ((ar-false? (car args)) (make-async-channel #f))
               ((positive? (car args)) (make-async-channel (car args)))
               ((zero? (car args))     (make-channel))
               (#t (err "Channel limit must be > 0 or nil: " (car args))))))

(define (sync? . args)
  (apply sync/timeout 0 args))

(define (chan-fn c method)
  (cond ((channel? c)
         (cond ((eq? method 'get)     channel-get)
               ((eq? method 'try-get) channel-try-get)
               ((eq? method 'put)     channel-put)
               ((eq? method 'put-evt) channel-put-evt)
               (#t (err "chan-fn: invalid method: " method))))
        ((async-channel? c)
         (cond ((eq? method 'get)     async-channel-get)
               ((eq? method 'try-get) async-channel-try-get)
               ((eq? method 'put)     async-channel-put)
               ((eq? method 'put-evt) async-channel-put-evt)
               (#t (err "chan-fn: invalid method: " method))))
        ((and (evt? c) (or (eq? method 'get) (eq? method 'try-get)))
         sync?)
        (#t (err "chan-fn: invalid channel: " c))))

(xdef <- (lambda (c . args)
           (ar-nill
             (if (null? args)
                 ((chan-fn c 'get) c)
                 (begin ((chan-fn c 'put) c args)
                        args)))))

(xdef <-? (lambda (c . args)
            (ar-nill
              (if (null? args)
                  ((chan-fn c 'try-get) c)
                  (let* ((evt ((chan-fn c 'put-evt) c args))
                         (ret (sync/timeout 0 evt)))
                    (if (eq? ret #f)
                        'nil
                        args))))))

; Added because Racket buffers output.  Not a permanent part of Arc.
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
; Racket's close-output-port doesn't work (just raises an error)
; if there is buffered output for a non-responsive socket.
; must use custodian-shutdown-all instead.

(define custodians (make-hash))

(define (associate-custodian c i o)
  (hash-set! custodians i c)
  (hash-set! custodians o c))

; if a port has a custodian, use it to close the port forcefully.
; also get rid of the reference to the custodian.
; sadly doing this to the input port also kills the output port.

(define (try-custodian p)
  (let ([c (hash-ref custodians p #f)])
    (if c
        (begin
          (custodian-shutdown-all c)
          (hash-remove! custodians p)
          #t)
        #f)))

(define (ar-close . args)
  (map (lambda (p)
         (cond [(input-port? p)   (close-input-port p)]
               [(output-port? p)  (close-output-port p)]
               [(tcp-listener? p) (tcp-close p)]
               [#t (err "Can't close " p)]))
       args)
  (map (lambda (p) (try-custodian p)) args) ; free any custodian
  'nil)

(xdef close ar-close)

(xdef force-close (lambda args
                       (map (lambda (p)
                              (when (not (try-custodian p))
                                (ar-close p)))
                            args)
                       'nil))

(xdef memory current-memory-use)

(xdef declarations* (make-hash))
(define (ar-declarations)
  (namespace-variable-value (ac-global-name 'declarations*)))

(define (ar-bflag key)
  (not (ar-false? (hash-ref (ar-declarations) key 'nil))))

(define (utc-date sec) (seconds->date sec #f))

(xdef timedate
  (lambda args
    (let ([d (utc-date (if (pair? args) (car args) (current-seconds)))])
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
  (let ([i (atpos s 0)])
    (if i
        (cons (substring s 0 i)
              (let* ([rest (substring s (+ i 1))]
                     [in (open-input-string rest)]
                     [expr (read in)]
                     [i2 (let-values ([(x y z) (port-next-location in)]) z)])
                (close-input-port in)
                (cons expr (codestring (substring rest (- i2 1))))))
        (list s))))

; First unescaped @ in s, if any.  Escape by doubling.

(define (atpos s i)
  (cond [(eqv? i (string-length s))
         #f]
        [(eqv? (string-ref s i) #\@)
         (if (and (< (+ i 1) (string-length s))
                  (not (eqv? (string-ref s (+ i 1)) #\@)))
             i
             (atpos s (+ i 2)))]
        [#t
         (atpos s (+ i 1))]))

(define (unescape-ats s)
  (list->string (letrec ([unesc (lambda (cs)
                                  (cond
                                    [(null? cs)
                                     '()]
                                    [(and (eqv? (car cs) #\@)
                                          (not (null? (cdr cs)))
                                          (eqv? (cadr cs) #\@))
                                     (unesc (cdr cs))]
                                    [#t
                                     (cons (car cs) (unesc (cdr cs)))]))])
                  (unesc (string->list s)))))

(define (range start end)
  (if (> start end)
    'nil
    (cons start (range (+ start 1) end))))
(xdef range range)

