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

; definition of falseness for Arc if.
; must include '() since sometimes Arc functions see
; Scheme lists (e.g. . body of a macro).

(define (ar-false? x)
  (or (eq? x 'nil) (eq? x '()) (eq? x #f)))

;----packages
(define axioms-package
  ; t and nil are no longer symbols, primarily
  ; because <axiom>t and <axiom>nil don't look
  ; nice.  The (coerce "t" 'sym) becomes <>t
  '(<axiom>o           ; needed for (fn (some (o foo bar)) ...)
    <axiom>fn
    <axiom>compose     ; because of (foo:bar) -> (foo (bar)) conversion
    <axiom>set
    <axiom>quote
    <axiom>quasiquote
    <axiom>unquote
    <axiom>unquote-splicing
    <axiom>if
    <axiom>symeval))

(define (make-package name)
  (let ((symbol-map (make-hash-table 'equal)))
    ; axioms-package is a scheme list
    (for-each
      (lambda (s)
       (hash-table-put! symbol-map (unpackaged-symbol s) s))
      axioms-package)
    (vector 'package
            ; mapping: unpackaged symbol -> symbol
            symbol-map
            ; mapping: packaged interface symbol -> list of interface members
            (make-hash-table 'equal)
            ; package-name
            name)))

(define (package? x)
  (and (vector? x) (eq? 'package (vector-ref x 0))))

(define (package-name pkg)
  (vector-ref pkg 3))

; mapping: package name symbol -> package object
(define packages
  (make-hash-table 'equal))

(define rex-is-package (regexp "^<([^>]*)>(.*)$"))

(define (packaged-string? str)
  (let ((rv (regexp-match rex-is-package str)))
    (if rv
        (if (equal? (cadr rv) "")
            #f
            #t)
        #f)))

(define (packaged-symbol? sym)
  (packaged-string? (symbol->string sym)))

(define (unpackaged-symbol sym)
  (let* ((ss (symbol->string sym))
         (pm (regexp-match rex-is-package ss)))
    (if pm
        (ar-string->symbol (caddr pm))
        sym)))

(define (ar-symbol->string s)
  (cond
    ((eq? s '<>t)   "t")
    ((eq? s '<>nil) "nil")
    (#t             (symbol->string s))))

(define (ar-string->symbol s)
  (cond
    ((equal? s "t")   '<>t)
    ((equal? s "nil") '<>nil)
    (#t               (string->symbol s))))

(define (package-of sym)
  (let ((rv (regexp-match rex-is-package (symbol->string sym))))
    (if (and rv (not (eqv? "" (cadr rv))))
        (cadr rv)
        'nil)))

(define (read-from-string str)
  (let ((port '())
        (rd   '()))
    ; protect with dynamic-wind
    ; in case of reader error
    (dynamic-wind
      (lambda ()
        (set! port (open-input-string str)))
      (lambda ()
        (set! rd (read port)))
      (lambda ()
        (close-input-port port)))
    rd))

(define (canonicalize-symbol sym)
  (let* ((ss (symbol->string sym))
         (pm (regexp-match rex-is-package ss)))
    (if pm
        ;check package
        (let ((pak (cadr pm))
              (unpak (caddr pm)))
          (cond
            ; <>t and <>nil are allowed to have
            ; an explicitly empty package
            ; <>t != t, <>nil != nil
            ((or (equal? unpak "t") (equal? unpak "nil"))
             sym)
            ; unpackaged symbols belong to
            ; the empty package: <>x == x
            ((equal? pak "")
             (string->symbol unpak))
            (#t
             sym)))
        ; no special markings
        sym)))

(define (canonicalize-all ex)
  (cond
    ;((ssyntax? ex)
    ; (canonicalize-all (expand-ssyntax ex)))
    ((symbol? ex)
     (canonicalize-symbol ex))
    ((pair? ex)
     (set-car! ex (canonicalize-all (car ex)))
     (set-cdr! ex (canonicalize-all (cdr ex)))
     ex)
    ((or (eq? ex '()) (eq? ex #f))
     'nil)
    ((eq? ex #t)
     't)
    ((hash-table? ex)
     (hash-table-for-each ex
       (lambda (k v)
         (hash-table-put! ex k
           (canonicalize-all v))))
     ex)
    ; included just for completeness, this
    ; is otherwise quite stupid of us
    ((vector? ex)
     (canonicalize-vector ex 0 (vector-length ex))
     ex)
    (#t
     ex)))

; can't remember do notation off-hand
(define (canonicalize-vector vec i l)
  (if (< i l)
      (begin
        (vector-set! vec i (canonicalize-all (vector-ref vec i)))
        (canonicalize-vector vec (+ i 1) l))))

(define (ar-read . opt)
  (let* ((port
           (if (eq? opt '())
               (current-input-port)
               (car opt)))
         (rd (read port)))
    (canonicalize-all rd)))

(define invalid-package-name (regexp ">"))

(define (the-package str)
  (if (string? str)
    (if (or (packaged-string? str)
            (regexp-match invalid-package-name str))
        #f
        (hash-table-get packages (string->symbol str)
          (lambda ()
            (let ((pak (make-package str)))
              (hash-table-put! packages (string->symbol str)
                pak)
              pak))))
    #f))

(define (package-ref pkg sym)
  (if (ar-symbol? sym)
      (if (packaged-symbol? sym)
          sym
          (let ((tb (vector-ref pkg 1)))
            (hash-table-get tb sym
              (lambda ()
                (let ((generated-sym
                      (string->symbol
                        (string-append
                          "<"
                          (package-name pkg)
                          ">"
                          (ar-symbol->string sym)))))
                  (hash-table-put! tb sym
                    generated-sym)
                  generated-sym)))))
      sym))

(define (package-sref pkg packaged unpackaged)
  (let ((symbol-map (vector-ref pkg 1)))
    (hash-table-put!
      symbol-map unpackaged packaged)
    packaged))

(define (interface-of-package pkg sym)
  (hash-table-get (interface-table-of-package pkg) sym
    (lambda ()
      #f)))

(define (interface-table-of-package pkg)
  (vector-ref pkg 2))

(define (interface-lookup sym)
  (let* ((pm  (regexp-match rex-is-package (ar-symbol->string sym)))
         (pak (if pm (cadr pm)
                     (error "interface lookup expected packaged symbol")))
         (pkg (the-package pak)))
    (interface-of-package pkg sym)))

(define (make-context)
  (vector 'context (the-package "User")))

(define (context? x)
  (and (vector? x) (eq? 'context (vector-ref x 0))))

(define (context-ref-reuse! cxt ex)
  (cond
    ((pair? ex)
     (let ((head (car ex)))
       (if (context-metacommand? head)
           (context-ref cxt ex)
           (begin
             (set-car! ex (context-ref-reuse-inner! cxt (car ex)))
             (set-cdr! ex (context-ref-reuse-inner! cxt (cdr ex)))
             ex))))
    (#t
      (context-ref-reuse-inner! cxt ex))))
(define (context-ref-reuse-inner! cxt ex)
  (cond
    ((ar-symbol? ex)
      (if (ssyntax? ex)
          (context-ref-reuse-inner! cxt (expand-ssyntax ex))
          (package-ref (package-of-context cxt) ex)))
    ((pair? ex)
      (set-car! ex (context-ref-reuse-inner! cxt (car ex)))
      (set-cdr! ex (context-ref-reuse-inner! cxt (cdr ex)))
      ex)
    (#t
      ex)))

(define (context-metacommand? head)
  (or (eq? head 'in-package)
      (eq? head 'using)
      (eq? head 'import)
      (eq? head 'interface)
      ; TODO
      (eq? head 'interface-ssyntax)
      (eq? head 'import-ssyntax)))

; TODO: make ssyntax functions based on the package
; in the context.

(define (context-metacommand-compile cxt expr)
  (let ((head (car expr)))
    (cond
      ((eq? head 'in-package)
       '__t)
      ((eq? head 'using)
       `(let ((cxt (make-context)))
          (context-ref cxt '(in-package ,(string->symbol
                                           (package-name
                                             (package-of-context cxt)))))
          (context-ref cxt '(using ,(cadr expr)))
          __t))
      ((eq? head 'interface)
       (let* ((pkg (package-of-context cxt))
              (parms
                (arc-map
                  (lambda (a)
                    (if (pair? a)
                        (arc-map (lambda (a)
                                   (package-ref pkg a)))
                        (package-ref pkg a)))
                  (cdr expr))))
         `(let ((cxt (make-context)))
            (context-ref cxt '(interface ,@parms)))))
      ((eq? head 'import)
       `(package-sref (the-package ,(package-name (package-of-context cxt)))
                      ,@(cdr expr))))))
      ; TODO: interface-ssyntax, import-ssyntax

(define (context-ref cxt ex)
  (cond
    ((pair? ex)
     (let ((head (car ex)))
       (cond
         ; (in-package package)
         ((eq? head 'in-package)
          ; syntax check
          (if (not (pair? (cdr ex)))
              (error "'in-package expects one parameter"))
          (if (not (ar-false? (cddr ex)))
              (error "'in-package expects at most one parameter"))
          (if (or (not (ar-symbol? (cadr ex)))
                  (packaged-symbol? (cadr ex)))
              (error "'in-package expects an unpackaged symbol"))
          (let ((pkg-str (ar-symbol->string (cadr ex))))
               (package-of-context-set! cxt (the-package pkg-str))
               't))
         ; (using <package>interface)
         ((eq? head 'using)
          ; syntax check
          (if (not (pair? (cdr ex)))
              (error "'using expects one parameter"))
          (if (not (ar-false? (cddr ex)))
              (error "'using expects at most one parameter"))
          (if (or (not (ar-symbol? (cadr ex)))
                  (not (packaged-symbol? (cadr ex))))
              (error "'using expects a packaged symbol"))
          (let* ((sym (cadr ex))
                 (ss (symbol->string sym))
                 (pm (regexp-match rex-is-package ss))
                 (pak (cadr pm))
                 (pak-file (path->string (apply build-path (split-by-/ pak))))
                 (pkg (the-package pak))
                 (int-list (interface-of-package pkg sym)))
            ; if package interface doesn't exist, try
            ; 'require-ing it.
            (if (not int-list)
                (if (namespace-variable-value '__<arc>require
                                              #t
                                              (lambda () #f))
                  (let ((f-path
                         (or (load-resolve (string-append pak-file ".arc"))
                             (load-resolve (string-append pak-file ".larc")))))
                    (if f-path
                        (ar-funcall1 (eval '__<arc>require) f-path)
                        ;; try to load it as a library
                        (ar-funcall1 (eval '__<arc>require) 
                                     (ar-string->symbol pak)))
                    (set! int-list (interface-of-package pkg sym)))))
            ; check if package interface *still* doesn't exist
            (if (not int-list)
                (error "Package interface does not exist: " ss))
            (let ((dest-pkg (package-of-context cxt)))
              ; int-list is from arc
              (arc-for-each
                (lambda (s)
                  (package-sref dest-pkg s (unpackaged-symbol s)))
                int-list))
            't))
         ((eq? head 'import)
          ; syntax check
          (if (or (not (pair?     (cdr ex)))
                  (not (pair?     (cddr ex)))
                  (not (ar-false? (cdddr ex))))
              (error "'import expects two parameters"))
          (if (not (packaged-symbol? (cadr ex)))
              (error "first parameter to 'import should be packaged symbol"))
          (if (packaged-symbol? (caddr ex))
              (error "second parameter to 'import should be unpackaged symbol"))
          (package-sref (package-of-context cxt) (cadr ex) (caddr ex))
          't)
         ;   (interface symbol
         ;     symbol (symbol-to-remove) included-interface)
         ((eq? head 'interface)
          (let* ((dest-pkg (package-of-context cxt))
                 (interface '())
                 (int-tl '())
                 (add-int
                   (lambda (np)
                     (if (eq? interface '())
                         (begin
                           (set! interface (list np))
                           (set! int-tl    interface))
                         (begin
                           (set-cdr! int-tl (list np))
                           (set!     int-tl (cdr int-tl))))))
                 (find
                   ; I'd implement this as 'afn instead of using 'ccc,
                   ; but then scheme doesn't have 'afn T.T
                   (lambda (i l)
                     (call/cc
                       (lambda (return)
                         ; np is from arc
                         (arc-for-each
                           (lambda (ii)
                             (if (eq? i ii) (return #t)))
                           l)
                         (return #f)))))
                 (remove
                   (lambda (np)
                     (let ((tmp interface))
                       (set! interface '())
                       (set! int-tl '())
                       ; tmp is the interface, which is
                       ; a scheme list
                       (for-each
                         (lambda (e)
                           ; lookup in current package
                           (let ((sym (package-ref dest-pkg e)))
                             (if (not (find sym np))
                                 (add-int sym))))
                         tmp))))
                 (int-name
                   (if (and (pair? (cdr ex)) (ar-symbol? (cadr ex)))
                       (package-ref dest-pkg (cadr ex))
                       (error "'interface expects a symbol for interface name")))
                 (params
                   (arc-map
                     (lambda (p)
                       (if (and (not (ar-symbol? p)) (pair? p))
                           (error "'interface expects a list of symbols or removed symbols"))
                       (cond
                        ((pair? p)
                         (arc-map
                           (lambda (p)
                             (if (not (ar-symbol? p))
                                 (error "'interface expects symbols in removed list"))
                             (package-ref dest-pkg p))
                           p))
                        (#t
                          (package-ref dest-pkg p))))
                     (cddr ex)))
                 (int-table (interface-table-of-package
                              (the-package (package-of int-name)))))
            ; params are from arc
            (arc-for-each
              (lambda (p)
                (if (pair? p)
                    (remove p)
                    ; look them up in the current package first
                    (let* ((sym (package-ref dest-pkg p))
                           ; check if there is an interface
                           ; with that name, and include if so
                           (int (interface-lookup sym)))
                      (if int
                          ; interfaces are scheme lists
                          (for-each
                            (lambda (e)
                              (add-int e))
                            int)
                          (add-int sym)))))
              params)
            (hash-table-put! int-table int-name
              interface)
            't))
         ;reserved
         ((or (eq? head 'interface-ssyntax) (eq? head 'import-ssyntax))
          (err "The 'interface-ssyntax and 'import-ssyntax contexter metacommands are reserved for future Arc3F revisions"))
         (#t
           (arc-map (lambda (x) (context-ref-inner cxt x))
                     ex)))))
    (#t (context-ref-inner cxt ex))))

(define (split-by-/ str)
  (let ((start 0)
        (ln    (string-length str))
        (accum '())
        (self 'nil))
    (set! self
          (lambda (i)
            (if (= i ln)
                (set! accum (cons (substring str start i) accum))
                (if (eqv? #\/ (string-ref str i))
                    (begin
                      (set! accum (cons (substring str start i) accum))
                      (if (< (+ i 1) ln)
                          (begin
                            (set! start (+ i 1))
                            (self (+ i 1)))))
                    (self (+ i 1))))))
    (self 0)
    (reverse accum)))

(define (context-ref-inner cxt x)
  (cond
    ((ar-symbol? x)
     (if (ssyntax? x)
         (context-ref-reuse-inner! cxt (expand-ssyntax x))
         (package-ref (package-of-context cxt) x)))
    ((pair? x)
     (cons (context-ref-inner cxt (car x))
           (context-ref-inner cxt (cdr x))))
    (#t
     x)))

(define (arc-map fn l)
  (if (ar-false? l)
      'nil
      (let ((rv (list (fn (car l)))))
        (arc-map-inner fn (cdr l) rv rv))))
(define (arc-map-inner fn l hd tl)
  (if (ar-false? l)
      hd
      (begin
        (set-cdr! tl (list (fn (car l))))
        (arc-map-inner fn (cdr l) hd (cdr tl)))))

(define (arc-for-each fn l)
  (if (ar-false? l) 'nil
                     (begin
                       (fn (car l))
                       (arc-for-each fn (cdr l)))))

(define (package-of-context cxt)
  (vector-ref cxt 1))
(define (package-of-context-set! cxt pkg)
  (vector-set! cxt 1 pkg))

(define (ar-symbol? p)
  (and (symbol? p) (not (eq? p 'nil)) (not (eq? p 't))))

;----packages end
;above used to be separate file, but then we started
;needing quite a bit from ac.scm T.T


; compile an Arc expression into a Scheme expression,
; both represented as s-expressions.
; env is a list of lexically bound variables, which we
; need in order to decide whether set should create a global.

(define (ac s env)
  (set! s (ac-denil s))
  (let ((head (xcar s)))
    (cond ((string? s) (string-copy s))  ; to avoid immutable strings
          ((literal? s) s)
          ((eqv? s 'nil) (list 'quote 'nil))
          ((symbol? s) (ac-var-ref s env))
          ((eq? head '<axiom>quote) (list 'quote (ac-niltree (cadr s))))
          ((eq? head '<axiom>quasiquote) (ac-qq (cadr s) env))
          ((eq? head '<axiom>if) (ac-if (cdr s) env))
          ((eq? head '<axiom>fn) (ac-fn (cadr s) (cddr s) env))
          ((eq? head '<axiom>set) (ac-set (cdr s) env))
          ((eq? head '<axiom>symeval) (ac-symeval (cdr s) env))
          ; this line could be removed without changing semantics
          ((eq? (xcar head) '<axiom>compose) (ac (decompose (cdar s) (cdr s)) env))
          ((pair? s) (ac-call (car s) (cdr s) env))
          ((eof-object? s) (exit))
          (#t (err "Bad object in expression" s)))))

(define *defs* (make-hash-table))

(define (literal? x)
  (or (boolean? x)
      (char? x)
      (string? x)
      (number? x)
      (ar-procedure? x) ; to allow (eval `(,+ 3 4))
      (eq? x '())))

(define (ssyntax? x)
  (default-ssyntax? x))
(define (expand-ssyntax sym)
  (ac-denil (default-expand-ssyntax sym)))

(define (default-ssyntax? x)
  (and (ar-symbol? x)
       (not (or (eqv? x '+) (eqv? x '++)))
       (let ((name (symbol->string x)))
         (has-ssyntax-char? name (- (string-length name) 1)))))

(define (has-ssyntax-char? string i)
  (and (>= i 0)
       (or (let ((c (string-ref string i)))
             (or (eqv? c #\:) (eqv? c #\~) (eqv? c #\.) (eqv? c #\!)))
           (has-ssyntax-char? string (- i 1)))))

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
  (if (lex? s env)
      s
      (ac-global-name s)))

; quasiquote

(define (ac-qq args env)
  (list 'quasiquote (ac-qq1 1 args env)))

; process the argument of a quasiquote. keep track of
; depth of nesting. handle unquote only at top level (level = 1).
; complete form, e.g. x or (fn x) or (unquote (fn x))
(define (ac-qq1 level x env)
  (cond ((= level 0)
         (ac x env))
        ((eqv? (xcar x) '<axiom>unquote)
         (list 'unquote (ac-qq1 (- level 1) (cadr x) env)))
        ((and (eqv? (xcar x) '<axiom>unquote-splicing) (= level 1))
         (list 'unquote-splicing
               (list 'ar-nil-terminate (ac-qq1 (- level 1) (cadr x) env))))
        ((eqv? (xcar x) '<axiom>quasiquote)
         (list 'quasiquote (ac-qq1 (+ level 1) (cadr x) env)))
        ((pair? x)
         (let ((t (lambda (f) (ac-qq1 level (f x) env)))) 
           (cons (t car) (t cdr))))
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
  (if (ac-complex-args? args)
      (ac-complex-fn args body env)
      `(lambda ,(let ((a (ac-denil args))) (if (eqv? a 'nil) '() a))
         ,@(ac-body* body (append (ac-arglist args) env)))))

; does an fn arg list use optional parameters or destructuring?
; a rest parameter is not complex
(define (ac-complex-args? args)
  (cond ((eqv? args '()) #f)
        ((symbol? args) #f)
        ((symbol? (xcar args))
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
         (let* ((x (if (and (pair? (car args)) (eqv? (caar args) '<axiom>o))
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
                     ((lex? a env) `(set! ,a ,name))
                     (#t
                        `(begin
                           (namespace-set-variable-value! ',(ac-global-name a) ,name)
                           (hash-table-put! *defs* ',(ac-global-name a) ',b))))
               name))
      (err "First arg to set must be a symbol" a)))
      
(define (ac-symeval xs env)
  (cond
    ((ar-false? xs)
      (err "'symeval accepts exactly one parameter, given " xs))
    ((ar-false? (cdr xs))
      (let ((x (car xs)))
            ; compile away symeval if (symeval 'global) form
        (if (and (pair? x)             (eq? (car x) '<axiom>quote)
                 (pair? (cdr x))       (ar-false? (cddr x))
                 (ar-symbol? (cadr x)) )
            (ac-global-name (cadr x))
            `(symeval ,(ac x env)))))
    (#t
      (err "'symeval accepts exactly one parameter, none given"))))

; compile a function call
; special cases for speed, to avoid compiled output like
;   (ar-apply __pr (list 1 2))
; which results in 1/2 the CPU time going to GC. Instead:
;   (ar-funcall2 __pr 1 2)
(define (ac-call fn args env)
  (let ((macfn (ac-macro? fn)))
    (cond (macfn
           (ac-mac-call macfn args env))
          ((and (pair? fn) (eqv? (car fn) '<axiom>fn))
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

(define (ac-mac-call m args env)
  (let ((x1 (ar-apply m (map ac-niltree args))))
    (let ((x2 (ac (ac-denil x1) env)))
      x2)))

; returns #f or the macro function

(define (ac-macro? fn)
  (if (symbol? fn)
      (let ((v (namespace-variable-value (ac-global-name fn) 
                                         #t 
                                         (lambda () #f))))
        (if (and v
                 (ar-tagged? v)
                 (eq? (ar-type v) '<arc>mac))
            (ar-rep v)
            #f))
      #f))

; macroexpand the outer call of a form as much as possible

(define (ac-macex e . once)
  (let ((m (ac-macro? (xcar e))))
    (if m
      (let ((expansion (ac-denil (ar-apply m (map ac-niltree (cdr e))))))
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
(define (lex? v env)
  (memq v env))

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

(define arc-package-cxt
        (let ((rv (make-context)))
          (context-ref rv '(in-package arc))
          rv))

(define (xdef a b)
  (namespace-set-variable-value!
    (ac-global-name
      (context-ref-reuse! arc-package-cxt a))
    b)
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

#|
   (if (eq? x 'nil) #t
      (if (eq? x '()) #t
          (not x)))
|#

; call a function or perform an array ref, hash ref, &c

; Non-fn donstants in functional position are valuable real estate, so
; should figure out the best way to exploit it.

; There are 6 types of Arc functions:
;   1. compiled directly to scheme (generic/non-method functions)
;   2. monomethods (dispatch on first parameter only)
;      - implement as flat tables on the type
;   3. multimethods (more complex dispatch)
;      - implement as nested tables on the type
;   4. reductors (left, right, or truth reduction)
;      - implement as vectors containing the functions
;        to execute
;   5. composers (two functions tied input-to-output)
;      - implement as a vector containing the functions
;        to compose
;   6. clojure-style multimethods (very generalized
;         dispatch)
;      - implement as pair of dispatcher function
;        and dispatch table

(define (ar-procedure? fn)
  (or (procedure? fn) (monomethod? fn) (multimethod? fn)
      (reductor? fn)  (composer? fn)   (clojure-method? fn)))

(define (ar-get-call)
  (let ((rv (eval '__<arc>call*)))
    (if (ar-procedure? rv)
        rv
        (err "call* must be a true function"))))

(define (ar-apply fn args)
  (cond
    ((procedure? fn)
       (apply fn args))
    ((monomethod? fn)
       (ar-apply (monomethod-lookup fn (car args)) args))
    ((multimethod? fn)
       (ar-apply (multimethod-lookup fn args) args))
    ((reductor? fn)
       (reductor-apply fn args))
    ((composer? fn)
       (ar-funcall1 (composer-fl fn) (ar-apply (composer-fr fn) args)))
    ((clojure-method? fn)
       (clojure-method-apply fn args))
    (#t
       (ar-apply (ar-get-call) (cons fn args)))))

(xdef 'apply (lambda (fn . args)
               (ar-apply fn (ar-apply-args args))))

(define (ar-f2-to-lambda f2)
  (if (procedure? f2)
      f2
      (lambda (a b) (ar-funcall2 f2 a b))))

; special cases of ar-apply for speed and to avoid consing arg lists
(define (ar-funcall0 fn)
  (cond
    ((procedure? fn)
      (fn))
    ; monomethods and multimethods dispatch on at least one argument...
    ((or (monomethod? fn) (multimethod? fn))
      ; get the generic
      (ar-funcall0 (vector-ref fn 2)))
    ((reductor? fn)
      (ar-funcall0 (reductor-f0 fn)))
    ((composer? fn)
      (ar-funcall1 (composer-fl fn) (ar-funcall0 (composer-fr fn))))
    ((clojure-method? fn)
      (ar-funcall0
        (clojure-method-lookup fn
                               (ar-funcall0
                                 (clojure-method-dispatch-fn fn)))))
    (#t
      ; note that we have inserted an argument...
      (ar-funcall1 (ar-get-call) fn))))

; hot spot
(define (ar-funcall1 fn arg1)
  (cond
    ((procedure? fn)
      (fn arg1))
    ((monomethod? fn)
      (ar-funcall1 (monomethod-lookup fn arg1) arg1))
    ((multimethod? fn)
      (ar-funcall1 (multimethod-lookup fn (list arg1)) arg1))
    ((reductor? fn)
      (ar-funcall1 (reductor-f1 fn) arg1))
    ((composer? fn)
      (ar-funcall1 (composer-fl fn) (ar-funcall1 (composer-fr fn) arg1)))
    ((clojure-method? fn)
      (ar-funcall1
        (clojure-method-lookup fn
                               (ar-funcall1
                                 (clojure-method-dispatch-fn fn)
                                 arg1))
        arg1))
    (#t
      (ar-funcall2 (ar-get-call) fn arg1))))

; hot spot (hotter than ar-funcall1)
(define (ar-funcall2 fn arg1 arg2)
  (cond
    ((procedure? fn)
      (fn arg1 arg2))
    ((monomethod? fn)
      (ar-funcall2 (monomethod-lookup fn arg1) arg1 arg2))
    ((multimethod? fn)
      (ar-funcall2 (multimethod-lookup fn (list arg1 arg2)) arg1 arg2))
    ((reductor? fn)
      (ar-funcall2 (reductor-f2 fn) arg1 arg2))
    ((composer? fn)
      (ar-funcall1 (composer-fl fn) (ar-funcall2 (composer-fr fn) arg1 arg2)))
    ((clojure-method? fn)
      (ar-funcall2
        (clojure-method-lookup fn
                               (ar-funcall2
                                 (clojure-method-dispatch-fn fn)
                                 arg1
                                 arg2))
        arg1
        arg2))
    (#t
      (ar-funcall3 (ar-get-call) fn arg1 arg2))))

(define (ar-funcall3 fn arg1 arg2 arg3)
  (cond
    ((procedure? fn)
      (fn arg1 arg2 arg3))
    ((monomethod? fn)
      (ar-funcall3 (monomethod-lookup fn arg1) arg1 arg2 arg3))
    ((multimethod? fn)
      (ar-funcall3 (multimethod-lookup fn (list arg1 arg2 arg3)) arg1 arg2 arg3))
    ((l-reductor? fn)
      (let ((f2 (ar-f2-to-lambda (reductor-f2 fn))))
        (f2 (f2 arg1 arg2) arg3)))
    ((r-reductor? fn)
      (let ((f2 (ar-f2-to-lambda (reductor-f2 fn))))
        (f2 arg1 (f2 arg2 arg3))))
    ((t-reductor? fn)
      (let ((f2 (ar-f2-to-lambda (reductor-f2 fn))))
        (if (ar-false? (f2 arg1 arg2))
            'nil
            (f2 arg2 arg3))))
    ((composer? fn)
      (ar-funcall1 (composer-fl fn)
                   (ar-funcall3 (composer-fr fn) arg1 arg2 arg3)))
    ((clojure-method? fn)
      (ar-funcall3
        (clojure-method-lookup fn
                               (ar-funcall3
                                 (clojure-method-dispatch-fn fn)
                                 arg1
                                 arg2
                                 arg3))
        arg1
        arg2
        arg3))
    (#t
      (ar-funcall4 (ar-get-call) fn arg1 arg2))))

(define (ar-funcall4 fn arg1 arg2 arg3 arg4)
  (cond
    ((procedure? fn)
      (fn arg1 arg2 arg3 arg4))
    ((monomethod? fn)
      (ar-funcall4 (monomethod-lookup fn arg1) arg1 arg2 arg3 arg4))
    ((multimethod? fn)
      (ar-funcall4 (multimethod-lookup fn (list arg1 arg2 arg3 arg4)) arg1 arg2 arg3 arg4))
    ; mzscheme 372 mysteriously segfaults on compiling
    ; this module if we don't compute return values
    ; individually
    ((l-reductor? fn)
      (let* ((f2 (ar-f2-to-lambda (reductor-f2 fn)))
             (r1 (f2 arg1 arg2))
             (r2 (f2 r1   arg3)))
        (f2 r2 arg4)))
    ((r-reductor? fn)
      (let* ((f2 (ar-f2-to-lambda (reductor-f2 fn)))
             (r1 (f2 arg3 arg4))
             (r2 (f2 arg2 r1  )))
        (f2 arg1 r2)))
    ((t-reductor? fn)
      (let ((f2 (ar-f2-to-lambda (reductor-f2 fn))))
        (if (ar-false? (f2 arg1 arg2))
            'nil
            (if (ar-false? (f2 arg2 arg3))
                'nil
                (f2 arg3 arg4)))))
    ((composer? fn)
      (ar-funcall1 (composer-fl fn)
                   (ar-funcall4 (composer-fr fn) arg1 arg2 arg3 arg4)))
    ((clojure-method? fn)
      (ar-funcall4
        (clojure-method-lookup fn
                               (ar-funcall4
                                 (clojure-method-dispatch-fn fn)
                                 arg1
                                 arg2
                                 arg3
                                 arg4))
        arg1
        arg2
        arg3
        arg4))
    (#t
      (ar-apply (ar-get-call) (list fn arg1 arg2 arg3 arg4)))))

; monomethod definition
(define (monomethod? f)
  (and (vector? f) (eq? (vector-ref f 0) 'monomethod)))

(define (ar-monomethod hash generic)
  (vector 'monomethod hash generic))

; hot spot
(define (monomethod-lookup f arg)
  (hash-table-get (vector-ref f 1) (ar-type arg) (lambda () (vector-ref f 2))))

; multimethod definition
; TODO: true CLOS-style generic multimethods

(define (multimethod? f)
  (and (vector? f) (eq? (vector-ref f 0) 'multimethod)))

(define (multimethod tb gen)
  (vector 'multimethod tb gen))

(define (multimethod-subtable? tb)
  (and (vector? tb) (eq? (vector-ref tb 0) 'multimethod-subtable)))

(define (multimethod-subtable tb)
  (vector 'multimethod-subtable tb))

(define (multimethod-from-generic lt spec gen)
  (err "multimethods not yet implemented!")
  (let ((tb (multimethod-subtable-nested lt spec)))
    (multimethod
      (multimethod-subtable tb) gen)))

(define (multimethod-subtable-nested lt spec)
  (if (eq? lt '())
      spec
      (let ((rv (make-hash-table 'equal)))
        (hash-table-put!
          rv (car lt) (multimethod-subtable-nested (cdr lt) spec))
        rv)))

(define (multimethod-lookup method args)
  ; destructure
  (let ((gen (vector-ref method 2))
        (subtb (vector-ref method 1)))
    (multimethod-subtable-lookup subtb () args)))

(define (multimethod-subtable-lookup subtb default-fn args)
  (let ((on-fail
          (lambda ()
            (hash-table-get subtb 'nil
              default-fn))))
    (if (multimethod-subtable? subtb)
        (multimethod-subtable-lookup
          (hash-table-get subtb (car (ar-type args))
            on-fail)
          on-fail
          (cdr args))
        subtb)))

(define (multimethod-add method lt spec)
  (err "multimethods not yet implemented!"))

(define (multimethod-from-monomethod mono)
  (err "multimethods not yet implemented!"))

;reductor

(define (reductor? f)
  (or (l-reductor? f) (r-reductor? f) (t-reductor? f)))

(define (reductor-f0 f)
  (vector-ref f 1))

(define (reductor-f1 f)
  (vector-ref f 2))

(define (reductor-f2 f)
  (vector-ref f 3))

(define (reductor-apply f args)
  (let ((la (length args)))
    (cond
      ((< la 3)
       (let ((f (vector-ref f (+ la 1))))
         (if (procedure? f)
             (apply f args)
             (ar-apply f args))))
      ; longer dispatching
      (#t
       (let ((f2 (ar-f2-to-lambda (reductor-f2 f))))
         (reductor-reduce f f2 args))))))

(define (reductor-reduce f f2 args)
  (cond
   ((l-reductor? f)
    (l-reduce f2 (f2 (car args) (cadr args)) (cddr args)))
   ; right reduction, sadly, just isn't tail recursive
   ((r-reductor? f)
    (f2 (car args) (r-reduce f2 (cdr args))))
   ((t-reductor? f)
                                             ; notice: only cdr
    (t-reduce f2 (f2 (car args) (cadr args)) (cdr args)))
   (#t
    (err "reductor type not implemented: " (vector-ref f 0)))))

(define (l-reduce f2 acc args)
  (if (eq? '() args)
      acc
      (l-reduce f2 (f2 acc (car args)) (cdr args))))

(define (l-reductor? f)
  (and (vector? f) (eq? (vector-ref f 0) 'l-reductor)))

(define (l-reductor f0 f1 f2)
  (vector 'l-reductor f0 f1 f2))

(define (r-reduce f2 args)
  (if (eq? '() (cdr args))
      (car args)
      (f2 (car args) (r-reduce f2 (cdr args)))))

(define (r-reductor? f)
  (and (vector? f) (eq? (vector-ref f 0) 'r-reductor)))

(define (r-reductor f0 f1 f2)
  (vector 'r-reductor f0 f1 f2))

(define (t-reduce f2 result args)
  (cond
    ((ar-false? result)
      'nil)
    ((eq? (cddr args) '())
     (f2 (car args) (cadr args)))
    (#t
     (t-reduce f2 (f2 (car args) (cadr args)) (cdr args)))))

(define (t-reductor? f)
  (and (vector? f) (eq? (vector-ref f 0) 't-reductor)))

(define (t-reductor f0 f1 f2)
  (vector 't-reductor f0 f1 f2))

(xdef 'l-reductor l-reductor)
(xdef 'r-reductor r-reductor)
(xdef 't-reductor t-reductor)

;<base>compose on functions

(define (composer fl fr)
  (vector 'composer fl fr))

(define (composer? fn)
  (and (vector? fn) (eq? (vector-ref fn 0) 'composer)))

(define (composer-fl fn)
  (vector-ref fn 1))

(define (composer-fr fn)
  (vector-ref fn 2))

(xdef '<base>compose composer)

;clojure-style multimethods

(define (clojure-method dispatch-fn . optional)
  (let ((generic
          (cond
            ((eq? optional '())
              (lambda rest
                (err "No generic function for dispatcher method")))
            ((eq? (cdr optional) '())
              (car optional))
            (#t
              (err "'dispatcher-method accepts at most 2 parameters")))))
    (vector 'clojure-method
            dispatch-fn
            (make-hash-table 'equal)
            generic)))

(define (clojure-method-add key new-fn orig-fn)
  (if (not (clojure-method? orig-fn))
      (err "'add-dispatcher-method expects a function generated by 'dispatcher-method"))
  (let ((new-table (make-hash-table 'equal)))
    (hash-table-for-each (vector-ref orig-fn 2)
      (lambda (k v)
        (hash-table-put! new-table k v)))
    (hash-table-put! new-table key new-fn)
    (vector 'clojure-method
            (vector-ref orig-fn 1)
            new-table
            (vector-ref orig-fn 3))))

(define (clojure-method? fn)
  (and (vector? fn) (eq? (vector-ref fn 0) 'clojure-method)))

(define (clojure-method-dispatch-fn fn)
  (vector-ref fn 1))

(define (clojure-method-lookup fn key)
  (hash-table-get (vector-ref fn 2) key
                  (lambda ()
                    (vector-ref fn 3))))

(define (clojure-method-apply fn args)
  (ar-apply (clojure-method-lookup fn (ar-apply (clojure-method-dispatch-fn fn) args)) args))

(xdef 'dispatcher-method clojure-method)
(xdef 'add-dispatcher-method clojure-method-add)

; polymorphism

(define (simplify-type-list lt)
  (cond
    ((ar-false? lt)
      '())
    ((all-falses? lt)
      '())
    (#t
      (cons (car lt) (simplify-type-list (cdr lt))))))

(define (all-falses? lt)
  (cond
    ((ar-false? lt) #t)
    ((and (ar-false? (car lt)) (all-falses? (cdr lt))) #t)
    (#t #f)))

; given:
;   lt = list of types (t1 t2 ...)
;   spec = specific function for that list of types
;   gen = function to extend
; returns a polymorphic function which
; invokes spec if the list of types
; matches the types of the parameters
;
; (defm <base>+ ((t a int) (t b int))
;   ($.add-ints a b))
; ==>
; (set <base>+
;   (polymorph
;     '(int int)
;     (fn (a b)
;       ($.add-ints a b))
;     <base>+))

; in future rename to true-polymorph
; and create ar-polymorph which puts
; the types into <arc> package first
(define (ar-polymorph lt spec gen)
  (true-polymorph
    (cadr (context-ref-reuse! arc-package-cxt `(idfn ,lt)))
    spec gen))

(define (true-polymorph lt spec gen)
  ; clean up the list
  (set! lt (simplify-type-list lt))
  (let ((llt (length lt)))
    (cond
      ((procedure? gen)
         (cond
           ((= llt 0)
            ; wants the generic method replaced, so...
            spec)
           ((= llt 1)
            (let ((hash (make-hash-table 'equal)))
              (hash-table-put! hash (car lt) spec)
              (ar-monomethod hash gen)))
           (#t
            (multimethod-from-generic lt spec gen))))
      ((monomethod? gen)
         (cond
           ((= llt 0)
            ; replace generic with this specific
            (ar-monomethod (vector-ref gen 1) spec))
           ((= llt 1)
            (let ((hash (make-hash-table 'equal)))
              ; copy old table
              ; inefficient? yes. so what? who's
              ; going to use 'polymorph in a tight
              ; loop?
              (hash-table-for-each (vector-ref gen 1)
                (lambda (k v)
                  (hash-table-put! hash k v)))
              ; insert new entry
              (hash-table-put! hash (car lt) spec)
              (ar-monomethod hash (vector-ref gen 2))))
           (#t
            (multimethod-add (multimethod-from-monomethod gen) lt spec))))
      ((multimethod? gen)
        (if (= llt 0)
            (multimethod (vector-ref gen 1) spec)
            (multimethod-add gen lt spec)))
      (#t
        (true-polymorph lt spec (lambda rest (ar-apply gen rest)))))))

(xdef 'polymorph true-polymorph)

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

(xdef 'car
  (ar-polymorph
    '(bool) (lambda (x) 'nil)
    (ar-polymorph
      '(cons) (lambda (x) (car x))
      (lambda (x)
        (err "'car expected a scanner" x)))))

(xdef 'cdr
  (ar-polymorph
    '(bool) (lambda (x) 'nil)
    (ar-polymorph
      '(cons) (lambda (x) (cdr x))
      (lambda (x)
        (err "'cdr expected a scanner" x)))))


; reduce? 

(define (pairwise pred args base)
  (let ((n (length args)))
    (cond ((< n 2) base)
          ((= n 2) (apply pred args))
          (#t (and (pred (car args) (cadr args))
                   (pairwise pred (cdr args) base))))))

(define (tnil x) (if x 't 'nil))

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

(xdef '<base>+
      ; when we have multimethods,
      ; use those instead of
      ; monomethods
      (ar-polymorph
        ; potentially also:
        ; (int int) fx+  ;although fx+ is r6rs (mzscheme 372 is r5rs)
        '(int) (lambda (a b) (+ a b))
        (ar-polymorph
          '(num) (lambda (a b) (+ a b))
          (ar-polymorph
            '(string) string-append
            (ar-polymorph
              '(cons) (lambda (a b)
                                 ; ar-nil-terminate creates
                                 ; a copy anyway
                        (append! (ar-nil-terminate a)
                                 (ar-nil-terminate b)))
              (ar-polymorph
                '(bool) (lambda (a b)
                          (if (ar-false? a)
                              b
                              (err "Attempted to add t to " b)))
                (lambda (a b)
                  (err "Unable to add " a " to " b))))))))

(xdef '<base>- (lambda (a b) (- a b)))
(xdef '<base>negate (lambda (a) (- a)))
(xdef '<base>* (lambda (a b) (* a b)))
(xdef '<base>/ (lambda (a b) (/ a b)))
(xdef '<base>reciprocal (lambda (a) (/ a)))
(xdef '<base>mod (lambda (a b) (modulo a b)))
(xdef '<base>quotient (lambda (a b) (quotient a b)))

(xdef 'expt expt)
(xdef 'sqrt sqrt)
(xdef 'log log)

; generic comparison

(xdef '<base>is
  (ar-polymorph
    '(string) (lambda (a b)
                (tnil (if (string? b) (string=? a b) #f)))
    (ar-polymorph
      '(bool) (lambda (a b)
                (tnil (if (ar-false? a) (ar-false? b)
                                        (eq? b 't))))
      (lambda (a b) (tnil (eqv? a b))))))

(xdef '<base><
      (ar-polymorph
        '(int) (lambda (a b) (tnil (< a b)))
        (ar-polymorph
          '(num) (lambda (a b) (tnil (< a b)))
          (ar-polymorph
            '(string) (lambda (a b) (tnil (string<? a b)))
            (ar-polymorph
              '(char) (lambda (a b) (tnil (char<? a b)))
              (ar-polymorph
                '(sym) (lambda (a b) (tnil (string<?
                                             (symbol->string a)
                                             (symbol->string b))))
                (lambda (a b)
                  (err "< unable to compare " a " to " b))))))))

(xdef 'len
      (ar-polymorph
        '(bool) (lambda (x) 0)
        (ar-polymorph
          '(string) string-length
          (ar-polymorph
            '(table) hash-table-count
            (ar-polymorph
              '(cons) (lambda (x) (length (ar-nil-terminate x)))
              (lambda (x)
                (err "len unable to compute length of " x)))))))

(define (ar-tagged? x)
  (and (vector? x) (eq? (vector-ref x 0) 'tagged)))

(define (ar-tag type rep)
  (cond ((eqv? (ar-type rep) type) rep)
        (#t (vector 'tagged type rep))))

(xdef 'annotate ar-tag)

; (type nil) -> bool

; hot spot
(define (ar-type x)
  (cond
        ((pair? x)          '<arc>cons)
        ((string? x)        '<arc>string)
        ((integer? x)       '<arc>int)
        ((number? x)        '<arc>num)     ; unsure about this
        ((hash-table? x)    '<arc>table)
        ((eq? x 't)         '<arc>bool)
        ((ar-false? x)      '<arc>bool)
        ((symbol? x)        '<arc>sym)
        ((ar-tagged? x)     (vector-ref x 1))
        ((ar-procedure? x)  '<axiom>fn) ; notice how this is an axiom ^^
        ((char? x)          '<arc>char)
        ((output-port? x)   '<arc>output)
        ((input-port? x)    '<arc>input)
        ((tcp-listener? x)  '<arc>socket)
        ((exn? x)           '<arc>exception)
        ((regexp? x)        '<arc>re)
        ((thread? x)        '<arc>thread)
        ((thread-cell? x)   '<arc>thread-local)
        ((semaphore? x)     '<arc>sema)
        ((context? x)       '<arc>cxt)
        ((package? x)       '<arc>pkg)
        (#t                 (err "Type: unknown type" x))))
(xdef 'type ar-type)

(define (ar-rep x)
  (if (ar-tagged? x)
      (vector-ref x 2)
      x))

(xdef 'rep ar-rep)

(xdef 'uniq gensym)

(xdef 'ccc (lambda (ar)
             (if (procedure? ar)
                 (call-with-current-continuation ar)
                 (call-with-current-continuation (lambda (k) (ar-funcall1 ar k))))))

(xdef 'dynamic-wind (lambda (a b c)
                      (dynamic-wind
                        (if (procedure? a) a (lambda () (ar-funcall0 a)))
                        (if (procedure? b) b (lambda () (ar-funcall0 b)))
                        (if (procedure? c) c (lambda () (ar-funcall0 c))))))

(xdef 'infile  open-input-file)

(xdef 'outfile (lambda (f . args) 
                 (open-output-file f 
                                   'text
                                   (if (equal? args '(<arc>append))
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
        (parameterize ((current-output-port port)) (ar-funcall0 thunk))))

(xdef 'call-w/stdin
      (lambda (port thunk)
        (parameterize ((current-input-port port)) (ar-funcall0 thunk))))

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
      (cond
        ; could print #<monomethod> instead, but!
        ;   1 people will then assume that monomethods
        ;     etc. are part of "the way arc-f is"
        ;   2 we don't want that: this is just a
        ;     function, nothing more, nothing less,
        ;     and monomethods etc. are implementation
        ;     details for arc-f-on-mzscheme.  they
        ;     are NOT arc!  'polymorph returns a
        ;     function, no there is no difference
        ;     between methods and functions
        ((monomethod? (car args))
          (display "#<procedure>" port))
        ((multimethod? (car args))
          (display "#<procedure>" port))
        ((reductor? (car args))
          (display "#<procedure>" port))
        ((composer? (car args))
          (display "#<procedure>" port))
        ((clojure-method? (car args))
          (display "#<procedure>" port))
        (#t (f (ac-denil (car args)) port))))
    (flush-output port))
    'nil)

(define ar-write (lambda args (printwith write   args)))

(xdef 'write ar-write)
(xdef 'disp  (lambda args (printwith display args)))

; sread = scheme read. eventually replace by writing read

(xdef 'sread (lambda (p eof)
               (let ((expr (ar-read p)))
                 (if (eof-object? expr) eof expr))))

; these work in PLT but not scheme48

(define char->ascii char->integer)
(define ascii->char integer->char)

(xdef 'coerce (lambda (x type . args)
                (cond 
                  ((ar-tagged? x) (err "Can't coerce annotated object"))
                  ((eqv? type (ar-type x)) x)

                  ((char? x)      (case type
                                    ((<arc>int)    (char->ascii x))
                                    ((<arc>string) (string x))
                                    ((<arc>sym)    (ar-string->symbol (string x)))
                                    (else          (err "Can't coerce" x type))))
                  ((integer? x)   (case type
                                    ((<arc>char)   (ascii->char x))
                                    ((<arc>string) (apply number->string x args))
                                    (else          (err "Can't coerce" x type))))
                  ((number? x)    (case type
                                    ((<arc>int)    (round x))
                                    ((<arc>char)   (ascii->char (round x)))
                                    ((<arc>string) (apply number->string x args))
                                    (else          (err "Can't coerce" x type))))
                  ((string? x)    (case type
                                    ((<arc>sym)    (ar-string->symbol x))
                                    ((<arc>cons)   (ac-niltree (string->list x)))
                                    ((<arc>int)    (or (apply string->number x args)
                                                  (err "Can't coerce" x type)))
                                    (else          (err "Can't coerce" x type))))
                  ((pair? x)      (case type
                                    ((<arc>string) (list->string
                                               (ar-nil-terminate x)))   
                                    (else          (err "Can't coerce" x type))))
                  ((eqv? x 'nil)  (case type
                                    ((<arc>string) "")
                                    (else          (err "Can't coerce" x type))))
                  ((symbol? x)    (case type 
                                    ((<arc>string) (symbol->string x))
                                    (else          (err "Can't coerce" x type))))
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

(xdef 'new-thread (lambda (k)
                    (thread (if (procedure? k) k (lambda () (ar-funcall0 k))))))
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
                  (dynamic-wind
                    (lambda () #t)
                    (if (procedure? during) during (lambda () (ar-funcall0 during)))
                    (if (procedure? after) after (lambda () (ar-funcall0 after))))))

; need to use a better seed

(xdef 'rand random)

(xdef 'dir (lambda (name) (map path->string (directory-list name))))

(xdef 'file-exists (lambda (name)
                     (if (file-exists? name) name 'nil)))

(xdef 'dir-exists (lambda (name)
                     (if (directory-exists? name) name 'nil)))

(xdef 'rmfile (wrapnil delete-file))

; assumes that the launching script was used
(define arc-path (getenv "arc_dir")) ; getenv is mzscheme-specific
(xdef 'arc-installation-path (lambda () (if arc-path arc-path 'nil)))

(xdef 'load-resolve
  (lambda (file)
    (or (load-resolve file)
        'nil)))

(define (load-resolve file)
  (let ((e-path (lambda (p)
                  (path->string (path->complete-path p)))))
    (cond
     ((not (string? file))
      (err "load-resolve expects a string"))
     ((file-exists? file)
      (e-path file))
     ;; absolute?, or can't find arc_dir?
     ((or (absolute-path? file)
	  (complete-path? file)
          (not arc-path))
      #f)
     ((file-exists? (build-path arc-path file))
      (e-path (build-path arc-path file)))
     ((file-exists? (build-path arc-path "lib" file))
      (e-path (build-path arc-path "lib" file)))
     (#t
      #f))))

; top level read-eval-print
; tle kept as a way to get a break loop when a scheme err

(define (arc-eval expr) 
  (eval (ac expr '()) (interaction-environment)))

(define (tle)
  (display "Arc> ")
  (let ((expr (ar-read)))
    (when (not (eqv? expr ':a))
      (ar-write (arc-eval expr))
      (newline)
      (tle))))

(define last-condition* #f)

(define (tl)
  (display "Use (quit) to quit, (tl) to return here after an interrupt.\n")
  (tl2 (make-context)))

(define (tl2 cxt)
  (display "<")
  (display (package-name (package-of-context cxt)))
  (display ">tl: ")
  (on-err (lambda (c) 
            (set! last-condition* c)
            (display "Error: ")
            (write (exn-message c))
            (newline)
            (tl2 cxt))
    (lambda ()
      (let ((expr (ar-read)))
        (if (and (ar-symbol? expr) (eq? (unpackaged-symbol expr) ':a))
            'done
            (let* ((post-cxt (context-ref cxt expr))
                   (val      (arc-eval post-cxt)))
              (arc-eval `(<arc>input-history-update (<axiom>quote ,post-cxt)))
              (arc-eval `(<arc>output-history-update (<axiom>quote ,val)))
              (ar-write (ac-denil val))
              (namespace-set-variable-value! '__<arc>that val)
              (namespace-set-variable-value! '__<arc>thatexpr post-cxt)
              (newline)
              (tl2 cxt)))))))

(define (probe x) (display "probe:") (write x) (display #\newline) x)

(define (aload1 p cxt)
  (let ((x (ar-read p)))
    (if (eof-object? x)
        #t
        (begin
          (arc-eval (context-ref-reuse! cxt x))
          (aload1 p cxt)))))

(define (atests1 p cxt)
  (let ((x (ar-read p)))
    (if (eof-object? x)
        #t
        (begin
          (write x)
          (newline)
          (let ((v (arc-eval (context-ref-reuse! cxt x))))
            (if (ar-false? v)
                (begin
                  (display "  FAILED")
                  (newline))))
          (atests1 p cxt)))))

(define (aload filename)
  (call-with-input-file filename
    (lambda (p)
      (aload1 p (make-context)))))

(define (test filename)
  (call-with-input-file filename
    (lambda (p)
      (atests1 p (make-context)))))

(define (acompile1 ip op cxt)
  (let ((x (ar-read ip)))
    (if (eof-object? x)
        #t
        (let ((scm (ac (context-ref-reuse! cxt x) '())))
          (pretty-print
            (compile
              (if (and (pair? x) (context-metacommand? (car x)))
                  (context-metacommand-compile cxt x)
                  scm))
            op)
          (eval scm (interaction-environment))
          (newline op)
          (newline op)
          (acompile1 ip op cxt)))))

; compile xx.arc to xx.arc.scm
; useful to examine the Arc compiler output
(define (acompile inname)
  (let ((outname (string-append inname ".scm"))
        (cxt (make-context)))
    (if (file-exists? outname)
        (delete-file outname))
    (call-with-input-file inname
      (lambda (ip)
        (call-with-output-file outname 
          (lambda (op)
            (acompile1 ip op (make-context))))))))

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
                                      (k (lambda () (ar-funcall1 errfn c)))))) 
                        (ar-funcall0 f)))))))
(xdef 'on-err on-err)

(define (disp-to-string x)
  (let ((o (open-output-string)))
    (display x o)
    (close-output-port o)
    (get-output-string o)))

(xdef 'details (lambda (c)
                 (disp-to-string (exn-message c))))

(xdef 'scar
  (ar-polymorph '(cons) (lambda (x val) (set-car! x val) val)
    (ar-polymorph '(string) (lambda (x val) (string-set! x 0 val) val)
      (lambda (x val)
        (err "Can't set car of" x)))))

(xdef 'scdr
  (ar-polymorph '(cons) (lambda (x val) (set-cdr! x val) val)
    (lambda (x val)
      (err "Can't set cdr of" x))))

; When and if cdr of a string returned an actual (eq) tail, could
; say (if (string? x) (string-replace! x val 1) ...) in scdr, but
; for now would be misleading to allow this, because fails for cddr.

(define (string-replace! str val index)
  (if (eqv? (string-length val) (- (string-length str) index))
      (do ((i index (+ i 1)))
          ((= i (string-length str)) str)
        (string-set! str i (string-ref val (- i index))))
      (err "Length mismatch between strings" str val index)))

(xdef 'sref
  (ar-polymorph '(table) (lambda (com val ind)
                           (if (ar-false? val)
                               (hash-table-remove! com ind)
                               (hash-table-put! com ind val))
                           val)
    (ar-polymorph '(string) (lambda (com val ind)
                              (string-set! com ind val)
                              val)
      (ar-polymorph '(cons) (lambda (com val ind)
                              (nth-set! com ind val)
                              val)
        (ar-polymorph '(thread-local) (lambda (com val)
                                        (thread-cell-set! com val)
                                        val)
          (ar-polymorph '(pkg) package-sref
            (lambda (com val . ind)
              (err "Can't set reference" com))))))))

(xdef 'call*
  (ar-polymorph '(table) (lambda (com ind)
                           (hash-table-get com ind 'nil))
    (ar-polymorph '(string) (lambda (com ind)
                              (string-ref com ind))
      (ar-polymorph '(cons) (lambda (com ind)
                              (list-ref com ind))
        (ar-polymorph '(thread-local) thread-cell-ref
          (ar-polymorph '(cxt) context-ref
            (ar-polymorph '(pkg) package-ref
              (lambda (com . rest)
                (err "Can't get reference (function call on inappropriate object)" com)))))))))

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

(xdef 'sema (lambda () (make-semaphore)))
(xdef 'sema-wait (wrapnil semaphore-wait))
(xdef 'sema-post (wrapnil semaphore-post))
(xdef 'sync sync)
(xdef 'synct (lambda (timeout . events)
               (if (ar-false? timeout)
                   (apply sync events)
                   (let ((rv (apply sync/timeout timeout events)))
                     (if rv rv 'nil)))))

; packages

(xdef 'cxt make-context)
(xdef 'pkg the-package)
(xdef 'pkg-of
      (ar-polymorph '(cxt) package-of-context
        (ar-polymorph '(sym) (lambda (a)
                               (or (the-package (package-of a))
                                   'nil))
          (lambda rest
            (err "'pkg-of expects a context or symbol")))))
(xdef 'pkg-name package-name)
(xdef 'unpkg unpackaged-symbol)
(xdef 'cxt-ref-d context-ref-reuse!)

)

