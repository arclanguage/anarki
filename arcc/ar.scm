; run-time primitive procedures

(module ar mzscheme

(provide (all-defined))
(require (lib "port.ss"))
(require (lib "process.ss"))
(require (lib "pretty.ss"))
(require (lib "foreign.ss"))
(unsafe!)

(define (ac-global-name s)
  (string->symbol (string-append "_" (symbol->string s))))

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

(define fn-signatures (make-hash-table 'equal))

; expose the scheme evaluator for the repl.
(xdef seval eval)

; This is a replacement for xdef that stores opeator signatures.
; Haven't started using it yet.

(define (odef a parms b)
  (namespace-set-variable-value! (ac-global-name a) b)
  (hash-table-put! fn-signatures a (list parms))
  b)

(xdef sig fn-signatures)

(define err error)
(xdef err err)

(xdef ac-global-name ac-global-name)
(xdef ac-emptylist '())

(define declarations (make-hash-table))
(xdef declarations* declarations)

(define (ar-bflag key)
  (not (ar-false? (hash-table-get declarations key 'nil))))

; macros return Arc lists, ending with NIL.
; but the Arc compiler expects Scheme lists, ending with '().
; what to do with (is x nil . nil) ?
;   the first nil ought to be replaced with 'NIL
;   the second with '()
; so the rule is: NIL in the car -> 'NIL, NIL in the cdr -> '().
;   NIL by itself -> NIL

(define (ac-denil x)
  (let ((orelse (lambda (x r) (if (eq? x 'nil) r (ac-denil x)))))
    (if (pair? x)
        (cons (orelse (car x) 'nil) (orelse (cdr x) '()))
        x)))
(xdef ac-denil ac-denil)

; #f and '() -> nil for a whole quoted list/tree.

; Arc primitives written in Scheme should look like:

; (xdef foo (lambda (lst)
;           (ac-niltree (scheme-foo (ar-nil-terminate lst)))))

; That is, Arc lists are NIL-terminated. When calling a Scheme
; function that treats an argument as a list, call ar-nil-terminate
; to change NIL to '(). When returning any data created by Scheme
; to Arc, call ac-niltree to turn all '() into NIL.
; (hash-table-get doesn't use its argument as a list, so it doesn't
; need ar-nil-terminate).

(define (ac-niltree x)
  (cond ((pair? x) (cons (ac-niltree (car x)) (ac-niltree (cdr x))))
        ((or (eq? x #f) (eq? x '())) 'nil)
        (#t x)))
(xdef ac-niltree ac-niltree)

; replace the nil at the end of a list with a '()

(define (ar-nil-terminate l)
  (if (or (eqv? l '()) (eqv? l 'nil))
      '()
      (cons (car l) (ar-nil-terminate (cdr l)))))

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
  (cond ((procedure? fn)
         (apply fn args))
        ((pair? fn)
         (list-ref fn (car args)))
        ((string? fn)
         (string-ref fn (car args)))
        ((hash-table? fn)
         (ar-nill (hash-table-get fn
                                  (car args)
                                  (if (pair? (cdr args)) (cadr args) #f))))
; experiment: means e.g. [1] is a constant fn
;       ((or (number? fn) (symbol? fn)) fn)
; another possibility: constant in functional pos means it gets
; passed to the first arg, i.e. ('kids item) means (item 'kids).
        (#t (err "Function call on inappropriate object" fn args))))

(xdef apply (lambda (fn . args)
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

(xdef nil 'nil)                         ; FIXME: oughtn't be necessary
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
                  (ac-niltree (apply append (map ar-nil-terminate args))))
                 (#t (apply + args)))))

(define (char-or-string? x) (or (string? x) (char? x)))

(define (ar-+2 x y)
  (cond ((char-or-string? x)
         (string-append (ar-coerce x 'string) (ar-coerce y 'string)))
        ((and (arc-list? x) (arc-list? y))
         (ac-niltree (append (ar-nil-terminate x) (ar-nil-terminate y))))
        (#t (+ x y))))

(xdef - -)
(xdef * *)
(xdef / /)
(xdef mod modulo)
(xdef expt expt)
(xdef sqrt sqrt)

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
                   (#t (length (ar-nil-terminate x))))))

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
        ((hash-table? x)    'table)
        ((output-port? x)   'output)
        ((input-port? x)    'input)
        ((tcp-listener? x)  'socket)
        ((exn? x)           'exception)
        ((thread? x)        'thread)
        (#t                 (err "Type: unknown type" x))))
(xdef type ar-type)

(define (ar-rep x)
  (if (ar-tagged? x)
      (vector-ref x 2)
      x))

(xdef rep ar-rep)

; currently rather a joke: returns interned symbols

(define ar-gensym-count 0)

(define (ar-gensym)
  (set! ar-gensym-count (+ ar-gensym-count 1))
  (string->symbol (string-append "gs" (number->string ar-gensym-count))))

(xdef uniq ar-gensym)

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

(xdef readb (lambda str
              (let ((c (read-byte (if (pair? str)
                                      (car str)
                                      (current-input-port)))))
                (if (eof-object? c) 'nil c))))

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

(define (printwith f args)
  (let ((port (if (> (length args) 1)
                  (cadr args)
                  (current-output-port))))
    (when (pair? args)
      (f (ac-denil (car args)) port))
    (unless (ar-bflag 'explicit-flush)
      (flush-output port)))
  'nil)

(xdef write (lambda args (printwith write   args)))
(xdef disp  (lambda args (printwith display args)))

; sread = scheme read. eventually replace by writing read

(xdef sread (lambda (p eof)
               (let ((expr (read p)))
                 (if (eof-object? expr) eof expr))))

; these work in PLT but not scheme48

(define char->ascii char->integer)
(define ascii->char integer->char)

(define (iround x) (inexact->exact (round x)))

(define (ar-coerce x type . args)
  (cond
    ((ar-tagged? x) (err "Can't coerce annotated object"))
    ((eqv? type (ar-type x)) x)
    ((char? x)      (case type
                      ((int)     (char->ascii x))
                      ((string)  (string x))
                      ((sym)     (string->symbol (string x)))
                      (else      (err "Can't coerce" x type))))
    ((exint? x)     (case type
                      ((num)     x)
                      ((char)    (ascii->char x))
                      ((string)  (apply number->string x args))
                      (else      (err "Can't coerce" x type))))
    ((number? x)    (case type
                      ((int)     (iround x))
                      ((char)    (ascii->char (iround x)))
                      ((string)  (apply number->string x args))
                      (else      (err "Can't coerce" x type))))
    ((string? x)    (case type
                      ((sym)     (string->symbol x))
                      ((cons)    (ac-niltree (string->list x)))
                      ((num)     (or (apply string->number x args)
                                     (err "Can't coerce" x type)))
                      ((int)     (let ((n (apply string->number x args)))
                                   (if n
                                       (iround n)
                                       (err "Can't coerce" x type))))
                      (else      (err "Can't coerce" x type))))
    ((pair? x)      (case type
                      ((string)  (apply string-append
                                        (map (lambda (y) (ar-coerce y 'string))
                                             (ar-nil-terminate x))))
                      (else      (err "Can't coerce" x type))))
    ((eqv? x 'nil)  (case type
                      ((string)  "")
                      (else      (err "Can't coerce" x type))))
    ((null? x)      (case type
                      ((string)  "")
                      (else      (err "Can't coerce" x type))))
    ((symbol? x)    (case type
                      ((string)  (symbol->string x))
                      (else      (err "Can't coerce" x type))))
    (#t             x)))

(xdef coerce ar-coerce)

(xdef open-socket  (lambda (num) (tcp-listen num 50 #t)))

; the 2050 means http requests currently capped at 2 meg
; http://list.cs.brown.edu/pipermail/plt-scheme/2005-August/009414.html

(xdef socket-accept (lambda (s)
                      (let ((oc (current-custodian))
                            (nc (make-custodian)))
                        (current-custodian nc)
                        (call-with-values
                         (lambda () (tcp-accept s))
                         (lambda (in out)
                           (let ((in1 (make-limited-input-port in 100000 #t)))
                             (current-custodian oc)
                             (associate-custodian nc in1 out)
                             (list in1
                                   out
                                   (let-values (((us them) (tcp-addresses out)))
                                               them))))))))

; allow Arc to give up root privileges after it
; calls open-socket. thanks, Eli!
(define setuid (get-ffi-obj 'setuid #f (_fun _int -> _int)))
(xdef setuid setuid)

(xdef new-thread thread)
(xdef kill-thread kill-thread)
(xdef break-thread break-thread)
(xdef current-thread current-thread)

(define (wrapnil f) (lambda args (apply f args) 'nil))

(xdef sleep (wrapnil sleep))

; Will system "execute" a half-finished string if thread killed
; in the middle of generating it?

(xdef system (wrapnil system))

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

; decide at run-time whether the underlying mzscheme supports
; set-car! and set-cdr!, since I can't figure out how to do it
; at compile time.

(define (x-set-car! p v)
  (let ((fn (namespace-variable-value 'set-car! #t (lambda () #f))))
    (if (procedure? fn)
        (fn p v)
        (n-set-car! p v))))

(define (x-set-cdr! p v)
  (let ((fn (namespace-variable-value 'set-cdr! #t (lambda () #f))))
    (if (procedure? fn)
        (fn p v)
        (n-set-cdr! p v))))

; Eli's code to modify mzscheme-4's immutable pairs.

;; to avoid a malloc on every call, reuse a single pointer, but make
;; it thread-local to avoid races
(define ptr (make-thread-cell #f))
(define (get-ptr)
  (or (thread-cell-ref ptr)
      (let ([p (malloc _scheme 1)]) (thread-cell-set! ptr p) p)))

;; set a pointer to the cons cell, then dereference it as a pointer,
;; and bang the new value in the given offset
(define (set-ca/dr! offset who p x)
  (if (pair? p)
    (let ([p* (get-ptr)])
      (ptr-set! p* _scheme p)
      (ptr-set! (ptr-ref p* _pointer 0) _scheme offset x))
    (raise-type-error who "pair" p)))

(define (n-set-car! p x) (set-ca/dr! 1 'set-car! p x))
(define (n-set-cdr! p x) (set-ca/dr! 2 'set-cdr! p x))

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
          (#t (err "Can't set reference " com ind val)))
    val))

(define (nth-set! lst n val)
  (x-set-car! (list-tail lst n) val))

; rewrite to pass a (true) gensym instead of #f in case var bound to #f

(define (bound? arcname)
  (namespace-variable-value (ac-global-name arcname)
                            #t
                            (lambda () #f)))

(xdef bound (lambda (x) (tnil (bound? x))))

(xdef newstring make-string)

(xdef trunc (lambda (x) (inexact->exact (truncate x))))

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

(define ar-the-sema (make-semaphore 1))

(define ar-sema-cell (make-thread-cell #f))

(xdef atomic-invoke (lambda (f)
                       (if (thread-cell-ref ar-sema-cell)
                           (ar-apply f '())
                           (begin
                             (thread-cell-set! ar-sema-cell #t)
			     (protect
			      (lambda ()
				(call-with-semaphore
				 ar-the-sema
				 (lambda () (ar-apply f '()))))
			      (lambda ()
				(thread-cell-set! ar-sema-cell #f)))))))

(xdef dead (lambda (x) (tnil (thread-dead? x))))

; Added because Mzscheme buffers output.  Not a permanent part of Arc.
; Only need to use when declare explicit-flush optimization.

(xdef flushout (lambda () (flush-output) 't))

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

(xdef sin sin)
(xdef cos cos)
(xdef tan tan)
(xdef asin asin)
(xdef acos acos)
(xdef atan atan)
(xdef log log)

)
