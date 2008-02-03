
;;; Implementation of Arc in SBCL
;;; (c) Pau Fernandez

(declaim (optimize (debug 3)))

(require :sb-bsd-sockets)

(defpackage :arc
  (:use :cl :sb-bsd-sockets :sb-ext)
  (:export :arc-eval 
	   :fn 
	   :if))

(in-package :arc)

(defun ac (s &optional env)
  (let ((head (xcar s)))
    (cond ((typep s 'string) (copy-seq s))
	  ((literal? s) s)
	  ((eql s 'nil) (list 'quote 'nil))
	  ((ssyntax? s) (ac (expand-ssyntax s) env))
	  ((symbolp s) (ac-var-ref s env))
	  ((ssyntax? head) (ac (cons (expand-ssyntax head) (cdr s)) env))
	  ((eq head 'quote) (list 'quote (ac-niltree (cadr s))))
	  ((backq? head) (ac-backq head (cdr s) env))
	  ((eq head 'fn) (ac-fn (cadr s) (cddr s) env))
	  ((eq head 'if) (ac-if (cdr s) env))
	  ((eq head 'set) (ac-set (cdr s) env))
	  ((eq (xcar head) 'compose) (ac (decompose (cdar s) (cdr s)) env))
	  ((consp s) (ac-call (car s) (cdr s) env))
	  (t (error "Bad object in expression [~a]" s)))))

(defun literal? (x)
  (or (or (eq x 't) (eq x 'nil))
      (characterp x)
      (stringp x)
      (numberp x)
      (eq x '())))

(defun ssyntax? (x)
  (and (symbolp x)
       (not (or (eq x '+) (eq x '++)))
       (let ((name (symbol-name x)))
         (has-ssyntax-char? name (- (length name) 1)))))

(defparameter *cmp-ch* #\.)
(defparameter *neg-ch* #\~)

(defun has-ssyntax-char? (string i)
  (and (>= i 0)
       (or (let ((c (char string i)))
	     (or (char= c *cmp-ch*) (char= c *neg-ch*)))
	   (has-ssyntax-char? string (1- i)))))

; read-from-string: Lisp has it.

(defun expand-ssyntax (sym)
  (let ((elts (mapcar #'(lambda (tok)
			  (if (eq (car tok) *neg-ch*)
			      `(complement ,(chars->value (cdr tok)))
			      (chars->value tok)))
		      (tokens *cmp-ch* (symbol->chars sym) nil nil))))
    (if (null (cdr elts))
	(car elts)
	(cons 'compose elts))))

(defun symbol->chars (x)
  (string->list (symbol-name x)))

(defun chars->value (chars)
  (read-from-string (list->string chars)))

(defun list->string (x)
  (map 'string #'identity x))

(defun string->list (x)
  (map 'list #'identity x))


(defun tokens (separator source token acc)
  (cond ((null source)
         (reverse (cons (reverse token) acc)))
        ((eq (car source) separator)
         (tokens separator
                 (cdr source)
                 '()
                 (cons (reverse token) acc)))
        (t
         (tokens separator
                 (cdr source)
                 (cons (car source) token)
                 acc))))

(defun decompose (fns args)
  (cond ((null fns) `((fn vals (car vals)) ,@args))
	((null (cdr fns)) (cons (car fns) args))
	(t (list (car fns) (decompose (cdr fns) args)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun arcsym (patt val)
    (intern (format nil patt val) (find-package :arc))))

(defun ac-global-name (s)
  (arcsym "_~a" (symbol-name s)))

(defun ac-var-ref (s env)
  (if (lex? s env)
      s 
      (ac-global-name s)))

(defparameter *backq-syms*
  '(sb-impl::backq-list
    sb-impl::backq-list*
    sb-impl::backq-cons
    sb-impl::backq-append))

(defun backq? (head)
  (member head *backq-syms*))

(defun ac-backq (head args env)
  (cons head (mapcar #'(lambda (x) (ac x env)) args)))

(defun ac-if (args env)
  (cond ((null args) ''nil)
	((null (cdr args)) (ac (car args) env))
	(t `(if (not (ar-false? ,(ac (car args) env)))
		,(ac (cadr args) env)
		,(ac-if (cddr args) env)))))

(defun ac-fn (args body env)
  (if (ac-complex-args? args)
      (ac-complex-fn args body env)
      (labels ((_argl (x)
		 (cond ((consp (cdr x))
			(cons (car x) (_argl (cdr x))))
		       ((null (cdr x))
			(cons (car x) nil))
		       (t (cons (car x) (list '&rest (cdr x))))))
	       (_args (a) (cond ((eql a 'nil) '())
				((symbolp a) `(&rest ,a))
				((consp a) (_argl a))
				(t a))))
	`(lambda ,(_args (ac-denil args))
	   ,@(ac-body* body (append (ac-arglist args) env))))))

(defun ac-complex-args? (args)
  (cond ((eql args '()) nil)
	((symbolp args) nil)
	((symbolp (xcar args))
	 (ac-complex-args? (cdr args)))
	(t t)))

(defun ac-complex-fn (args body env)
  (let* ((ra (ar-gensym))
	 (z  (ac-complex-args args env ra t)))
    `(lambda ,ra
       (let ,z
	 'nil
	 ,@(ac-body body (append (ac-complex-getargs z) env))))))

(defun ac-complex-args (args env ra is-params)
  (cond ((or (eql args '()) (eql args 'nil) 'nil))
	((symbolp args) (list (list args ra)))
	((consp args) 
	 (let* ((x (if (and (consp (car args)) (eql (caar args) 'o))
                       (ac-complex-opt (cadar args) 
                                       (if (consp (cddar args))
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
                        nil)))
                (xa (ac-complex-getargs x)))
           (append x (ac-complex-args (cdr args)
                                      (append xa env)
                                      `(ar-xcdr ,ra)
                                      is-params))))
	(t (error "Can't understand fn arg list [~a]" args))))

(defun ac-complex-opt (var expr env ra)
  (list (list var `(if (consp ,ra) (car ,ra) ,(ac expr env)))))

(defun ac-complex-getargs (a)
  (mapcar #'car a))

(defun ac-arglist (a)
  (cond ((null a) '())
	((symbolp a) (list a))
	((symbolp (cdr a)) (list (car a) (cdr a)))
	(t (cons (car a) (ac-arglist (cdr a))))))

(defun ac-body (body env)
  (mapcar #'(lambda (x) (ac x env)) body))

(defun ac-body* (body env)
  (if (null body)
      (list (list 'quote 'nil))
      (ac-body body env)))

(defun ac-set (x env)
  `(progn ,@(ac-setn x env)))

(defun ac-setn (x env)
  (if (null x)
      nil
      (cons (ac-set1 (ac-macex (car x)) (ac (cadr x) env) env)
	    (ac-setn (cddr x) env))))

(defun ac-set1 (a b env)
  (if (symbolp a)
      (let ((name (intern (format nil "%~a" (symbol-name a)))))
	(list 'let `((,name ,b))
	      (cond ((eql a 'nil) (error "Can't rebind nil"))
		    ((eql a 't) (error "Cant' rebind t"))
		    ((lex? a env) `(setf ,a ,name))
		    (t `(set ',(ac-global-name a) ,name)))
	      name))
      (error "First arg to set must be a symbol [~a]" a)))

(defun ac-call (fn args env)
  (let ((macfn (ac-macro? fn)))
    (if macfn
	(ac-mac-call macfn args env)
	(let ((afn   (ac fn env))
	      (aargs (mapcar #'(lambda (x) (ac x env)) args))
	      (nargs (length args)))
	  (cond
	    ((eql (xcar fn) 'fn)
	     `(,afn ,@aargs))
	    ((and (<= 0 nargs 4))
	     `(,(arcsym "AR-FUNCALL~d" nargs)
		(symbol-value ',afn) ,@aargs))
	    (t 
	     `(ar-apply ,afn (list ,@aargs))))))))

(defun ac-mac-call (m args env)
  (ac (ac-denil (apply m (mapcar #'ac-niltree args))) env))

(defun ac-macro? (fn)
  (if (symbolp fn)
      (let* ((sym (ac-global-name fn))
	     (v (and (boundp sym) (symbol-value sym))))
	(if (and v (ar-tagged? v) (eq (ar-type v) 'mac))
	    (ar-rep v)
	    nil))
      nil))

(defun ac-macex (e &rest once)
  (let ((m (ac-macro? (xcar e))))
    (if m
	(let ((expns (ac-denil (apply m (mapcar #'ac-niltree (cdr e))))))
	  (if (null once) 
	      (ac-macex expns) 
	      expns))
	e)))

; ac-denil not needed in Lisp... (?)

(defun ac-denil (x) x)

(defun lex? (v env)
  (member v env))

(defun xcar (x)
  (and (consp x) (car x)))

; ac-niltree not needed in Lisp... (?)

(defun ac-niltree (x) x)
       
; run-time primitive procedures

(defun xdef (sym val)
  (set (ac-global-name sym) val))

(defmacro mxdef (name args &body body)
  `(xdef ',name (lambda ,args ,@body)))


(defparameter fn-signatures (make-hash-table :test #'equal))

(defun odef (a parms b)
  (set (ac-global-name a) b)
  (setf (gethash a fn-signatures) (list parms))
  b)

(xdef 'sig fn-signatures)

(defun ar-xcar (x)
  (xcar x))

(defun ar-xcdr (x)
  (and (consp x) (cdr x)))

(defun ar-nill (x) x)

(defun ar-false? (x) (not x))

(defun ar-apply (fn args)
  (cond ((functionp fn) (apply fn args))
	((consp fn) (nth (car args) fn))
	((stringp fn) (char fn (car args)))
	((hash-table-p fn) (ar-nill (gethash (car args) fn)))
	(t (error "Function call on inappropriate object [~a ~a]" fn args))))

(mxdef apply (fn &rest args)
  (ar-apply fn (ar-apply-args args)))

(macrolet ((_ar-funcall-x (num &rest args)
	     `(defun ,(arcsym "AR-FUNCALL~d" num) (fn ,@args)
		(if (functionp fn)
		    (funcall fn ,@args)
		    (ar-apply fn (list ,@args))))))
  (_ar-funcall-x 0)
  (_ar-funcall-x 1 arg1)
  (_ar-funcall-x 2 arg1 arg2)
  (_ar-funcall-x 3 arg1 arg2 arg3)
  (_ar-funcall-x 4 arg1 arg2 arg3 arg4))

; Not needed in Lisp... (?)

(defun ar-nil-terminate (l) l)

(defun ar-apply-args (args)
  (cond ((null args) nil)
	((null (cdr args)) (ar-nil-terminate (car args)))
	(t (cons (car args) (ar-apply-args (cdr args))))))

(xdef 'cons #'cons)

(mxdef car (x)
  (cond ((consp x) (car x))
	((null x) nil)
	(t (error "Can't take car of ~a" x))))

(mxdef cdr (x)
  (cond ((consp x) (cdr x))
	((null x) nil)
	(t (error "Cant' take cdr of ~a" x))))

(defun pairwise (pred args base)
  (let ((n (length args)))
    (cond ((< n 2) base)
	  ((= n 2) (apply pred args))
	  (t (and (funcall pred (car args) (cadr args))
		  (pairwise pred (cdr args) base))))))

(mxdef is (&rest args)
  (or (every #'(lambda (x) (eql (car args) x)) (cdr args))
      (and (every #'stringp args)
	   (every #'(lambda (x) (string= (car args) x)) (cdr args)))
      (every #'ar-false? args)))

(xdef 'err #'error)
(xdef 'nil 'nil)
(xdef 't 't)

(defun all (test seq)
  (or (null seq)
      (and (funcall test (car seq)) 
	   (all test (cdr seq)))))

(defun arc-list? (x)
  (or (consp x) (eql x nil)))

(mxdef + (&rest args)
  (cond ((null args) 0)
	((every #'stringp args)
	 (apply #'concatenate (cons 'string args)))
	((every #'arc-list? args)
	 (ac-niltree (apply #'append (mapcar #'ar-nil-terminate args))))
	(t (apply #'+ args))))

(xdef '- #'-)
(xdef '* #'*)
(xdef '/ #'/)
(xdef 'mod #'mod)
(xdef 'expt #'expt)
(xdef 'sqrt #'sqrt)

(macrolet ((_arc-comp (name f fstr fchar)
	     `(progn
		(defun ,name (&rest args)
		  (cond ((every #'numberp args) 
			 (apply #',f args))
			((every #'stringp args) 
			 (pairwise #',fstr args nil))
			((every #'symbolp args) 
			 (pairwise (lambda (x y)
				     (,fstr (symbol-name x)
					    (symbol-name y)))
				   args
				   nil))
			((all #'characterp args) 
			 (pairwise #',fchar args nil))
			(t (apply #',f args))))
		(mxdef ,f (&rest args)
		  (if (apply #',name args) t nil)))))
  (_arc-comp arc> > string> char>)
  (_arc-comp arc< < string< char<))

(mxdef len (x)
  (cond ((stringp x) (length x))
	((hash-table-p x) (hash-table-count x))
	(t (length (ar-nil-terminate x)))))

(defun ar-tagged? (x)
  (and (vectorp x) (eq (svref x 0) 'tagged)))

(defun ar-tag (type rep)
  (cond ((eql (ar-type rep) type) rep)
	(t (vector 'tagged type rep))))

(xdef 'annotate #'ar-tag)

(defun ar-type (x)
  (cond ((ar-tagged? x) (svref x 1))
	((null x)       'sym)
	((consp x)      'cons)
	((symbolp x)    'sym)
	((functionp x)  'fn)
	((characterp x) 'char)
	((stringp x)    'string)
	((integerp x)   'int)
	((numberp x)    'num)
	((hash-table-p x)    'table)
	((output-stream-p x) 'output)
	((input-stream-p x)  'input)
	((typep x 'socket)   'socket)
	((typep x 'error)    'error)
	(t (error "Type: unknown type [~a]" x))))

(xdef 'type #'ar-type)

(defun ar-rep (x)
  (if (ar-tagged? x) (svref x 2) x))

(xdef 'rep #'ar-rep)

(defun ar-gensym ()
  (gensym "arc"))

;; ccc not implemented... yet.

(mxdef infile (file)
  (open file :direction :input))

(mxdef outfile (file &rest args)
  (open file 
	:direction :output 
	:if-exists (if (equal args '(append))
		       :append
		       :overwrite)))

(xdef 'instring #'make-string-input-stream)
(xdef 'outstring #'make-string-output-stream)
(xdef 'inside #'get-output-stream-string)

(mxdef close (p)
  (cond ((input-stream-p p) (close p))
	((output-stream-p p) (close p))
	((typep p 'socket) (sb-bsd-sockets:socket-close p))
	(t (error "Can't close ~a" p)))
  nil)

(xdef 'stdout *standard-output*)
(xdef 'stdin  *standard-input*)
(xdef 'stderr *error-output*)

(mxdef call-w/stdout (port thunk)
  (let ((*standard-output* port)) (funcall thunk)))

(mxdef call-w/stdin (port thunk)
  (let ((*standard-input* port)) (funcall thunk)))

(macrolet ((_f (name fn)
	     `(mxdef ,name (stream)
		(let ((s (if (ar-false? stream)
			     *standard-input*
			     stream)))
		  (,fn s nil nil)))))
  (_f readc read-char)
  (_f readb read-byte)
  (_f peekc peek-char))

(macrolet ((_port (args)
	     `(if (consp ,args) 
		  (car ,args) 
		  *standard-output*))
	   (_wr1 (name prm fn)
	     `(mxdef ,name (,prm &rest args)
		(,fn ,prm (_port args))
		,prm))
	   (_wr2 (name)
	     `(mxdef ,name (&rest args)
		(when (consp args)
		  (write (ac-denil (car args))
			 :stream (_port (cdr args))))
		(force-output)
		nil)))
  (_wr1 writec c write-char)
  (_wr1 writeb b write-byte)
  (_wr2 write)
  (_wr2 disp))

(mxdef sread (p eof)
  (read p nil eof))

(defun char->ascii (c)
  (char-code c))

(defun ascii->char (c)
  (code-char c))

(defun number->string (n &optional (radix 10) precision)
  (declare (ignore precision)) ;; will polish later...
  (format nil (format nil "~~~DR" radix) n))

(defun string->number (str &optional (radix 10))
  (parse-integer str :radix radix))

(mxdef coerce (x type &rest args)
  (flet ((_err () (error "Can't coerce ~a ~a" x type)))
    (cond ((ar-tagged? x) 
	   (error "Can't coerce annotated object [~a]" x))
	  ((eql type (ar-type x)) x)
	  ((characterp x) 
	   (case type
	     (int    (char->ascii x))
	     (string (string x))
	     (sym    (intern (string x)))
	     (t      (_err))))
	  ((integerp x)
	   (case type
	     (char   (ascii->char x))
	     (string (apply #'number->string x args))
	     (t      (_err))))
	  ((numberp x)
	   (case type
	     (int    (round x))
	     (char   (ascii->char (round x)))
	     (string (apply #'number->string x args))
	     (t      (_err))))
	  ((stringp x)
	   (case type 
	     (sym    (intern x))
	     (cons   (ac-niltree (map 'list #'identity x)))
	     (int    (or (apply #'string->number x args)))
	     (t      (_err))))
	  ((consp x)
	   (case type
	     (string (map 'list #'identity (ar-nil-terminate x)))
	     (t      (_err))))
	  ((null x)
	   (case type
	     (string "")
	     (t      (_err))))
	  ((symbolp x)
	   (case type
	     (string (symbol-name x))
	     (t      (_err))))
	  (t         x))))

(mxdef open-socket (num)
  (let ((sk (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (setf (sockopt-reuse-address sk) t)
    (socket-bind sk #(0 0 0 0) num) ;; 0.0.0.0 ????
    (sb-bsd-sockets:socket-listen sk 15) ;; from Araneida... 15?
    sk))

; (mxdef socket-accept (sk) ...) ;;; ufff

; (xdef 'thread ...)
; (xdef 'kill-thread ...)
; (xdef 'break-thread ...)

(mxdef sleep (n)
  (sleep n)
  nil)

(mxdef system (cmd)
  (process-wait 
   (run-program "/bin/sh" (list "-c" cmd))) ; dirty trick for PATH
  nil)

(mxdef pipe-from (cmd)
  (let ((p (run-program "/bin/sh" (list "-c" cmd))))
    (process-output p)))

(mxdef table ()
  (make-hash-table :test #'equal))

(mxdef maptable (fn table)
  (maphash fn table))

(mxdef protect (during after)
  (unwind-protect (funcall during)
    (funcall after)))

(xdef 'rand #'random)

; dir
; file-exists
; rmfile

(defun arc-eval (expr)
  (eval (ac expr ())))

(defun tle ()
  (loop
     (write "Arc> ")
     (let ((expr (read)))
       (when (not (eql expr :a))
	 (write (arc-eval expr))
	 (terpri)))))

(defparameter *last-condition* nil)

(defun tl ()
  (format t "Use (quit) to quit, (tl) to return here after an interrupt.~%")
  (tl2))

(defun tl2 ()
  (flet ((_repl ()
	   (loop 
	      (princ "arc> ")
	      (let ((expr (read)))
		(if (eql expr :a)
		    (return 'done)
		    (let ((val (arc-eval expr)))
		      (write (ac-denil val))
		      (set '_that val)
		      (set '_thatexpr expr)
		      (terpri)))))))
    (loop
       (handler-case (_repl)
	 (error (e) (format t "Error: ~a~%" e))))))
      
(defun aload1 (s)
  (loop for x = (read s nil t)
     while x
     do (format t "; eval: ~a~%" (arc-eval x)))
  t)

;; atests1

(defun aload (file)
  (with-open-file (s file) (aload1 s)))

;; test

(defun acompile1 (in out)
  (loop for x = (read in nil nil)
     while x
     do (let ((lisp (ac x ())))
	  (eval lisp)
	  (format out "~a~%~%" lisp))))

(defun acompile (inname)
  (let ((outname (format nil "~a.lisp" inname)))
    (with-open-file (in inname)
      (with-open-file (out outname 
			   :direction :output
			   :if-exists :supersede)
	(acompile1 in out)))))

(mxdef macex (e)
  (ac-macex (ac-denil e)))

(mxdef macex1 (e)
  (ac-macex (ac-denil e) 'once))

(mxdef eval (e)
  (eval (ac (ac-denil e) ())))

(mxdef on-err (errfn f)
  (handler-case (funcall f)
    (error (e) (funcall errfn e))))

; details

(mxdef scar (x val)
  (if (stringp x) 
      (setf (char x 0) val)
      (setf (car x) val)))

(mxdef scdr (x val)
  (if (stringp x)
      (error "Can't set cdr of a string [~a]" x)
      (setf (cdr x) val)))

; sref

(mxdef bound (x)
  (boundp (ac-global-name x)))

(xdef 'newstring #'make-string)

; trucate
; exact
; msec
; current-process-milliseconds
; current-gc-milliseconds

; seconds
; client-ip
; atomic-invoke
; dead

(mxdef ssyntax (x)
  (if (ssyntax? x) t nil))

(mxdef ssexpand (x)
  (if (symbolp x) (expand-ssyntax x) x))

(mxdef seval (x)
  (eval (ac-denil x)))

(xdef 'quit #'sb-ext:quit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;   Bracket notation

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\] 
    (get-macro-character #\) nil))
  (set-macro-character #\[
    #'(lambda (stream char)
	(declare (ignore char))
	(list 'fn (list '_) (read-delimited-list #\] stream t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;   Load arc.arc

; (aload "arc.arc")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;   Test suite 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *tests* nil))

(defvar *curr* '(nil . 0))
(defvar *failed* nil)

(defmacro deftest (name &body body)
  (pushnew name *tests*)
  `(defun ,name ()
     (setf *curr* (cons ',name 1))
     (format t "~a~15t " ',name)
     (macrolet ((_e (x) `(arc-eval ',x))) ,@body)
     (terpri)))
     
(defun chk (res)
  (princ (if res #\. #\+))
  (when (not res) (push (copy-list *curr*) *failed*))
  (incf (cdr *curr*)))

(defun test ()
  (setf *failed* nil)
  (loop for _t in (reverse *tests*)
     do (funcall (symbol-function _t)))
  (when *failed*
    (format t "~%Failed:~%")
    (let (prev)
      (dolist (f (reverse *failed*))
	(if (eq prev (car f))
	    (format t " [~a]" (cdr f))
	    (format t "~&  ~a [~a]" (car f) (cdr f)))
	(setf prev (car f)))))
  (values))

(defgeneric == (a b)
  (:method (a b) (equal a b)))

(deftest t-literals
  (chk (== 1    (_e 1)))
  (chk (== #\a  (_e #\a)))
  (chk (== "a"  (_e "a")))
  (chk (== 1.45 (_e 1.45)))
  (chk (== 'nil (_e 'nil)))
  (chk (== nil  (_e nil)))
  (chk (== t    (_e t)))
  (chk (== '(quote a) (_e '(quote a))))
  (chk (== '(1 2 3) (_e '(1 2 3)))))

(deftest t-operations
  (chk (== 3  (_e (+ 1 2))))
  (chk (== 6  (_e (* 2 3))))
  (chk (== 4  (_e (/ 12 3))))
  (chk (== 1  (_e (- 2 1))))
  (chk (== 0  (_e (+))))
  (chk (== 1  (_e (*))))
  (chk (== -1 (_e (- 1))))
  (chk (== 1  (_e (+ 1))))
  (chk (== 15 (_e (+ (+ 1 2) (+ 3 (+ 4 5)))))))

(deftest t-ar-funcall-n
  (chk (== 'ar-funcall0 (car (ac '(+)))))
  (chk (== 'ar-funcall1 (car (ac '(+ 1)))))
  (chk (== 'ar-funcall2 (car (ac '(+ 1 2)))))
  (chk (== 'ar-funcall3 (car (ac '(+ 1 2 3)))))
  (chk (== 'ar-funcall4 (car (ac '(+ 1 2 3 4)))))
  (chk (== 10 (_e (+ 1 1 1 1 1 1 1 1 1 1)))))

(deftest t-funcalls
  (chk (== 3 (_e ((fn (x) (+ 1 x)) 2))))
  (chk (== 5 (_e ((fn (a b) (+ a b)) 2 3))))
  (chk (== '((2 3) . 1) (_e ((fn (x . y) (cons y x)) 1 2 3))))
  (chk (== '((1 2)) (_e ((fn x x) '(1 2))))))

(deftest t-if 
  (chk (== 0 (_e (if t 0 1))))
  (chk (== 1 (_e (if nil 0 1))))
  (chk (== 0 (_e (if t   0 nil 1 2))))
  (chk (== 1 (_e (if nil 0 t   1 2))))
  (chk (== 2 (_e (if nil 0 nil 1 2)))))

(deftest t-backq
  (chk (== '(1 2)         (_e ((fn (x) `(1 ,x)) 2))))
  (chk (== '(1 2 3)       (_e ((fn (x y) `(,x 2 ,y)) 1 3))))
  (chk (== '(1 2 3)       (_e ((fn (x) `(1 ,@x)) '(2 3)))))
  (chk (== '(1 2 3 4)     (_e ((fn (x y) `(1 ,@x ,y)) '(2 3) 4))))
  (chk (== '(1 (2 3))     (_e ((fn (x) `(1 (2 ,x))) 3))))
  (chk (== '(1 (2 (3 4))) (_e ((fn (x) `(1 (2 ,x))) '(3 4)))))
  (chk (== '((1 () 2 3))  (_e ((fn x `((1 () ,@x))) 2 3)))))

(deftest t-set 
  (chk (== #\a  (_e ((fn (x) (set x #\a) x) #\z)))))

(deftest t-brackets
  (chk (== 2 (_e ([+ _ 1] 1)))))
