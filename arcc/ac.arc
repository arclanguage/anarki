; compiler itself
(def ac (s env)
  (case type.s
    string  (ac-string s env)
    sym     (if no.s `'nil
                ac-ssyntax.s (ac ac-ssexpand.s env)
                (ac-var-ref s env))
    cons    (let (f . a) s
              (case f
                quote      `',(ac-niltree car.a)
                quasiquote (ac-qq car.a env)
                if         (ac-if a env)
                fn         (ac-fn car.a cdr.a env)
                assign     (ac-assign a env)
                           (case (if acons.f car.f)
                             ; "optimizations"
                             compose    (ac (ac-decompose cdr.f a) env)
                             complement (ac `(no (,cadr.f ,@a)) env)
                             andf       (ac (ac-andf cdr.f a) env)
                                        (ac-call f a env))))
            (if (ac-literal? s) s
                (err "Bad object in expression" s))))

(= ac-declarations* (table))

(def ac-global-name (s) (sym:string "_" s))
(def ac-var-ref (s env) (if (ac-lex s env) s (ac-global-name s)))

; The next two are optimizations, except work for macros.

(def ac-decompose (fns args)
  (if no.fns `((fn vals (car vals)) ,@args)
      (~cdr fns) (cons car.fns args)
      `(,car.fns ,(decompose cdr.fns args))))

(def ac-andf (fns args env)
  (let gs (map [uniq] args)
    `((fn ,gs (and ,@(map [cons _ gs] fns))) ,@args)))

; string compilation
(def ac-string (s env)
  (if ac-declarations*!atstrings
      (if (some #\@ s)
          (let x ac-codestring.s
            (if (and (is len.x 1) (acons car.x) (is caar.x 'quote))
                (cadr:car x)
                `(string ,@x)))
          s)
      string.s))

(def ac-codestring (s)
  (accum a
    (with (tok nil
           add [a `',(string:rev _)])
      (w/instring p s
        (whilet c (readc p)
          (if (is c #\@)
            (if (in peekc.p #\@ nil)
                (do readc.p (push #\@ tok))
                (do (add tok)
                    (wipe tok)
                    (a:read p)))
            (push c tok))))
      (add tok))))

; ssyntax
(= ac-ssyntax* `((#\:              infix   nil  compose)
                 (#\~              prefix  nil  complement)
                 (,[in _ #\. #\!]  infix   t    ,ac-ssexpand-call:rev)
                 (#\&              infix   nil  andf)))

(def ac-ssyntax (x)
  (and (isa x 'sym)
       ;; no currying ssyntax, and andf is done via &, so we don't need this
       ;(~in x '+ '++ '_)
       (some (apply orf (map testify:car ac-ssyntax*)) string.x)))

(def ac-ssexpand (s)
  (catch:let n string.s
    (each (test fix keepsep expans) ac-ssyntax*
      (iflet ps (positions test n)
        (let xfrm (case type.expans
                    fn  expans
                    sym (case fix infix [cons expans _]
                                  prefix (fn (x) `(,expans ,x)))
                        (err "Bad ssyntax expansion:" expans))
          (throw:on-err
            (fn (e) (if (is details.e "Bad ssyntax")
                      (err "Bad ssyntax" s)
                      (err details.e)))
            (fn ()
              (case fix
                prefix (if (is ps.0 0)
                           (xfrm ((if keepsep [cons n.0 _] idfn) (sym:cut n 1)))
                           (err "Bad ssyntax"))
                infix  (xfrm:ac-toksplit n ps keepsep)
                       (err "Bad ssyntax fixity:" fix)))))))
    s))

(def ac-toksplit (str posits (o keepsep))
  (let sub (fn (a b) (if (isnt a b) (list:sym:cut str a b)))
    (join (sub 0 posits.0)
          (mappend (fn (a b) ((if keepsep [cons str.a _] idfn)
                              (sub (+ a 1) b)))
                   posits (+ cdr.posits (list:len str))))))

(def ac-ssexpand-call (toks)
  (iflet (a . rest) toks
    (if (in a #\. #\!) (err "Bad ssyntax")
      (iflet (b . rest) rest
        (list (ac-ssexpand-call rest)
          (case b
            #\. a
            #\! (list 'quote a)
            (err "Bad ssyntax")))
        a))
    'get))


; primitive forms

; quasiquotes
(def ac-qq (args env)
  `(quasiquote ,(ac-qq1 1 args env)))

(def ac-qq1 (level x env)
  (if (is level 0) (ac x env)
      atom.x x
      (case car.x
        unquote `(,'unquote ,(ac-qq1 (- level 1) cadr.x env))
        unquote-splicing `(,'unquote-splicing
                           ,((if (is level 1) [list 'ar-nil-terminate _] idfn)
                             (ac-qq1 (- level 1) cadr.x env)))
        quasiquote `(,'quasiquote ,(ac-qq1 (+ level 1) cadr.x env))
        (ac-map [ac-qq1 level _ env] x))))

; (if) -> nil
; (if x) -> x
; (if t a ...) -> a
; (if nil a ...) -> (if ...)

(def ac-if (args env)
  (if no.args (list 'quote nil)
      (~cdr args) (ac car.args env)
      `(if (ar-boolean ,(ac car.args env))
           ,(ac cadr.args env)
           ,(ac-if cddr.args env))))

; translate fn directly into a lambda if it has ordinary
; parameters, otherwise use a rest parameter and parse it.

(def ac-fn (args body env)
  (ac-nameit (ac-dbname-find env)
    (if (ac-simple-args? args)
        `(lambda ,(or ac-denil.args ac-emptylist)
           ,@(ac-body body (join ac-arglist.args env)))
        (ac-complex-fn args body env))))

; does an fn arg list use optional parameters or destructuring?
; a rest parameter is not complex

(def ac-simple-args? (args)
  (or (and acons.args
           (isa car.args 'sym)
           (ac-simple-args? cdr.args))
      (isa args 'sym)))

; translate a fn with optional or destructuring args
; (fn (x (o y x) (o z 21) (x1 x2) . rest) ...)
; arguments in top-level list are mandatory (unless optional),
; but it's OK for parts of a list you're destructuring to
; be missing.

(def ac-complex-fn (args body env)
  (with (ra (uniq)
         z (ac-complex-args args env ra t))
    `(lambda ,ra
       (let* ,ac-denil.z
         ,@(ac-body* body (join ac-complex-getargs.z env))))))

; returns a list of two-element lists, first is variable name,
; second is (compiled) expression. to be used in a let.
; caller should extract variables and add to env.
; ra is the rest argument to the fn.
; is-params indicates that args are function arguments
;   (not destructuring), so they must be passed or be optional.

(def ac-complex-args (args env ra is-params)
  (if no.args ac-emptylist
      (isa args 'sym) `((,args ,ra))
      acons.args
        (withs ((a . r) args
                x (if (and acons.a (is car.a 'o))
                      (ac-complex-opt cadr.a (car:cddr a) env ra)
                      (ac-complex-args a env (if is-params `(car ,ra)
                                                 `(ar-xcar ,ra)) nil))
                xa ac-complex-getargs.x)
          (join x (ac-complex-args
                    cdr.args (join xa env) `(ar-xcdr ,ra) is-params)))
      (err "Can't understand fn arg list" args)))

; (car ra) is the argument
; so it's not present if ra is nil or '()

(def ac-complex-opt (var expr env ra)
  (list (list var `(if (pair? ,ra) (car ,ra) ,(ac expr env)))))

; extract list of variables from list of two-element lists.

(def ac-complex-getargs (a) (map car a))

; (a b . c) -> (a b c)
; a -> (a)

(def ac-arglist (a)
  (if (isa a 'sym) (and a list.a)
      (cons car.a (ac-arglist cdr.a))))

(def ac-body (body env) (map [ac _ env] body))

; like ac-body, but spits out a nil expression if empty

(def ac-body* (body env)
  (if body (ac-body body env) `'nil))

; (assign v1 expr1 v2 expr2 ...)

(def ac-assign (x env)
  `(begin ,@(map (fn ((a b)) (ac-assign1 ac-macex.a b env)) pair.x)))

; = replaced by assign, which is only for vars
; = now defined in arc (is it?)
; name is to cause fns to have their arc names for debugging

(def ac-assign1 (a b1 env)
  (if (isa a 'sym)
      `(let ((zz ,(ac b1 (ac-dbname cons a env))))
         ,(if (in a nil t) (err "Can't rebind" a)
              (ac-lex a env) `(set! ,a zz)
              `(namespace-set-variable-value! ',(ac-global-name a) zz))
         zz)
      (err "First arg to assign must be a symbol" a)))

; given a list of Arc expressions, return a list of Scheme expressions.
; for compiling passed arguments.

(def ac-args (names exprs env)
  (when exprs
    (cons (ac car.exprs (ac-dbname-cons car.names env))
          (ac-args cdr.names cdr.exprs env))))

; generate special fast code for ordinary two-operand
; calls to the following functions. this is to avoid
; calling e.g. ar-is with its &rest and apply.

(= ac-binaries (obj is 'ar-is2
                    < 'ar-<2
                    > 'ar->2
                    + 'ar-+2))

; (foo bar) where foo is a global variable bound to a procedure.

(def ac-global-call (fun args env)
  (aif (and (is len.args 2) ac-binaries.fun)
       `(,it ,@(ac-args nil args env))
       `(,(ac-global-name fun) ,@(ac-args nil args env))))

; compile a function call
; special cases for speed, to avoid compiled output like
;   (ar-apply _pr (list 1 2))
; which results in 1/2 the CPU time going to GC. Instead:
;   (ar-funcall2 _pr 1 2)
; and for (foo bar), if foo is a reference to a global variable,
;   and it's bound to a function, generate (foo bar) instead of
;   (ar-funcall1 foo bar)

(= ac-funcall* (obj 0 'ar-funcall0
                    1 'ar-funcall1
                    2 'ar-funcall2
                    3 'ar-funcall3
                    4 'ar-funcall4))

(def ac-call (fun args env)
  (aif ac-macro.fun (ac-mac-call it args env)
       (if (and acons.fun (is car.fun 'fn))
             `(,(ac-fn cadr.fun cddr.fun env) ,@(ac-args cadr.fun args env))
           (and ac-declarations*!direct-calls
                (isa fun 'sym)
                (not:ac-lex fun env)
                (bound fun)
                (isa eval.fun 'fn))
             (ac-global-call fun args env)
           (with (cfun (ac fun env)
                  cargs (map [ac _ env] args))
             (aif (ac-funcall* len.cargs)
                  `(,it ,cfun ,@cargs)
                  `(ar-apply ,cfun (list ,cargs)))))))

(def ac-mac-call (m args env)
  (ac (ac-denil (apply m (map ac-niltree args))) env))

; returns nil or the macro function

(def ac-macro (fun)
  (and (isa fun 'sym) (bound fun)
       (rep:check eval.fun [isa _ 'mac])))

; macroexpand the outer call of a form as much as possible

(def ac-macex (e (o n))
  (if (and acons.e (isnt n 0))
      (aif (ac-macro car.e)
           (ac-macex (ac-denil:apply it (map ac-niltree cdr.e))
                     (if (isa n 'int) (- n 1)))
           e)
      e))


; helpers
(= ac-lex mem)

(def ac-literal? (x)
  (in type.x 'char 'int 'num))

; like map, but doesn't demand nil-terminated lists, and gives back a
; ()-terminated list.
(def ac-map (f l)
  (if acons.l (cons (f:car l) (ac-map f cdr.l))
      no.l ac-emptylist
      (f l)))

; a hack to get '() instead of 'nil
(= ac-emptylist (read "()"))

; trick to tell Scheme the name of something, so Scheme
; debugging and profiling make more sense.

(def ac-dbname-cons (name env)
  (if (and (isa name 'sym) name)
      (cons list.name env)
      env))

(def ac-dbname-find (env)
  (car:find acons env))

(def ac-nameit (name v)
  (if (and (isa name 'sym) name)
      (let n (sym:string " " name)
        `(let ((,n ,v)) ,n))
      v))

; macros return Arc lists, ending with NIL.
; but the Arc compiler expects Scheme lists, ending with '().
; what to do with (is x nil . nil) ?
;   the first nil ought to be replaced with 'NIL
;   the second with '()
; so the rule is: NIL in the car -> 'NIL, NIL in the cdr -> '().
;   NIL by itself -> NIL

(def ac-denil (x)
  (let orelse (fn (x r) (if x ac-denil.x r))
    (if acons.x (cons (orelse car.x car.x)
                      (orelse cdr.x ac-emptylist))
        x)))

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

(def ac-niltree (x)
  (if acons.x (cons (ac-niltree car.x) (ac-niltree cdr.x))
      no.x nil
      x))
