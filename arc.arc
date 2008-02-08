; Main Arc lib.  Ported to Scheme version Jul 06.

; optimize ~foo in functional position in ac, like compose
; rename: string, into-string (shorter).  could call intos string,
;  but then what to call string?
; get hold of error types within arc
; why is macex defined in scheme instead of using def below?
; write disp, read, write in arc
; could prob write rmfile and dir in terms of system
; could I get all of macros up into lib.arc?

; any logical reason I can't say (push x (if foo y z)) ?
;   eval would have to always ret 2 things, the val and where it came from
; idea: implicit tables of tables; setf empty field, becomes table
;   or should setf on a table just take n args?
; idea: permanent objs that live on disk and are updated when modified

; compromises in this implementation:
; no objs in code
;  (mac testlit args (listtab args)) breaks when called
; separate string type
;  (= (cdr (cdr str)) "foo") couldn't work because no way to get str tail

(set *help* (table))

(set do (annotate 'mac
          (fn args `((fn () ,@args)))))
;documentation for do itself
(sref *help*
  '(fn " Evaluates each expression in sequence. ")
  'do)
(sref sig
  'args
  'do)

(set safeset (annotate 'mac
               (fn (var val)
                 `(do (if (bound ',var)
                          (do (disp "*** redefining ")
                              (disp ',var)
                              (writec #\newline)))
                      (set ,var ,val)))))

; It would be nice if multiple strings counted as multiple docstring lines.
(set def (annotate 'mac
            (fn (name parms . body)
              `(do (sref sig ',parms ',name)
                   ; Document the function, including the docstring if present
                   (if (is (type ',(car body)) 'string)
                       (sref *help* '(fn ,(car body)) ',name)
                       (sref *help* '(fn nil) ',name))
                   (safeset ,name (fn ,parms ,@body))))))
;documentation for def itself
(sref *help*
  '(fn " Defines a function with the given `name', `parms', and `body'. ")
  'def)
(sref sig
  '(name parms . body)
  'def)

(def caar (xs) " Equivalent to (car (car xs)) " (car (car xs)))
(def cadr (xs) " Equivalent to (car (cdr xs)) " (car (cdr xs)))
(def cddr (xs) " Equivalent to (cdr (cdr xs)) " (cdr (cdr xs)))

(def no (x) " Determines if `x' is `nil'. " (is x nil))

(def acons (x) " Determines if `x' is a `cons' cell or list. " (is (type x) 'cons))

(def atom (x) " Determines if `x' is atomic. " (no (acons x)))

(def list args " Creates a list from the given parameters. " args)

(def idfn (x)
  " Identity function - just returns its argument. "
  x)

; Maybe later make this internal.

(def map1 (f xs)
  " Return a sequence with function f applied to every element in sequence xs. "
  (if (no xs)
      nil
      (cons (f (car xs)) (map1 f (cdr xs)))))

(def pair (xs (o f list))
  " Applies pairs of elements to the function f. "
  (if (no xs)
       nil
      (no (cdr xs))
       (list (list (car xs)))
      (cons (f (car xs) (cadr xs))
            (pair (cddr xs) f))))

(set mac (annotate 'mac
           (fn (name parms . body)
             `(do (sref sig ',parms ',name)
                   ; Document the macro, including the docstring if present
                   (if (is (type ',(car body)) 'string)
                       (sref *help* '(mac ,(car body)) ',name)
                       (sref *help* '(mac nil) ',name))
                  (safeset ,name (annotate 'mac (fn ,parms ,@body)))))))
;documentation for mac itself
(sref *help*
  '(fn " Defines a macro, a special function which transforms code. ")
  'mac)
(sref sig
  '(name parms . body)
  'mac)

(mac and args
  " Evaluates arguments till false is found else returns the last one. "
  (if args
      (if (cdr args)
          `(if ,(car args) (and ,@(cdr args)))
          (car args))
      't))

(def assoc (key al)
  " Finds a (key value) pair in an associated list. "
  (if (atom al)
       nil
      (and (acons (car al)) (is (caar al) key))
       (car al)
      (assoc key (cdr al))))

(def alref (al key)
  " Get a value from a key in a associated list. "
  (cadr (assoc key al)))

(mac with (parms . body)
  " Assigns a set of local variables for the given `body'.
    Assignment is simultaneous. "
  `((fn ,(map1 car (pair parms))
     ,@body)
    ,@(map1 cadr (pair parms))))

(mac let (var val . body)
  " Assigns a local variable for the given `body'. "
  `(with (,var ,val) ,@body))

(mac withs (parms . body)
  " Assigns local variables for the given `body'.
    The assignments are made in the given order."
  (if (no parms)
      `(do ,@body)
      `(let ,(car parms) ,(cadr parms)
         (withs ,(cddr parms) ,@body))))

; Rtm prefers to overload + to do this

(def join args
  " Joins all list arguments together. "
  (if (no args)
      nil
      (let a (car args)
        (if (no a)
            (apply join (cdr args))
            (cons (car a) (apply join (cdr a) (cdr args)))))))

(mac rfn (name parms . body)
  " Self-referencing function expression.
    Creates a closure wherein lambda is bound to name. "
  `(let ,name nil
     (set ,name (fn ,parms ,@body))))

(mac afn (parms . body)
  " Self-referencing lambda expression.
    Creates a closure wherein lambda is bound to name.
    Use self as the name."
  `(rfn self ,parms ,@body))

(mac compose args
  " Arc expands x:y:z into (compose x y z)
    quick way to write (x(y(z)))
    Only used when the call to compose doesn't occur in functional position.
    Composes in functional position are transformed away by ac. "
  (let g (uniq)
    `(fn ,g
       ,((afn (fs)
           (if (cdr fs)
               (list (car fs) (self (cdr fs)))
               `(apply ,(if (car fs) (car fs) 'idfn) ,g)))
         args))))

(mac complement (f)
  " Arc expands ~x into (complement x)
    whenever the function returns true this returns false."
  (let g (uniq)
    `(fn ,g (no (apply ,f ,g)))))

(def rev (xs)
  " Reverses a sequence. "
  ((afn (xs acc)
     (if (no xs)
         acc
         (self (cdr xs) (cons (car xs) acc))))
   xs nil))

(def isnt (x y)
  " Inverse of is. "
  (no (is x y)))

(mac w/uniq (names . body)
  " Assigns a set of variables to unique symbols.
    Generally used for macro functions. "
  (if (acons names)
      `(with ,(apply + nil (map1 (fn (n) (list n '(uniq)))
                             names))
         ,@body)
      `(let ,names (uniq) ,@body)))

(mac or args
  " Computes arguments until one of them is true and returns that result. "
  (and args
       (w/uniq g
         `(let ,g ,(car args)
            (if ,g ,g (or ,@(cdr args)))))))

(def alist (x)
  " Is this a (non-empty) list?
    Return true if argument consists of cons pairs. "
  (or (no x) (is (type x) 'cons)))

(mac or= (var val)
  " Performs (or var val) and assigns the result to var. "
  `(= ,var (or ,var ,val)))

(mac in (x . choices)
  " Returns true if the first argument is one of the other arguments. "
  (w/uniq g
    `(let ,g ,x
       (or ,@(map1 (fn (c) `(is ,g ,c)) choices)))))

; should take n args

(def iso (x y)
  " Isomorphic compare - compares structure (can be slow). "
  (or (is x y)
      (and (acons x)
           (acons y)
           (iso (car x) (car y))
           (iso (cdr x) (cdr y)))))

(mac when (test . body)
  " When `test' is true, do `body'. "
  `(if ,test (do ,@body)))

(mac unless (test . body)
  " When `test' is not true, do `body'. "
  `(if (no ,test) (do ,@body)))

(mac while (test . body)
  " While `test' is true, perform `body' in a loop. "
  (w/uniq (gf gp)
    `((rfn ,gf (,gp)
        (when ,gp ,@body (,gf ,test)))
      ,test)))

(def empty (seq)
  " Test to see if `seq' is an empty list or other sequence. "
  (or (no seq)
      (and (no (acons seq)) (is (len seq) 0))))

(def reclist (f xs)
  " Applies the function `f' on the elements of `xs' until `f' returns true. "
  (and xs (or (f xs) (reclist f (cdr xs)))))

(def recstring (test s (o start 0))
  " Applies the function `test' on the characters of `s' until `test' returns true. "
  (let n (len s)
    ((afn (i)
       (and (< i (len s))
            (or (test i)
                (self (+ i 1)))))
     start)))

(def isa (x y)
  " Checks if x is of type y. "
  (is (type x) y))

(def testify (x)
  " Creates a test that determines if a given argument is `x'. "
  (if (isa x 'fn) x (fn (_) (is _ x))))

(def some (test seq)
  " Determines if at least one element of `seq' satisfies `test'. "
  (let f (testify test)
    (if (alist seq)
        (reclist f:car seq)
        (recstring f:seq seq))))

(def all (test seq)
  " Determines if all elements of `seq' satisfy `test'. "
  (~some (complement (testify test)) seq))

(def dotted (x)
  (if (atom x)
      nil
      (and (cdr x) (or (atom (cdr x))
                       (dotted (cdr x))))))

; I couldn't find a pre-existing total macro-expander
(def expand (expr)
  (if (and (acons expr) (~dotted expr))
      (macex (cons (car expr)
                   (map1 expand (cdr expr))))
      expr))

(def makeproper (lst)
  (if (no (acons lst))
      lst
      (cons (car lst)
            (if (alist (cdr lst))
              (makeproper (cdr lst))
              (list (cdr lst))))))

(def andmap (pred seq)
  (or
    seq
    (and
      (pred (car seq))
      (andmap pred (cdr seq)))))

(def ormap (pred seq)
  (and
    seq
    (or
      (pred (car seq))
      (ormap pred (cdr seq)))))

(def arglist-vars (arglist)
  (if (is arglist 'cons)
    (apply join
      (map1
        (fn (_)
          (if (isa _ 'cons)
            (if (is (car _) 'o)
              (list:cadr _)
              _)
            (list _)))
        (makeproper arglist)))
    arglist))

(def arglist-frees (arglist)
  (if (isa arglist 'cons)
    (apply join
      (map1
        (fn (_)
          (and (isa _ 'cons)
               (is (car _) 'o)
               (all-vars (car:cdar _))))
        (makeproper arglist)))
    nil))

(def all-vars (form)
  (let head (and (isa form 'cons) (car form))
    (if
      (or (no form) (and (no (isa form 'sym)) (no (isa form 'cons))))
        nil
      (isa form 'sym)
        (list form)
      (is head 'quote)
        nil
      (is head 'quasiquote)
        (ormap
          (fn (_)
            (and (isa _ 'cons)
                 (in (car _) 'unquote 'unquote-splicing)
                 (apply join (map1 all-vars (cdr _)))))
          (cdr form))
      (is head 'if)
        (apply join (map1 all-vars (cdr form)))
      (is head 'fn)
        (join (apply join (map1 all-vars (arglist-vars  (cadr form))))
              (apply join (map1 all-vars (arglist-frees (cadr form))))
              (apply join (map1 all-vars                (cddr form))))
      ; else (including set)
        (apply join (map1 all-vars form)))))

(def free? (form var)
  ; I'd like to use case, but it doesn't exist yet.
  (with (kind (type form)
         find (afn (x lst)
                (if (and (alist lst) lst)
                  (or (is x (car lst) (self x (cdr lst))))
                  nil)))
    (if
      (is kind 'sym)
        (is form var)
      (is kind 'cons)
        (let head (car form)
          (if
            (is head 'fn)
              (or (find var (arglist-frees (cadr form)))
                  (and (no (find var (arglist-vars (cadr form))))
                       (free? (cddr form) var)))
            (is head 'quote)
              #f
            (is head 'quasiquote)
              (ormap
                (fn (_)
                  (and (isa _ 'cons)
                       (in (car _) 'unquote 'unquote-splicing)
                       (free? (cdr _) var)))
                form)
            ; else
              (ormap (fn (_) (free? _ var)) form)))
    ; else
      nil)))

(def filter (pred lst)
  (if (acons lst)
    (if (pred (car lst))
      (cons (car lst) (filter pred (cdr lst)))
      (filter pred (cdr lst)))
    lst))

(mac make-br-fn (body)
  (with (max 0 arbno nil)
    (map1 ; Just for the side-effect; used as "max"
      (fn (_)
        (withs (s (coerce _ 'string) first (s 0) rest (coerce (cdr (coerce s 'cons)) 'string))
          (and (is (s 0) #\_)
               (or (is rest "")
                   (is rest "_")
                   (and (some (fn (c) (isnt c #\0)) rest)
                        (all  (fn (c) (in c #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)) rest)))
               (free? body _)
               (let num (or (and (is rest "") 1) (and (is rest "_") (do (set arbno t) -1)) (coerce rest 'int))
                 (when (> num max) (set max num)))
               nil)))
      (all-vars (expand body)))
    `(fn ,((afn (n)
             (if (< n (+ max 1))
               (cons (coerce (+ "_" (coerce n 'string)) 'sym) (self (+ n 1)))
               (if arbno
                 '__
                 (if (is max 0) '(_) nil)))) 1)
         ,(if (> max 0)
            `(let _ _1 ,body)
            body))))

;;; NO []s ABOVE THIS LINE ;;;

(def mem (test seq)
  " Returns the sublist of `seq' whose first element satisfies `test'."
  (let f (testify test)
    (reclist [if (f:car _) _] seq)))

(def find (test seq)
  " Returns the first element that matches the test function. "
  (let f (testify test)
    (if (alist seq)
        (reclist   [if (f:car _) (car _)] seq)
        (recstring [if (f:seq _) (seq _)] seq))))

; Possible to write map without map1, but makes News 3x slower.

;(def map (f . seqs)
;  (if (some1 no seqs)
;       nil
;      (no (cdr seqs))
;       (let s1 (car seqs)
;         (cons (f (car s1))
;               (map f (cdr s1))))
;      (cons (apply f (map car seqs))
;            (apply map f (map cdr seqs)))))


(def map (f . seqs)
  " Applies the elements of the sequences to the given function.
    Returns a sequence containing the results of the function. "
  (if (some [isa _ 'string] seqs)
       (withs (n   (apply min (map len seqs))
               new (newstring n))
         ((afn (i)
            (if (is i n)
                new
                (do (sref new (apply f (map [_ i] seqs)) i)
                    (self (+ i 1)))))
          0))
      (no (cdr seqs))
       (map1 f (car seqs))
      ((afn (seqs)
        (if (some no seqs)
            nil
            (cons (apply f (map1 car seqs))
                  (self (map1 cdr seqs)))))
       seqs)))

(def mappend (f . args)
  " Applies the elements of the seqeunces to the given function.
    Returns a sequence containing the concatenation of the results
    of the function."
  (apply + nil (apply map f args)))

(def firstn (n xs)
  " Returns the first `n' elements of the given list `xs'. "
  (if (and (> n 0) xs)
      (cons (car xs) (firstn (- n 1) (cdr xs)))
      nil))

(def nthcdr (n xs)
  " Returns the sublist of `xs' starting on the `n'th element.
    `n' is 0-based. "
  (if (> n 0)
      (nthcdr (- n 1) (cdr xs))
      xs))

; Generalization of pair: (tuples x) = (pair x)

(def tuples (xs (o n 2))
  " Returns a list of lists of the elements of `xs', grouped by `n'. "
  (if (no xs)
      nil
      (cons (firstn n xs)
            (tuples (nthcdr n xs) n))))

(def caris (x val) " Determines if (car x) is `val'. " (and (acons x) (is (car x) val)))

(def warn (msg . args)
  " Displays a warning message on its arguments. "
  (disp (+ "Warning: " msg ". "))
  (map [do (write _) (disp " ")] args)
  (disp #\newline))

(mac atomic body
  " Performs `body' atomically, blocking other threads. "
  `(atomic-invoke (fn () ,@body)))

(mac atlet args
  " Performs a `let' atomically, blocking other threads. "
  `(atomic (let ,@args)))

(mac atwith args
  " Performs a `with' atomically, blocking other threads. "
  `(atomic (with ,@args)))

(mac atwiths args
  " Performs a `withs' atomically, blocking other threads. "
  `(atomic (withs ,@args)))

; setforms returns (vars get set) for a place based on car of an expr
;  vars is a list of gensyms alternating with expressions whose vals they
;   should be bound to, suitable for use as first arg to withs
;  get is an expression returning the current value in the place
;  set is an expression representing a function of one argument
;   that stores a new value in the place

; A bit gross that it works based on the *name* in the car, but maybe
; wrong to worry.  Macros live in expression land.

; seems meaningful to e.g. (push 1 (pop x)) if (car x) is a cons.
; can't in cl though.  could I define a setter for push or pop?

(set setter (table))

(mac defset (name parms . body)
  " Defines a setter for the named form. "
  (w/uniq gexpr
    `(sref setter
           (fn (,gexpr)
             (let ,parms (cdr ,gexpr)
               ,@body))
           ',name)))

(defset car (x)
  (w/uniq g
    (list (list g x)
          `(car ,g)
          `(fn (val) (scar ,g val)))))

(defset cdr (x)
  (w/uniq g
    (list (list g x)
          `(cdr ,g)
          `(fn (val) (scdr ,g val)))))

(defset caar (x)
  (w/uniq g
    (list (list g x)
          `(caar ,g)
          `(fn (val) (scar (car ,g) val)))))

(defset cadr (x)
  (w/uniq g
    (list (list g x)
          `(cadr ,g)
          `(fn (val) (scar (cdr ,g) val)))))

(defset cddr (x)
  (w/uniq g
    (list (list g x)
          `(cddr ,g)
          `(fn (val) (scdr (cdr ,g) val)))))

; Note: if expr0 macroexpands into any expression whose car doesn't
; have a setter, setforms assumes it's a data structure in functional
; position.  Such bugs will be seen only when the code is executed, when
; sref complains it can't set a reference to a function.

(def setforms (expr0)
  (let expr (macex expr0)
    (if (isa expr 'sym)
         (w/uniq (g h)
           (list (list g expr)
                 g
                 `(fn (,h) (set ,expr ,h))))
        ; make it also work for uncompressed calls to compose
        (and (acons expr) (metafn (car expr)))
         (setforms (expand-metafn-call (ssexpand (car expr)) (cdr expr)))
         (let f (setter (car expr))
           (if f
               (f expr)
               ; assumed to be data structure in fn position
               (do (when (caris (car expr) 'fn)
                     (warn "Inverting what looks like a function call"
                           expr0 expr))
                   (w/uniq (g h)
                     (let argsyms (map [uniq] (cdr expr))
                        (list (+ (list g (car expr))
                                 (mappend list argsyms (cdr expr)))
                              `(,g ,@argsyms)
                              `(fn (,h) (sref ,g ,h ,@argsyms)))))))))))

(def metafn (x)
  (or (ssyntax x)
      (and (acons x) (in (car x) 'compose 'complement))))

(def expand-metafn-call (f args)
  (if (is (car f) 'compose)
      ((afn (fs)
         (if (caris (car fs) 'compose)            ; nested compose
              (self (join (cdr (car fs)) (cdr fs)))
             (cdr fs)
              (list (car fs) (self (cdr fs)))
             (cons (car fs) args)))
       (cdr f))
      (err "Can't invert " (cons f args))))

(def expand= (place val)
  (if (isa place 'sym)
      `(set ,place ,val)
      (let (vars prev setter) (setforms place)
        (w/uniq g
          `(atwith ,(+ vars (list g val))
             (,setter ,g))))))

(def expand=list (terms)
  `(do ,@(map (fn ((p v)) (expand= p v))  ; [apply expand= _]
                  (pair terms))))

(mac = args
  " Assigns values to variables. "
  (expand=list args))

(mac loop (start test update . body)
  " First performs `start'; while `test' is true, performs `body' then
    `update' in a loop. "
  (w/uniq (gfn gparm)
    `(do ,start
         ((rfn ,gfn (,gparm)
            (if ,gparm
                (do ,@body ,update (,gfn ,test))))
          ,test))))

(mac for (v init max . body)
  " Loops for the variable `v' from `init' to `max'. "
  (w/uniq (gi gm)
    `(with (,v nil ,gi ,init ,gm (+ ,max 1))
       (loop (set ,v ,gi) (< ,v ,gm) (set ,v (+ ,v 1))
         ,@body))))

(mac repeat (n . body)
  " Repeats the `body' `n' times."
  `(for ,(uniq) 1 ,n ,@body))

; could bind index instead of gensym

(mac each (var expr . body)
  " Performs `body' for each element of the sequence returned by `expr',
    with each element assigned to `var'. "
  (w/uniq (gseq g)
    `(let ,gseq ,expr
       (if (alist ,gseq)
            ((afn (,g)
               (when (acons ,g)
                 (let ,var (car ,g) ,@body)
                 (self (cdr ,g))))
             ,gseq)
           (isa ,gseq 'table)
            (maptable (fn (,g ,var) ,@body)
                      ,gseq)
            (for ,g 0 (- (len ,gseq) 1)
              (let ,var (,gseq ,g) ,@body))))))

; (nthcdr x y) = (subseq y x).

(def subseq (seq start (o end (len seq)))
  " Returns a subsequence of the given `seq'. "
  (if (isa seq 'string)
      (let s2 (newstring (- end start))
        (for i 0 (- end start 1)
          (= (s2 i) (seq (+ start i))))
        s2)
      (firstn (- end start) (nthcdr start seq))))

(def prefix (pre str)
  " Determines if `pre' is the same as the first part of `str'. "
  (and (<= (len pre) (len str))
       (is pre (subseq str 0 (len pre)))))

(mac ontable (k v h . body)
  " Loops through the entries of table `h', with key-value pairs assigned
    to `k' and `v', respectively. "
  `(maptable (fn (,k ,v) ,@body) ,h))

(mac whilet (var test . body)
  " While `test' is true, perform `body' in a loop.
    The result of `test' is assigned to `var'. "
  (w/uniq (gf gp)
    `((rfn ,gf (,gp)
        (let ,var ,gp
          (when ,var ,@body (,gf ,test))))
      ,test)))

(def last (seq)
  " Returns the last element of `seq'. "
  (if (no (cdr seq))
      (car seq)
      (last (cdr seq))))

(def rem (test seq)
  " Returns a list with the elements of `seq' that pass `test' removed. "
  (let f (testify test)
    (if (alist seq)
        ((afn (s)
           (if (no s)       nil
               (f (car s))  (self (cdr s))
                            (cons (car s) (self (cdr s)))))
          seq)
        (coerce (rem test (coerce seq 'cons)) 'string))))

(def keep (test seq)
  " Returns a list with the elements of `seq' that pass `test'. "
  (rem (complement (testify test)) seq))

(def trues (f seq) " Returns a list with all `nil's removed. " (rem nil (map f seq)))

(mac do1 args
  " Performs the body in sequence, then returns the value of the
    first expression. "
  (w/uniq g
    `(let ,g ,(car args)
       ,@(cdr args)
       ,g)))

; Would like to write a faster case based on table generated by a macro,
; but can't insert objects into expansions in Mzscheme.

(mac caselet (var expr . args)
  " Matches the result of `expr' to arguments until one matches.
    The result of `expr' is assigned to `var'. "
  (let ex (afn (args)
            (if (no (cdr args))
                (car args)
                `(if (is ,var ',(car args))
                     ,(cadr args)
                     ,(self (cddr args)))))
    `(let ,var ,expr ,(ex args))))

(mac case (expr . args)
  " Matches the result of `expr' to arguments until one matches. "
  `(caselet ,(uniq) ,expr ,@args))

(mac push (x place)
  " Pushes the value `x' on the front of the list in `place'. "
  (w/uniq gx
    (let (binds val setter) (setforms place)
      `(let ,gx ,x
         (atwiths ,binds
           (,setter (cons ,gx ,val)))))))

(mac swap (place1 place2)
  " Swaps the values of the specified places. "
  (w/uniq (g1 g2)
    (with ((binds1 val1 setter1) (setforms place1)
           (binds2 val2 setter2) (setforms place2))
      `(atwiths ,(+ binds1 (list g1 val1) binds2 (list g2 val2))
         (,setter1 ,g2)
         (,setter2 ,g1)))))

(mac rotate places
  " Rotates the values of the specified places, from right to left. "
  (with (vars (map [uniq] places)
         forms (map setforms places))
    `(atwiths ,(mappend (fn (g (binds val setter))
                          (+ binds (list g val)))
                        vars
                        forms)
       ,@(map (fn (g (binds val setter))
                (list setter g))
              (+ (cdr vars) (list (car vars)))
              forms))))

(mac pop (place)
  " Pops a value from the front of the list in `place'."
  (w/uniq g
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ binds (list g val))
         (do1 (car ,g)
              (,setter (cdr ,g)))))))

(def adjoin (x xs (o test iso))
  " Returns a list with `x' in front of `xs', unless `test' returns true
    for some element of `xs' when matched with `x'. "
  (if (some [test x _] xs)
      xs
      (cons x xs)))

(mac pushnew (x place . args)
  " Pushes `x' into the front of the list in `place' unless it is
    already in that list. "
  (w/uniq gx
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ (list gx x) binds)
         (,setter (adjoin ,gx ,val ,@args))))))

(mac pull (test place)
  " Pulls all elements that pass `test' from the list in `place'."
  (w/uniq g
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ (list g test) binds)
         (,setter (rem ,g ,val))))))

(mac ++ (place (o i 1))
  " Increments `place' by the given increment `i' (defaults to 1). "
  (if (isa place 'sym)
      `(= ,place (+ ,place ,i))
      (w/uniq gi
        (let (binds val setter) (setforms place)
          `(atwiths ,(+ binds (list gi i))
             (,setter (+ ,val ,gi)))))))

(mac -- (place (o i 1))
  " Decrements `place' by the given decrement `i' (defaults to 1). "
  (if (isa place 'sym)
      `(= ,place (- ,place ,i))
      (w/uniq gi
        (let (binds val setter) (setforms place)
          `(atwiths ,(+ binds (list gi i))
             (,setter (- ,val ,gi)))))))

; E.g. (inc x) equiv to (zap + x 1)

(mac zap (op place . args)
  " Modifies `place' with the result of `op' on that `place'. "
  (with (gop    (uniq)
         gargs  (map [uniq] args)
         mix    (afn seqs
                  (if (some no seqs)
                      nil
                      (+ (map car seqs)
                         (apply self (map cdr seqs))))))
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ binds (list gop op) (mix gargs args))
         (,setter (,gop ,val ,@gargs))))))

; Can't simply mod pr to print strings represented as lists of chars,
; because empty string will get printed as nil.  Would need to rep strings
; as lists of chars annotated with 'string, and modify car and cdr to get
; the rep of these.  That would also require hacking the reader.

;(def pr args
;  (if (isa (car args) 'output)
;      (do (error "stream arg!" args)
;          (map1 [disp _ (car args)] (cdr args))
;          (cadr args))
;      (do (map1 disp args)
;          (car args))))

(def pr args
  " Prints the arguments. "
  (map1 disp args)
  (car args))

; Rtm says this version should make the server 20% faster because map1
; generates so much garbage; in fact makes slower; maybe rewrite map1?

;(def newpr args
;  (if (isa (car args) 'output)
;      (do (each a (cdr args) (disp a (car args)))
;          (cadr args))
;      (do (each a args (disp a))
;          (car args))))

(def prn args
  " Prints the arguments followed by a newline. "
  (do1 (apply pr args)
       (writec #\newline
               (if (isa (car args) 'output) (car args) (stdout)))))

(mac nil! args
  " Sets each of the given places to nil. "
  `(do ,@(map (fn (a) `(= ,a nil)) args)))

(mac t! args
  " Sets each of the given places to t. "
  `(do ,@(map (fn (a) `(= ,a t)) args)))

; Destructing means ambiguity: are pat vars bound in else? (no)

(mac iflet (var expr then . rest)
  " Checks if `expr' is true, and if so, assigns it to `var' and
    performs the `then' clause. "
  (w/uniq gv
    `(let ,gv ,expr
       (if ,gv (let ,var ,gv ,then) ,@rest))))

(mac whenlet (var expr . body)
  " Checks if `expr' is true, and if so, assigns it to `var' and
    performs the `body'."
  `(iflet ,var ,expr (do ,@body)))

(mac aif (expr . body)
  " Similar to `if' but assigns the result of 'expr' to the variable `it'."
  (if (<= (len body) 2)
    `(let it ,expr (if it ,@body))
    `(let it ,expr (if it ,(car body) (aif ,@(cdr body))))))

(mac awhen (expr . body)
  " Similar to `when' but assigns the result of 'expr' to the variable `it'."
  `(let it ,expr (if it (do ,@body))))

(mac aand args
  " Similar to `and' but assigns the previous expression to the variable `it'."
  (if (no args)
      't
      (no (cdr args))
       (car args)
      `(let it ,(car args) (and it (aand ,@(cdr args))))))

(mac accum (accfn . body)
  " Accumulates the values given to all calls to `accfn' within `body' and
    returns a list of those values. "
  (w/uniq gacc
    `(withs (,gacc nil ,accfn [push _ ,gacc])
       ,@body
       ,gacc)))

; Repeatedly evaluates its body till it returns nil, then returns vals.

(mac drain (expr (o eof nil))
  " Repeatedly evaluates `expr' until it returns nil, then returns a list
    of the true values. "
  (w/uniq (gacc gdone gres)
    `(with (,gacc nil ,gdone nil)
       (while (no ,gdone)
         (let ,gres ,expr
           (if (is ,gres ,eof)
               (= ,gdone t)
               (push ,gres ,gacc))))
       (rev ,gacc))))

; For the common C idiom while (x = snarfdata) != stopval.
; Rename this if use it often.

(mac whiler (var expr endval . body)
  " Performs `body' while `expr' is not `endval', assigning the result of
    `expr' to `var'."
  (w/uniq (gf ge)
    `(let ,ge ,endval
        ((rfn ,gf (,var)
          (when (and ,var (no (is ,var ,ge)))
            ,@body
            (,gf ,expr)))
         ,expr))))

;(def macex (e)
;  (if (atom e)
;      e
;      (let op (and (atom (car e)) (eval (car e)))
;        (if (isa op 'mac)
;            (apply (rep op) (cdr e))
;            e))))

(def consif (x y)
  " Adds `x' to the front of the list `y' if `x' is true. "
  (if x (cons x y) y))

(def string args
  " Creates a string from its arguments "
  (apply + "" (map [coerce _ 'string] args)))

(def flat (x (o stringstoo))
  " Flattens a nested list. "
  ((rfn f (x acc)
     (if (or (no x) (and stringstoo (is x "")))
          acc
         (and (atom x) (no (and stringstoo (isa x 'string))))
          (cons x acc)
         (f (car x) (f (cdr x) acc))))
   x nil))

; Perhaps not the final idea, or at least final name

(mac default (x test alt)
  " Returns `x' if it passes `test', otherwise returns `alt'. "
  (w/uniq gx
    `(let ,gx ,x
       (if (,test ,gx) ,gx ,alt))))

(def pos (test seq (o start 0))
  " Returns the position of the first element in `seq' that passes `test'. "
  (let f (testify test)
    (if (alist seq)
        ((afn (seq n)
           (if (no seq)
                nil
               (f (car seq))
                n
               (self (cdr seq) (+ n 1))))
         (nthcdr start seq)
         start)
        (recstring [if (f (seq _)) _] seq start))))

(def even (n) " Determines if a number is even. " (is (mod n 2) 0))

(def odd (n) " Determines if a number is odd. " (no (even n)))

(mac after (x . ys)
  " Ensures that the body is performed after the expression `x',
    even if it fails."
  `(protect (fn () ,x) (fn () ,@ys)))

(let expander
     (fn (f var name body)
       `(let ,var (,f ,name)
          (after (do ,@body) (close ,var))))

  (mac w/infile (var name . body)
    " Opens the given file `name' for input, assigning the stream to `var'.
      The stream is automatically closed on exit from the `body'. "
    (expander 'infile var name body))

  (mac w/outfile (var name . body)
    " Opens the given file `name' for output, assigning the stream to `var'.
      The stream is automatically closed on exit from the `body'. "
    (expander 'outfile var name body))

  (mac w/instring (var str . body)
    " Opens the given string `str' for input, assigning the stream to `var'.
      The stream is automatically closed on exit from the `body'. "
    (expander 'instring var str body))
  )

(mac w/outstring (var . body)
  " Opens a string for output, assigning the stream to `var'.
    The stream is automatically closed on exit from the `body'. "
  `(let ,var (outstring) ,@body))

(mac w/appendfile (var name . body)
  " Opens a file `name' for append, assigning the stream to `var'.
    The stream is automatically closed on exit from the `body'. "
  `(let ,var (outfile ,name 'append)
     (after (do ,@body) (close ,var))))

; rename this simply "to"?  - prob not; rarely use

(mac w/stdout (str . body)
  " Opens the string `str' for output; normal printed output from `body'
    is redirected to the string. "
  `(call-w/stdout ,str (fn () ,@body)))

(mac w/stdin (str . body)
  " Opens the string `str' for input; normal read input from `body'
    is redirected from the string. "
  `(call-w/stdin ,str (fn () ,@body)))

(mac tostring body
  " Returns the printed output from `body' as a string. "
  (w/uniq gv
   `(w/outstring ,gv
      (w/stdout ,gv ,@body)
      (inside ,gv))))

(mac fromstring (str . body)
  " Redirects input to `body' from the given string `str'. "
  (w/uniq gv
   `(w/instring ,gv ,str
      (w/stdin ,gv ,@body))))

(def readstring1 (s (o eof nil))
  " Reads a single expression from the string. "
  (w/instring i s (read i eof)))

(def read ((o x (stdin)) (o eof nil))
  " Reads a single expression from a string or stream. "
  (if (isa x 'string) (readstring1 x eof) (sread x eof)))

(def readfile (name)
  " Reads the expressions from the file `name', and returns a list of
    expressions read from the file. "
  (w/infile s name (drain (read s))))

(def readfile1 (name)
  " Reads a single expression from the file `name'. "
  (w/infile s name (read s)))

(def writefile1 (val name)
  " Writes the value to the file `name'. "
  (w/outfile s name (write val s)) val)

(def readall (src (o eof nil))
  " Reads the expressions from the string or stream `src', and returns a
    list of expressions read from the file. "
  ((afn (i)
    (let x (read i eof)
      (if (is x eof)
          nil
          (cons x (self i)))))
   (if (isa src 'string) (instring src) src)))

(def sym (x)
  " Returns the symbol for `x'. "
  (coerce x 'sym))

(mac rand-choice exprs
  " Returns the result of one of the given `exprs', chosen at random. "
  `(case (rand ,(len exprs))
     ,@(let key -1
         (mappend [list (++ key) _]
                  exprs))))

(mac n-of (n expr)
  " Repeats `expr' `n' times, then returns the results in a list. "
  (w/uniq ga
    `(let ,ga nil
       (repeat ,n (push ,expr ,ga))
       (rev ,ga))))

(def rand-string (n)
  " Generates a random string of letters and numbers, starting with a letter. "
  (with (cap (fn () (+ 65 (rand 26)))
         sm  (fn () (+ 97 (rand 26)))
         dig (fn () (+ 48 (rand 10))))
    (coerce (map [coerce _ 'char]
                 (cons (rand-choice (cap) (sm))
                       (n-of (- n 1) (rand-choice (cap) (sm) (dig)))))
            'string)))

(mac forlen (var s . body)
  " Loops across the length of the sequence `s'. "
  `(for ,var 0 (- (len ,s) 1) ,@body))

(mac on (var s . body)
  " Loops across the sequence `s', assigning each element to `var', and providing the current index in `index'.  "
  (if (is var 'index)
      (err "Can't use index as first arg to on.")
      (w/uniq gs
        `(let ,gs ,s
           (forlen index ,gs
             (let ,var (,gs index)
               ,@body))))))

(def best (f seq)
  " Selects the best element of `seq' according to `f'. "
  (if (no seq)
      nil
      (let wins (car seq)
        (each elt (cdr seq)
          (if (f elt wins) (= wins elt)))
        wins)))

(def max args " Returns the highest argument. " (best > args))
(def min args " Returns the lowest argument. " (best < args))

; (mac max2 (x y)
;   (w/uniq (a b)
;     `(with (,a ,x ,b ,y) (if (> ,a ,b) ,a ,b))))

(def most (f seq)
  " Selects the element of `seq' with the highest [f _]. "
  (unless (no seq)
    (withs (wins (car seq) topscore (f wins))
      (each elt (cdr seq)
        (let score (f elt)
          (if (> score topscore) (= wins elt topscore score))))
      wins)))

; Insert so that list remains sorted.  Don't really want to expose
; these but seem to have to because can't include a fn obj in a
; macroexpansion.

(def insert-sorted (test elt seq)
  " Inserts `elt' into a sequence `seq' sorted by `test'. "
  (if (no seq)
       (list elt)
      (test elt (car seq))
       (cons elt seq)
      (cons (car seq) (insert-sorted test elt (cdr seq)))))

(mac insort (test elt seq)
  " Inserts `elt' into a sequence in the place `seq' sorted by `test'. "
  `(zap [insert-sorted ,test ,elt _] ,seq))

(def reinsert-sorted (test elt seq)
  " Inserts `elt' into a sequence `seq', partially sorted by `test'. "
  (if (no seq)
       (list elt)
      (is elt (car seq))
       (reinsert-sorted test elt (cdr seq))
      (test elt (car seq))
       (cons elt (rem elt seq))
      (cons (car seq) (reinsert-sorted test elt (cdr seq)))))

(mac insortnew (test elt seq)
  " Inserts `elt' into a sequence in the place `seq', partially sorted
    by `test'. "
  `(zap [reinsert-sorted ,test ,elt _] ,seq))

; Could make this look at the sig of f and return a fn that took the
; right no of args and didn't have to call apply (or list if 1 arg).

(def memo (f)
  " Creates a function that will store results of calls to the given
    source function.
    For each set of arguments, the source function will only be called
    once; if the memo'ed function is called again with the same arguments,
    it will return the stored result instead of calling the source function. "
  (let cache (table)
    (fn args
      (or (cache args)
          (= (cache args) (apply f args))))))

(mac defmemo (name parms . body)
  " Defines a function that automatically stores the results of calls.
    For each set of arguments, this function will only execute once.
    If the function is called again with the same arguments, it will
    immediately return the stored result for that set of arguments. "
  `(safeset ,name (memo (fn ,parms ,@body))))

(def <= args
  " Determines if each argument is less than or equal to succeeding
    arguments. "
  (or (no args)
      (no (cdr args))
      (and (no (> (car args) (cadr args)))
           (apply <= (cdr args)))))

(def >= args
  " Determines if each argument is greater than or equal to succeeding
    arguments. "
  (or (no args)
      (no (cdr args))
      (and (no (< (car args) (cadr args)))
           (apply >= (cdr args)))))

(def whitec (c)
  " Determines if the given `c' is a whitespace character. "
  (in c #\space #\newline #\tab #\return))

(def nonwhite (c)
  " Determines if the given `c' is not a whitespace character. "
  (no (whitec c)))

(def alphadig (c)
  " Determines if the given `c' is an alphanumeric character. "
  (or (<= #\a c #\z) (<= #\A c #\Z) (<= #\0 c #\9)))

(def punc (c)
  " Determines if the given `c' is punctuation character. "
  (in c #\. #\, #\; #\: #\! #\?))

(def readline ((o str (stdin)))
  " Reads a string terminated by a newline from the stream `str'. "
  (awhen (readc str)
    (tostring
      (writec it)
      (whiler c (readc str) #\newline
        (writec c)))))

; Don't currently use this but suspect some code could.

(mac summing (sumfn . body)
  " Counts the number of times `sumfn' is called with a true value
    within `body'. "
  (w/uniq (gc gt)
    `(let ,gc 0
       (let ,sumfn (fn (,gt) (if ,gt (++ ,gc)))
         ,@body)
       ,gc)))

(def trav (f base tree)
  " Traverses a list as a binary tree. "
  (if (atom tree)
      (base tree)
      (f (trav f base (car tree)) (trav f base (cdr tree)))))

(def carif (x)
  " Returns the first element of a list if the argument is a list. "
  (if (atom x) x (car x)))

; Could prob be generalized beyond printing.

(def prall (elts (o init "") (o sep ", "))
  (when elts
    (pr init (car elts))
    (map [pr sep _] (cdr elts))
    elts))

(def prs args
  (prall args "" #\space))

(def tree-subst (old new tree)
  (if (is tree old)
       new
      (atom tree)
       tree
      (cons (tree-subst old new (car tree))
            (tree-subst old new (cdr tree)))))

(def ontree (f tree)
  (f tree)
  (unless (atom tree)
    (ontree f (car tree))
    (ontree f (cdr tree))))

(def fill-table (table data)
  (each (k v) (pair data) (= (table k) v))
  table)

(mac obj args
  (w/uniq g
    `(let ,g (table)
       ,@(map (fn ((k v)) `(= (,g ',k) ,v))
              (pair args))
       ,g)))

(def keys (h)
  (accum a (ontable k v h (a k))))

(def vals (h)
  (accum a (ontable k v h (a v))))

; These two should really be done by coerce.  Wrap coerce?

(def tablist (h)
  (accum a (maptable (fn args (a args)) h)))

(def listtab (al)
  (let h (table)
    (map (fn ((k v)) (= (h k) v))
         al)
    h))

(def load-table (file (o eof))
  (w/infile i file (read-table i eof)))

(def read-table ((o i (stdin)) (o eof))
  (let e (read i eof)
    (if (alist e) (listtab e) e)))

(def load-tables (file)
  (w/infile i file
    (w/uniq eof
      (drain (read-table i eof) eof))))

(def save-table (h file)
  (w/outfile o file (write-table h o)))

(def write-table (h (o o (stdout)))
  (write (tablist h) o))

(def copy (x)
  (case (type x)
    sym    x
    cons   (apply (fn args args) x)
    string (let new (newstring (len x))
             (forlen i x
               (= (new i) (x i)))
             new)
    table  (let new (table)
             (ontable k v x
               (= (new k) v))
             new)
           (err "Can't copy " x)))

(def abs (n)
  (if (< n 0) (- n) n))

(def signop (n)
  (if (< n 0) - +))

; The problem with returning a list instead of multiple values is that
; you can't act as if the fn didn't return multiple vals in cases where
; you only want the first.  Not a big problem.

(def round (n)
  (withs (base (truncate n) rem (abs (- n base)))
    (if (> rem 1/2) ((if (> n 0) + -) base 1)
        (< rem 1/2) base
        (odd base)  ((if (> n 0) + -) base 1)
                    base)))

(def roundup (n)
  (withs (base (truncate n) rem (abs (- n base)))
    (if (>= rem 1/2)
        ((if (> n 0) + -) base 1)
        base)))

(def to-nearest (n quantum)
  (* (roundup (/ n quantum)) quantum))

(def avg (ns) (/ (apply + ns) (len ns)))

; Use mergesort on assumption that mostly sorting mostly sorted lists
; benchmark: (let td (n-of 10000 (rand 100)) (time (sort < td)) 1)

(def sort (test seq)
  (if (alist seq)
      (mergesort test (copy seq))
      (coerce (mergesort test (coerce seq 'cons)) (type seq))))

; Destructive stable merge-sort, adapted from slib and improved
; by Eli Barzilay for MzLib; re-written in Arc.

(def mergesort (less? lst)
  (with (n (len lst))
    (if (<= n 1) lst
        ; ; check if the list is already sorted
        ; ; (which can be a common case, eg, directory lists).
        ; (let loop ([last (car lst)] [next (cdr lst)])
        ;   (or (null? next)
        ;       (and (not (less? (car next) last))
        ;            (loop (car next) (cdr next)))))
        ; lst
        ((afn (n)
           (if (> n 2)
                ; needs to evaluate L->R
                (withs (j (/ (if (even n) n (- n 1)) 2) ; faster than round
                        a (self j)
                        b (self (- n j)))
                  (merge less? a b))
               ; the following case just inlines the length 2 case,
               ; it can be removed (and use the above case for n>1)
               ; and the code still works, except a little slower
               (is n 2)
                (with (x (car lst) y (cadr lst) p lst)
                  (= lst (cddr lst))
                  (when (less? y x) (scar p y) (scar (cdr p) x))
                  (scdr (cdr p) nil)
                  p)
               (is n 1)
                (with (p lst)
                  (= lst (cdr lst))
                  (scdr p nil)
                  p)
               nil))
         n))))

; Also by Eli.

(def merge (less? x y)
  (if (no x) y
      (no y) x
      (let lup nil
        (set lup
             (fn (r x y r-x?) ; r-x? for optimization -- is r connected to x?
               (if (less? (car y) (car x))
                 (do (if r-x? (scdr r y))
                     (if (cdr y) (lup y x (cdr y) nil) (scdr y x)))
                 ; (car x) <= (car y)
                 (do (if (no r-x?) (scdr r x))
                     (if (cdr x) (lup x (cdr x) y t) (scdr x y))))))
        (if (less? (car y) (car x))
          (do (if (cdr y) (lup y x (cdr y) nil) (scdr y x))
              y)
          ; (car x) <= (car y)
          (do (if (cdr x) (lup x (cdr x) y t) (scdr x y))
              x)))))

(def bestn (n f seq)
  (firstn n (sort f seq)))

(def split (seq pos)
  (withs (mid (nthcdr (- pos 1) seq)
          s2  (cdr mid))
    (nil! (cdr mid))
    (list seq s2)))

(mac time (expr)
  (w/uniq (t1 t2)
    `(let ,t1 (msec)
       (do1 ,expr
            (let ,t2 (msec)
              (prn "time: " (- ,t2 ,t1) " msec."))))))

(mac jtime (expr)
  `(do1 'ok (time ,expr)))

(mac time10 (expr)
  `(time (repeat 10 ,expr)))

(= templates* (table))

(def maps (fn . args)
  (apply join (apply map fn args)))

(mac deftem (tem . fields)
  (withs (name (carif tem) includes (if (acons tem) (cdr tem)))
    `(= (templates* ',name)
        (+ (maps templates* ',(rev includes))
           (list ,@(map (fn ((k v)) `(list ',k (fn () ,v)))
                        (pair fields)))))))

(def inst (tem . args)
  (let x (table)
    (each (k v) (templates* tem)
      (unless (no v) (= (x k) (v))))
    (each (k v) (pair args)
      (= (x k) v))
    x))

; To write something to be read by temread, (write (tablist x))

(def temread (tem (o str (stdin)))
  (templatize tem (read str)))

; Converts alist to inst; ugly; maybe should make this part of coerce.
; Note: discards fields not defined by the template.

(def templatize (tem raw)
  (with (x (inst tem) fields (templates* tem))
    (each (k v) raw
      (when (assoc k fields)
        (= (x k) v)))
    x))

(def temload (tem file)
  (w/infile i file (temread tem i)))

(def temloadall (tem file)
  (map (fn (pairs) (templatize tem pairs))
       (w/infile in file (readall in))))


(def number (n) (in (type n) 'int 'num))

(def cache (timef valf)
  (with (cached nil gentime nil)
    (fn ()
      (unless (and cached (< (- (seconds) gentime) (timef)))
        (= cached  (valf)
           gentime (seconds)))
      cached)))

(mac errsafe (expr)
  `(on-err (fn (c) nil)
           (fn () ,expr)))

(def saferead (arg) (errsafe (read arg)))

(def safe-load-table (filename)
  (or (errsafe (load-table filename))
      (table)))

(def ensure-dir (path)
  (unless (dir-exists path)
    (system (string "mkdir -p " path))))

(def uname nil
  (let val (tostring (system "uname"))
  (subseq val 0 (- (len val) 1))))

(def date ((o time (seconds)))
  (let val (tostring (system
                      (string
                       "date -u "
                       (if
                        (is (uname) "Linux")
                        ;; Linux wants -d and an interval
                        (string "-d \"" (- 1 (since time)) " seconds\"")
                        ;; BSD wants -r and epoch seconds
                        (string "-r " time))
                       " \"+%Y-%m-%d\"")))
    (subseq val 0 (- (len val) 1))))

(def since (t1) (- (seconds) t1))

(def count (test x)
  (with (n 0 testf (testify test))
    (each elt x
      (if (testf elt) (++ n)))
    n))

(def ellipsize (str (o limit 80))
  (if (<= (len str) limit)
      str
      (+ (subseq str 0 limit) "...")))

(def random-elt (seq) (seq (rand (len seq))))

(mac until (test . body)
  `(while (no ,test) ,@body))

(def before (x y seq (o i 0))
  (with (xp (pos x seq i) yp (pos y seq i))
    (and xp (or (no yp) (< xp yp)))))

(def orf fns
  (fn (x) (some [_ x] fns)))

(def andf fns
  (fn (x) (all [_ x] fns)))

(def atend (i s)
  (>= i (- (len s) 1)))

(def multiple (x y)
  (is 0 (mod x y)))

(mac nor args `(no (or ,@args)))

; Consider making the default sort fn take compare's two args (when do
; you ever have to sort mere lists of numbers?) and rename current sort
; as prim-sort or something.

; Could simply modify e.g. > so that (> len) returned the same thing
; as (compare > len).

(def compare (comparer scorer)
  (fn (x y) (comparer (scorer x) (scorer y))))

; Cleaner thus, but may only ever need in 2 arg case.

;(def compare (comparer scorer)
;  (fn args (apply comparer map scorer args)))

(def only (f g . args)
  (aif (apply g args) (f it)))

(mac conswhen (f x y)
  (w/uniq (gf gx)
   `(with (,gf ,f ,gx ,x)
      (if (,gf ,gx) (cons ,gx ,y) ,y))))

; Could rename this get, but don't unless it's frequently used.

(def firstn-that (n f xs)
  (if (or (<= n 0) (no xs))
       nil
      (f (car xs))
       (cons (car xs) (firstn-that (- n 1) f (cdr xs)))
       (firstn-that n f (cdr xs))))

(def dedup (xs)
  (with (h (table) acc nil)
    (each x xs
      (unless (h x)
        (push x acc)
        (t! (h x))))
    (rev acc)))

(def single (x) (and (acons x) (no (cdr x))))

(def plural (n str)
  (if (or (is n 1) (single n))
      str
      (string str "s")))

(def intersperse (x ys)
  (cons (car ys)
        (mappend [list x _] (cdr ys))))

(def counts (seq (o c (table)))
  (if (no seq)
      c
      (do (zap [if _ (+ _ 1) 1] (c (car seq)))
          (counts (cdr seq) c))))

(def commonest (seq)
  (with (winner nil n 0)
    (ontable k v (counts seq)
      (when (> v n) (= winner k n v)))
    (list winner n)))

(def splitn (n xs)
  (let acc nil
    ((afn (n xs)
       (if (or (no xs) (<= n 0))
           (list (rev acc) xs)
           (do (push (car xs) acc)
               (self (- n 1) (cdr xs)))))
     n xs)))

; In the normal case, the result of reduce is the combined result of function's
; being applied to successive pairs of elements of sequence. If the sequence
; contains exactly one element and no init is given, then that element is returned
; and function is not called. If the sequence is empty and init is given, then
; init is returned and function is not called. If the sequence is empty and init
; is not given, then the function is called with zero arguments, and reduce returns
; whatever function does. This is the only case where the function is called with
; other than two arguments.

(let initsym (uniq)

  ; Left-associative
  (def reduce (f xs (o init initsym))
    ((afn (xs)
       (if (cdr xs) (self (cons (f (car xs) (cadr xs)) (cddr xs)))
           xs (car xs)
           (f)))
     (if (is init initsym) xs (cons init xs))))

  ; Right-associative
  ; Rather inefficent due to recursive call not being in the tail position.
  (def rreduce (f xs (o init initsym))
    ((afn (xs)
       (if (cdr xs) (f (car xs) (rreduce f (cdr xs)))
           xs (car xs)
           (f)))
     (if (is init initsym) xs (join xs (list init))))))

(let argsym (uniq)

  (def parse-format (str)
    (rev (accum a
           (with (chars nil  i -1)
             (w/instring s str
               (whilet c (readc s)
                 (case c
                   #\# (do (a (coerce (rev chars) 'string))
                           (nil! chars)
                           (a (read s)))
                   #\~ (do (a (coerce (rev chars) 'string))
                           (nil! chars)
                           (readc s)
                           (a (list argsym (++ i))))
                       (push c chars))))
              (when chars
                (a (coerce (rev chars) 'string)))))))

  (mac prf (str . args)
    `(let ,argsym (list ,@args)
       (pr ,@(parse-format str))))
)

(def load (file (o hook))
  (or= hook idfn)
  (w/infile f file
    (whilet e (read f)
      (eval (hook e)))))

(def positive (x)
  (and (number x) (> x 0)))

(mac w/table (var . body)
  `(let ,var (table) ,@body ,var))

(def ero args
  (each a args
    (write a (stderr))
    (writec #\space (stderr))))

(def queue () (list nil nil 0))

; Despite call to atomic, once had some sign this wasn't thread-safe.

(def enq (obj q)
  (atomic
    (++ (q 2))
    (if (no (car q))
        (= (cadr q) (= (car q) (list obj)))
        (= (cdr (cadr q)) (list obj)
           (cadr q)       (cdr (cadr q))))
    (car q)))

(def deq (q)
  (atomic (unless (is (q 2) 0) (-- (q 2)))
          (pop (car q))))

; Should redef len to do this, and make queues lists annotated queue.

(def qlen (q) (q 2))

(def qlist (q) (car q))

(def enq-limit (val q (o limit 1000))
  (atomic
     (unless (< (qlen q) limit)
       (deq q))
     (enq val q)))

(def median (ns)
  ((sort > ns) (truncate (/ (len ns) 2))))

(mac noisy-each (n var val . body)
  (w/uniq (gn gc)
    `(with (,gn ,n ,gc 0)
       (each ,var ,val
         (when (multiple (++ ,gc) ,gn)
           (pr ".")
           ;(flushout)
           )
         ,@body)
       (prn)
       ;(flushout)
       )))

(mac point (name . body)
  (w/uniq g
    `(ccc (fn (,g)
            (let ,name [,g _]
              ,@body)))))

(mac catch body
  `(point throw ,@body))

(def downcase (x)
  (let downc (fn (c)
               (let n (coerce c 'int)
                 (if (or (< 64 n 91) (< 191 n 215) (< 215 n 223))
                     (coerce (+ n 32) 'char)
                     c)))
    (case (type x)
      string (map downc x)
      char   (downc x)
      sym    (sym (map downc (coerce x 'string)))
             (err "Can't downcase" x))))

(def upcase (x)
  (let upc (fn (c)
             (let n (coerce c 'int)
               (if (or (< 96 n 123) (< 223 n 247) (< 247 n 255))
                   (coerce (- n 32) 'char)
                   c)))
    (case (type x)
      string (map upc x)
      char   (upc x)
      sym    (sym (map upc (coerce x 'string)))
             (err "Can't upcase" x))))

(def range (start end (o step 1))
  "Return a range of numbers from `start' to `end', by `step'."
  (if (> (abs start) (abs end)) nil
      (is start end) (list end)
      (cons start
            (range ((signop (- end start)) start step) end step))))

(def mismatch (s1 s2)
  (catch
    (on c s1
      (when (isnt c (s2 index))
        (throw index)))))

(def memtable (ks)
  (let h (table)
    (each k ks (t! (h k)))
    h))

(= bar* " | ")

(mac w/bars body
  (w/uniq (out needbars)
    `(let ,needbars nil
       (do ,@(map (fn (e)
                    `(let ,out (tostring ,e)
                       (unless (is ,out "")
                         (if ,needbars
                             (pr bar* ,out)
                             (do (t! ,needbars)
                                 (pr ,out))))))
                  body)))))

(mac redef (name parms . body)
  `(do (tostring
        (let old ,name
          (def ,name ,parms ,@body)))
       ,name))

(mac $ body
   (list 'seval (cons 'quasiquote body)))

(mac help (name)
   `(do (pr ,(helpstr name))
        nil))

(def helpstr (name (o verbose t))
  (tostring
   (let h (*help* name)
     (if (no h)
         (if verbose (prn name " is not documented."))
         (with (kind  (car h)
                doc   (cadr h))
           (pr "[" kind "]" (if (is kind 'mac) " " "  "))
           (prn (if (sig name)
                    (cons name (sig name))))
           (and verbose doc (prn doc)))))))

(def fns (pfx (o test [prefix pfx _]))
  "Print sigs for macros & functions starting with pfx, or that pass test if given."
  (each f (sort < (keep test (map [string _] (keys sig))))
        (pr (helpstr (sym f) nil))))

(= env ($ getenv))

(defset env (x)
  (w/uniq g
    (list (list g x)
          `(env ,g)
          `(fn (val) (($ putenv) ,g val)))))

(mac % () nil)
(mac %% () nil)
(mac %%% () nil)

(def input-history-update (expr)
  (let expandedexpr (expand expr)
    (= %%% %%
       %% %)
    (tostring (mac % () expandedexpr))))

(= ^ nil
   ^^ nil
   ^^^ nil)

(def output-history-update (val)
  (= ^^^ ^^
     ^^ ^
     ^ val))


; Lower priority ideas

; solution to the "problem" of improper lists: allow any atom as a list
;  terminator, not just nil.  means list recursion should terminate on
;  atom rather than nil, (def empty (x) (or (atom x) (is x "")))
; table should be able to take an optional initial-value.  handle in sref.
; warn about code of form (if (= )) -- probably mean is
; warn when a fn has a parm that's already defined as a macro.
;   (def foo (after) (after))
; idea: a fn (nothing) that returns a special gensym which is ignored
;  by map, so can use map in cases when don't want all the vals
; idea: anaph macro so instead of (aand x y) say (anaph and x y)
; idea: foo.bar!baz as an abbrev for (foo bar 'baz)
; could uniq be (def uniq () (annotate 'symbol (list 'u))) again?
; idea: use x- for (car x) and -x for (cdr x)  (but what about math -?)
; idea: get rid of strings and just use symbols
; could a string be (#\a #\b . "") ?
; better err msg when , outside of a bq
; idea: parameter (p foo) means in body foo is (pair arg)
; idea: make ('string x) equiv to (coerce x 'string) ?  or isa?
;   quoted atoms in car valuable unused semantic space

