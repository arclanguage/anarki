; Main Arc lib.  Ported to Scheme version Jul 06.

; optimize ~foo in functional position in ac, like compose
; make foo~bar equiv of foo:~bar (in expand-ssyntax)
; rename assert
; (10 x) for (= x 10)?
; should (= x)  mean (= x t)?
; add sigs of ops defined in ac.scm
; get hold of error types within arc
; why is macex defined in scheme instead of using def below?
; write disp, read, write in arc
; could prob write rmfile and dir in terms of system
; could I get all of macros up into arc.arc?
; warn when shadow a global name
; permanent objs that live on disk and are updated when modified
; way to spec default 0 rather than nil for hts
;  do in access call or when ht created?  simply have ++ nil -> 1?
; some simple regexp/parsing plan

; compromises in this implementation: 
; no objs in code
;  (mac testlit args (listtab args)) breaks when called
; separate string type
;  (= (cdr (cdr str)) "foo") couldn't work because no way to get str tail

(set help* (table))
(set call* (table))
(set source* (table))

(sref call* ref 'cons)
(sref call* ref 'string)
(sref call* ref 'table)
(sref call* ref 'vec)
(sref call* thread-local-ref 'thread-local)
((fn (old)
  (set sref
    (fn (c v . i)
      (if (is (type c) 'thread-local)
          (thread-local-set c v)
          (apply old c v i)))))
 sref)

(set current-load-file* "arc.arc")
(set source-file* (table))

(set do (annotate 'mac
          (fn args `((fn () ,@args)))))
;documentation for do itself
(sref help*
  '(mac
  " Evaluates each expression in sequence and returns the result of the
    last expression.
    See also [[do1]] [[after]] ")
  'do)
(sref sig
  'args
  'do)
(sref source-file*
  current-load-file*
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
                       (sref help* '(fn ,(car body)) ',name)
                       (sref help* '(fn nil) ',name))
                   (sref source-file* current-load-file* ',name)
		   (sref source* '(def ,name ,parms ,@body) ',name)
                   (safeset ,name (fn ,parms ,@body))))))

;documentation for def itself
(sref help*
  '(mac
  " Defines a function with the given `name', `parms', and `body'.
    See also [[fn]] [[mac]] ")
  'def)
(sref sig
  '(name parms . body)
  'def)
(sref source-file*
  current-load-file*
  'def)

(def caar (xs) " Equivalent to (car (car xs)) " (car (car xs)))
(def cadr (xs) " Equivalent to (car (cdr xs)) " (car (cdr xs)))
(def cddr (xs) " Equivalent to (cdr (cdr xs)) " (cdr (cdr xs)))

(def no (x) " Determines if `x' is `nil'. " (is x nil))

(def acons (x)
  " Determines if `x' is a `cons' cell or list.
    Unlike 'alist, this function will return nil if given an empty list
    See also [[atom]] [[alist]] [[dotted]] [[isa]] [[cons]] [[list]] "
  (is (type x) 'cons))

(def atom (x)
  " Determines if `x' is atomic.
    See also [[acons]] [[isa]] "
  (no (acons x)))

(def list args
  " Creates a list from the given parameters.
    See also [[cons]] [[acons]] "
  args)

(def idfn (x)
  " Identity function - just returns its argument. "
  x)

(def nilfn args
  " Takes any number of arguments and returns nil. "
  nil)

; Maybe later make this internal.  Useful to let xs be a fn?

(def map1 (f xs)
  " Return a sequence with function f applied to every element in sequence xs.
    See also [[map]] [[each]] [[mappend]] [[andmap]] [[ormap]] "
  (if (no xs) 
      nil
      (cons (f (car xs)) (map1 f (cdr xs)))))

(def pair (xs (o f list))
  " Applies pairs of elements to the function `f'.
    See also [[tuples]] [[map]] "
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
                      (sref help* '(mac ,(car body)) ',name)
                      (sref help* '(mac nil) ',name))
                  (sref source-file* current-load-file* ',name)
		  (sref source* '(mac ,name ,parms ,@body) ',name)
                  (safeset ,name (annotate 'mac (fn ,parms ,@body)))))))
;documentation for mac itself
(sref help*
  '(mac
  " Defines a macro, a special function which transforms code.
    See also [[def]] ")
  'mac)
(sref sig
  '(name parms . body)
  'mac)
(sref source-file*
  current-load-file*
  'mac)

(mac $ body
   " Allows access to the underlying Scheme. "
   (list 'seval (cons 'quasiquote body)))

(mac and args
  " Evaluates arguments till false is found else returns the last one.
    See also [[or]] [[aand]] [[andf]] [[andmap]] "
  (if args
      (if (cdr args)
          `(if ,(car args) (and ,@(cdr args)))
          (car args))
      't))

(def assoc (key al)
  " Finds a (key value) pair in an associated list.
    See also [[alref]] [[listtab]] [[tablist]] "
  (if (atom al)
       nil
      (and (acons (car al)) (is (caar al) key))
       (car al)
      (assoc key (cdr al))))

(def alref (al key)
  " Get a value from a key in a associated list.
    See also [[assoc]] [[listtab]] [[tablist]] "
  (cadr (assoc key al)))

(mac with (parms . body)
  " Assigns a set of local variables for the given `body'.
    Assignment is simultaneous.
    See also [[withs]] [[given]] [[let]] [[fn]] [[do]] "
  `((fn ,(map1 car (pair parms))
     ,@body)
    ,@(map1 cadr (pair parms))))

(mac let (var val . body)
  " Assigns a local variable for the given `body'.
    See also [[with]] [[withs]] [[fn]] [[do]] "
  `(with (,var ,val) ,@body))

(mac withs (parms . body)
  " Assigns local variables for the given `body'.
    The assignments are made in the given order.
    See also [[with]] [[givens]] [[let]] [[fn]] [[do]] "
  (if (no parms) 
      `(do ,@body)
      `(let ,(car parms) ,(cadr parms) 
         (withs ,(cddr parms) ,@body))))

(def butlast (seq)
  " Returns every element of `seq' but the last one.
    See also [[last]] [[cut]] "
  (cut seq 0 -1))

(mac given body
  " Simultaneously assigns the given (unparenthesized) local variables in the
    one-statement body.
    See also [[let]] [[with]] [[givens]]"
  (with (args (butlast body)
         expr (last body))
    `(with ,args
       ,expr)))

(mac givens body
  " Sequentially assigns the given (unparenthesized) local variables in the
    one-statement body.
    See also [[let]] [[withs]] [[given]]"
  (with (args (butlast body)
         expr (last body))
    `(withs ,args
       ,expr)))

; Rtm prefers to overload + to do this

(def join args
  " Joins all list arguments together.
    See also [[cons]] [[+]] "
  (if (no args)
      nil
      (let a (car args) 
        (if (no a) 
            (apply join (cdr args))
            (cons (car a) (apply join (cdr a) (cdr args)))))))

(mac rfn (name parms . body)
  " Creates a function which calls itself as `name'.
    See also [[fn]] [[afn]] "
  `(let ,name nil
     (set ,name (fn ,parms ,@body))))

(mac afn (parms . body)
  " Creates a function which calls itself with the name `self'.
    See also [[fn]] [[rfn]] [[aif]] [[awhen]] [[aand]] "
  `(rfn self ,parms ,@body))

(mac compose args
  " Arc expands x:y:z into (compose x y z)
    quick way to write (x(y(z)))
    Only used when the call to compose doesn't occur in functional position.
    Composes in functional position are transformed away by ac.
    See also [[complement]] "
  (let g (uniq)
    `(fn ,g
       ,((afn (fs)
           (if (cdr fs)
               (list (car fs) (self (cdr fs)))
               `(apply ,(if (car fs) (car fs) 'idfn) ,g)))
         args))))

(mac complement (f)
  " Arc expands ~x into (complement x)
    whenever the function returns true this returns false.
    See also [[no]] [[isnt]] [[compose]]"
  (let g (uniq)
    `(fn ,g (no (apply ,f ,g)))))

(def rev (xs) 
  " Reverses a copy of the list `xs'
    See also [[copy]] "
  ((afn (xs acc)
     (if (no xs)
         acc
         (self (cdr xs) (cons (car xs) acc))))
   xs nil))

(def isnt (x y)
  " Inverse of is.
    See also [[no]] [[is]] "
  (no (is x y)))

(mac w/uniq (names . body)
  " Assigns a set of variables to unique symbols.
    Generally used for macro functions.
    See also [[uniq]] "
  (if (acons names)
      `(with ,(apply + nil (map1 (fn (n) (list n '(uniq)))
                             names))
         ,@body)
      `(let ,names (uniq) ,@body)))

(mac or args
  " Computes arguments until one of them is true and returns that result.
    See also [[and]] [[orf]] [[ormap]] [[check]] "
  (and args
       (w/uniq g
         `(let ,g ,(car args)
            (if ,g ,g (or ,@(cdr args)))))))

(def alist (x)
  " Return true if argument is a possibly empty list
    Unlike 'acons, this function returns t when given an empty list
    See also [[atom]] [[acons]] [[dotted]] [[isa]] [[cons]] [[list]] "
  (or (no x) (is (type x) 'cons)))

(mac or= (var val)
  " Performs (or var val) and assigns the result to var.
    See also [[or]] [[=]] "
  `(= ,var (or ,var ,val)))

(mac in (x . choices)
  " Returns true if the first argument is one of the other arguments.
    See also [[some]] [[mem]] "
  (w/uniq g
    `(let ,g ,x
       (or ,@(map1 (fn (c) `(is ,g ,c)) choices)))))

; should take n args

(def iso (x y)
  " Isomorphic compare - compares structure (can be slow).
    See also [[is]] "
  (or (is x y)
      (and (acons x) 
           (acons y) 
           (iso (car x) (car y)) 
           (iso (cdr x) (cdr y)))))

(mac when (test . body)
  " When `test' is true, do `body'.
    See also [[unless]] [[if]] [[awhen]] "
  `(if ,test (do ,@body)))

(mac unless (test . body)
  " When `test' is not true, do `body'.
    See also [[when]] [[if]] [[no]] "
  `(if (no ,test) (do ,@body)))

(mac breakable (expr)
  " Allows a (break ...) form to return a value from within the
    body of a control structure.
    Example:
    (breakable:while t
      (aif (something) (break it)))
    See also [[catch]] [[point]] [[accum]] [[while]] "
  `(point break ,expr))

(mac while (test . body)
  " While `test' is true, perform `body' in a loop.
    See also [[until]] [[loop]] [[whilet]] [[whiler]] [[for]]
    [[repeat]] [[drain]] "
  (w/uniq (gf gp)
    `((rfn ,gf (,gp)
        (when ,gp ,@body (,gf ,test)))
      ,test)))

(def empty (seq) 
  " Test to see if `seq' is an empty list or other sequence.
    See also [[no]] [[acons]] [[len]] "
  (or (no seq) 
      (and (no (acons seq)) (is (len seq) 0))))

(def reclist (f xs)
  " Applies the function `f' on the sublists of `xs' until `f' returns true.
    See also [[ormap]] [[andmap]] [[map]] "
  (and xs (or (f xs) (reclist f (cdr xs)))))

(def recstring (test s (o start 0))
  " Applies the function `test' on indices of `s' until `test' returns true.
    See also [[map]] [[reclist]] "
  (let n (len s)
    ((afn (i)
       (and (< i (len s))
            (or (test i)
                (self (+ i 1)))))
     start)))

(def isa (x y)
  " Checks if x is of type y.
    See also [[acons]] [[alist]] [[atom]] "
  (is (type x) y))

(def testify (x)
  " Creates a test that determines if a given argument is `x'.
    See also [[is]] "
  (if (isa x 'fn) x (fn (_) (is _ x))))

(def some (test seq)
  " Determines if at least one element of `seq' satisfies `test'.
    See also [[all]] [[mem]] [[in]] [[pos]] "
  (let f (testify test)
    (if (alist seq)
        (reclist f:car seq)
        (recstring f:seq seq))))

(def all (test seq) 
  " Determines if all elements of `seq' satisfy `test'.
    See also [[some]] "
  (~some (complement (testify test)) seq))
       
(def dotted (x)
  " Determines if `x' is a dotted cons pair.
    See also [[acons]] [[alist]] "
  (if (atom x)
      nil
      (and (cdr x) (or (atom (cdr x))
                       (dotted (cdr x))))))

; I couldn't find a pre-existing total macro-expander
(def expand (expr)
  " Completely expands all macros in `expr'.
    See also [[macex]] [[mac]] "
  (if (and (acons expr) (~dotted expr) (~is 'quote (car expr)))
      (let expansion (macex (cons (expand (car expr))
                                  (map1 expand (cdr expr))))
        (if (and (acons expansion) (acons:car expansion))
          (cons (expand:car expansion) (cdr expansion))
          expansion))
      expr))

(def makeproper (lst)
  " Transforms `list' to a proper list if it is a dotted list.
    See also [[dotted]] [[list]] "
  (if (no (acons lst))
      lst
      (cons (car lst)
            (if (alist (cdr lst))
              (makeproper (cdr lst))
              (list (cdr lst))))))

(def andmap (pred seq)
  " Applies `pred' to elements of `seq' until an element fails.
    See also [[and]] [[andf]] [[map]] "
  (or
    (no seq)
    (and
      (pred (car seq))
      (andmap pred (cdr seq)))))

(def ormap (pred seq)
  " Applies `pred' to elements of `seq' until an element passes.
    See also [[or]] [[orf]] [[map]] "
  (and
    seq
    (or
      (pred (car seq))
      (ormap pred (cdr seq)))))

; The call* table defines how to deal with non-functions
; in functional positions.
; Each entry is just a (type fn) pair.
; The fn should take as its first argument the object itself;
; the rest are the arguments to the object.

(mac defcall (name parms . body)
  " Defines a function to run when an object of the given type
    is encountered in functional position.
    The first argument to this function is the `rep' of the object,
    and the rest are passed as arguments to the object.
    See also [[rep]] [[annotate]] [[type]] "
  `(sref call* (fn ,parms ,@body) ',name))

(defcall num (num . args)
  (if (acons args)
      (if (or (isa (car args) 'num) (isa (car args) 'int))
        (err:tostring:prn "Number applied to number - " num " - parameters - " args)
        (apply (car args) num (cdr args)))
      num))

(def *mbf-arglist-vars (arglist)
  " Returns the variables bound in an argument list.
    See also [[make-br-fn]] "
  (if (isa arglist 'cons)
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

(def *mbf-arglist-frees (arglist)
  " Returns the free variables used in default values for optional arguments
    of an argument list.
    See also [[make-br-fn]] "
  (if (isa arglist 'cons)
    (apply join
      (map1
        (fn (_)
          (and (isa _ 'cons)
               (is (car _) 'o)
               (*mbf-all-vars (cddr _))))
        (makeproper arglist)))
    nil))

(def *mbf-all-vars (form)
  " Extracts all the variables in the fully macro-expanded s-expression `form'.
    See also [[make-br-fn]] "
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
                 (apply join (map1 *mbf-all-vars (cdr _)))))
          (cdr form))
      (is head 'if)
        (apply join (map1 *mbf-all-vars (cdr form)))
      (is head 'fn)
        (join (apply join (map1 *mbf-all-vars (*mbf-arglist-vars  (cadr form))))
              (apply join (map1 *mbf-all-vars (*mbf-arglist-frees (cadr form))))
              (apply join (map1 *mbf-all-vars                     (cddr form))))
      ; else (including set)
        (apply join (map1 *mbf-all-vars form)))))

(def *mbf-free? (form var)
  " Checks if the variable named `var' occurs free (unbound) in `form'.
    See also [[make-br-fn]] "
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
              (or (find var (*mbf-arglist-frees (cadr form)))
                  (and (no (find var (*mbf-arglist-vars (cadr form))))
                       (*mbf-free? (cddr form) var)))
            (is head 'quote)
              #f
            (is head 'quasiquote)
              (ormap
                (fn (_)
                  (and (isa _ 'cons)
                       (in (car _) 'unquote 'unquote-splicing)
                       (*mbf-free? (cdr _) var)))
                form)
            ; else
              (ormap (fn (_) (*mbf-free? _ var)) form)))
    ; else
      nil)))

(mac make-br-fn (body)
  " Constructs an anonymous procedure with the body `body'; the procedure will
    have bound variables of the form _1 ... _N, where N is the largest of those
    variables used in the function.  __ will be a rest parameter, and _ is an
    alias for _1.  Each variable is declared iff it is used.  This is used to
    implement the [] anonymous functions. "
  (with (max 0 arbno nil)
    (map1 ; Just for the side-effect; used as "max"
      (fn (_)
        (withs (s (coerce _ 'string) first (s 0) rest (coerce (cdr (coerce s 'cons)) 'string))
          (and (is (s 0) #\_)
               (or (is rest "")
                   (is rest "_")
                   (and (some (fn (c) (isnt c #\0)) rest)
                        (all  (fn (c) (in c #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)) rest)))
               (*mbf-free? body _)
               (let num (or (and (is rest "") 1) (and (is rest "_") (do (set arbno t) -1)) (coerce rest 'int))
                 (when (> num max) (set max num)))
               nil)))
      (*mbf-all-vars (expand body)))
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
  " Returns the sublist of `seq' whose first element satisfies `test'.
    See also [[find]] [[some]] [[in]]"
  (let f (testify test)
    (reclist [if (f:car _) _] seq)))

(def find (test seq)
  " Returns the first element that matches the test function.
    See also [[mem]] [[some]] [[in]] "
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
    Returns a sequence containing the results of the function.
    See also [[each]] [[mapeach]] [[map1]] [[mappend]] [[andmap]]
    [[ormap]] [[reduce]] "
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
  " Applies the elements of the sequences to the given function.
    Returns a sequence containing the concatenation of the results
    of the function.
    See also [[map]] [[join]] "
  (apply + nil (apply map f args)))

(def firstn (n xs)
  " Returns the first `n' elements of the given list `xs'.
    See also [[cut]] [[nthcdr]] "
  (if (no n)            xs
      (and (> n 0) xs)  (cons (car xs) (firstn (- n 1) (cdr xs)))
                        nil))

(def nthcdr (n xs)
  " Returns the sublist of `xs' starting on the `n'th element.
    `n' is 0-based.
    See also [[cut]] [[firstn]] "
  (if (no n)  xs
      (> n 0) (nthcdr (- n 1) (cdr xs))
              xs))

; Generalization of pair: (tuples x) = (pair x)

(def tuples (xs (o n 2))
  " Returns a list of lists of the elements of `xs', grouped by `n'.
    See also [[pair]] "
  (if (no xs)
      nil
      (cons (firstn n xs)
            (tuples (nthcdr n xs) n))))

(def caris (x val) 
  " Determines if (car x) is `val'.
    See also [[is]] [[car]] [[carif]] "
  (and (acons x) (is (car x) val)))

(def warn (msg . args)
  " Displays a warning message on its arguments.
    See also [[ero]] [[pr]] "
  (disp (+ "Warning: " msg ". "))
  (map [do (write _) (disp " ")] args)
  (disp #\newline))

(mac atomic body
  " Performs `body' atomically, blocking other threads.
    See also [[atlet]] [[atwith]] [[atwiths]] "
  `(atomic-invoke (fn () ,@body)))

(mac atlet args
  " Performs a `let' atomically, blocking other threads.
    See also [[atomic]] [[atwith]] [[atwiths]] "
  `(atomic (let ,@args)))
  
(mac atwith args
  " Performs a `with' atomically, blocking other threads.
    See also [[atomic]] [[atlet]] [[atwiths]] "
  `(atomic (with ,@args)))

(mac atwiths args
  " Performs a `withs' atomically, blocking other threads.
    See also [[atomic]] [[atlet]] [[atwith]] "
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
  " Defines a setter for the named form.
    See also [[=]] "
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
         (if (ssyntax expr)
             (setforms (ssexpand expr))
             (w/uniq (g h)
               (list (list g expr) 
                     g
                     `(fn (,h) (set ,expr ,h)))))
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
  (set x (ssyntax x))
  (and (acons x) (in (car x) 'compose 'complement)))

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
  (if (and (isa place 'sym) (~ssyntax place))
      `(set ,place ,val)
      (let (vars prev setter) (setforms place)
        (w/uniq g
          `(atwith ,(+ vars (list g val))
             (,setter ,g))))))

(def expand=list (terms)
  `(do ,@(map (fn ((p v)) (expand= p v))  ; [apply expand= _]
                  (pair terms))))

(mac = args
  " Assigns values to variables.
    See also [[assert]] [[wipe]] [[++]] [[--]] [[rotate]] [[defset]] "
  (expand=list args))

(mac loop (start test update . body)
  " First performs `start'; while `test' is true, performs `body' then
    `update' in a loop.
    See also [[while]] "
  (w/uniq (gfn gparm)
    `(do ,start
         ((rfn ,gfn (,gparm) 
            (if ,gparm
                (do ,@body ,update (,gfn ,test))))
          ,test))))

(mac for (v init max . body)
  " Loops for the variable `v' from `init' to `max'.
    See also [[repeat]] [[forlen]] "
  (w/uniq (gi gm)
    `(with (,v nil ,gi ,init ,gm (+ ,max 1))
       (loop (set ,v ,gi) (< ,v ,gm) (set ,v (+ ,v 1))
         ,@body))))

(mac repeat (n . body)
  " Repeats the `body' `n' times.
    See also [[for]] [[forlen]] [[n-of]] "
  `(for ,(uniq) 1 ,n ,@body))

; could bind index instead of gensym
;; the above is done by `on'

(mac each (var expr . body)
  " Performs `body' for each element of the sequence returned by `expr',
    with each element assigned to `var'.
    See also [[forlen]] [[on]] [[map]] [[mapeach]] [[ontable]] "
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

(mac mapeach (var expr . body)
  " Performs `body' for each element of the list returned by `expr',
    with each element assigned to `var'; the result of the last expression
    in `body' is stored in a returned list of values.
    See also [[each]] [[map]] "
  `(map1 (fn (,var) ,@body) ,expr))

; (nthcdr x y) = (cut y x).

(def cut (seq start (o end (len seq)))
  " Returns a subsequence of the given `seq'
    If `end' is negative,
    it's a 0-based index into the end of the string.
    For example,

      > (cut \"abcde\" 1, -1)
      \"bcd\"

    See also [[firstn]] [[nthcdr]] [[split]] "
  (with (end (if (< end 0) (+ (len seq) end) end)
         start (if (< start 0) (+ (len seq) start) start))
    (if (isa seq 'string)
        (let s2 (newstring (- end start))
          (for i 0 (- end start 1)
            (= (s2 i) (seq (+ start i))))
          s2)
        (firstn (- end start) (nthcdr start seq)))))

(def at (lst n)
  " Get the `n'th item of lst, *including* negative indicies.
    See also [[cut]] "
  (lst (mod n (len lst))))

(def prefix (pre str)
  " Determines if `pre' is the same as the first part of `str'. "
  (and (<= (len pre) (len str))
       (is pre (cut str 0 (len pre)))))
      
(mac ontable (k v h . body)
  " Loops through the entries of table or object `h', with key-value pairs
    assigned to `k' and `v', respectively.
    See also [[each]] [[keys]] "
  `(maptable (fn (,k ,v) ,@body) ,h))

(mac whilet (var test . body)
  " While `test' is true, perform `body' in a loop.
    The result of `test' is assigned to `var'.
    See also [[while]] [[whiler]] [[drain]] "
  (w/uniq (gf gp)
    `((rfn ,gf (,gp)
        (let ,var ,gp
          (when ,var ,@body (,gf ,test))))
      ,test)))

(def last (xs)
  " Returns the last element of `seq'. "
  (if (cdr xs)
      (last (cdr xs))
      (car xs)))

(def rem (test seq)
  " Returns a list with the elements of `seq' that pass `test' removed.
    See also [[keep]] [[pull]] "
  (let f (testify test)
    (if (alist seq)
        ((afn (s)
           (if (no s)       nil
               (f (car s))  (self (cdr s))
                            (cons (car s) (self (cdr s)))))
          seq)
        (coerce (rem test (coerce seq 'cons)) 'string))))

(def keep (test seq) 
  " Returns a list with the elements of `seq' that pass `test'.
    See also [[rem]] [[pull]] "
  (rem (complement (testify test)) seq))

(def trues (f seq) 
  " Returns a list with all `nil's removed.
    See also [[rem]] [[keep]] "
  (rem nil (map f seq)))

(mac do1 args
  " Performs the body in sequence, then returns the value of the
    first expression.
    See also [[do]] "
  (w/uniq g
    `(let ,g ,(car args)
       ,@(cdr args)
       ,g)))

; Would like to write a faster case based on table generated by a macro,
; but can't insert objects into expansions in Mzscheme.

(mac caselet (var expr . args)
  " Matches the result of `expr' to arguments until one matches.
    The result of `expr' is assigned to `var'.
    See also [[case]] [[if]] [[iflet]] "
  (let ex (afn (args)
            (if (no (cdr args)) 
                (car args)
                `(if (is ,var ',(car args))
                     ,(cadr args)
                     ,(self (cddr args)))))
    `(let ,var ,expr ,(ex args))))

(mac case (expr . args)
  " Matches the result of `expr' to arguments until one matches.
    See also [[caselet]] [[if]] "
  `(caselet ,(uniq) ,expr ,@args))

(mac push (x place)
  " Pushes the value `x' on the front of the list in `place'.
    See also [[pop]] [[cons]] "
  (w/uniq gx
    (let (binds val setter) (setforms place)
      `(let ,gx ,x
         (atwiths ,binds
           (,setter (cons ,gx ,val)))))))

(mac swap (place1 place2)
  " Swaps the values of the specified places.
    See also [[rotate]] [[=]] "
  (w/uniq (g1 g2)
    (with ((binds1 val1 setter1) (setforms place1)
           (binds2 val2 setter2) (setforms place2))
      `(atwiths ,(+ binds1 (list g1 val1) binds2 (list g2 val2))
         (,setter1 ,g2)
         (,setter2 ,g1)))))

(mac rotate places
  " Rotates the values of the specified places, from right to left.
    See also [[swap]] [[=]] "
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
  " Pops a value from the front of the list in `place'.
    See also [[push]] [[car]] "
  (w/uniq g
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ binds (list g val))
         (do1 (car ,g) 
              (,setter (cdr ,g)))))))

(def adjoin (x xs (o test iso))
  " Returns a list with `x' in front of `xs', unless `test' returns true
    for some element of `xs' when matched with `x'.
    See also [[cons]] [[pushnew]] [[consif]] "
  (if (some [test x _] xs)
      xs
      (cons x xs)))

(mac pushnew (x place . args)
  " Pushes `x' into the front of the list in `place' unless it is
    already in that list.
    See also [[push]] [[adjoin]] [[cons]] [[consif]] "
  (w/uniq gx
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ (list gx x) binds)
         (,setter (adjoin ,gx ,val ,@args))))))

(mac pull (test place)
  " Removes all elements that pass `test' from the list in `place'.
    See also [[rem]] [[keep]] "
  (w/uniq g
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ (list g test) binds)
         (,setter (rem ,g ,val))))))

(mac ++ (place (o i 1))
  " Increments `place' by the given increment `i' (defaults to 1).
    See also [[--]] [[zap]] [[=]] "
  (if (isa place 'sym)
      `(= ,place (+ ,place ,i))
      (w/uniq gi
        (let (binds val setter) (setforms place)
          `(atwiths ,(+ binds (list gi i))
             (,setter (+ ,val ,gi)))))))

(mac -- (place (o i 1))
  " Decrements `place' by the given decrement `i' (defaults to 1).
    See also [[++]] [[zap]] [[=]] "
  (if (isa place 'sym)
      `(= ,place (- ,place ,i))
      (w/uniq gi
        (let (binds val setter) (setforms place)
          `(atwiths ,(+ binds (list gi i))
             (,setter (- ,val ,gi)))))))

; E.g. (inc x) equiv to (zap + x 1)

(mac zap (op place . args)
  " Modifies `place' with the result of `op' on that `place'.
    See also [[++]] [[--]] [[pull]] [[push]] [[pop]] [[=]]
    [[wipe]] [[assert]] "
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

(def pr args
  " Prints the arguments.
    See also [[prn]] [[warn]] [[ero]] "
  (map1 disp args)
  (car args))

(def prn args
  " Prints the arguments followed by a newline.
    See also [[pr]] [[warn]] [[ero]] "
  (do1 (apply pr args)
       (writec #\newline)))

(mac wipe args
  " Sets each of the given places to nil.
    See also [[assert]] [[zap]] [[=]] "
  `(do ,@(map (fn (a) `(= ,a nil)) args)))

(mac assert args
  " Sets each of the given places to t.
    See also [[wipe]] [[zap]] [[=]] "
  `(do ,@(map (fn (a) `(= ,a t)) args)))

; Destructuring means ambiguity: are pat vars bound in else? (no)

(mac iflet (var expr then . rest)
  " Checks if `expr' is true, and if so, assigns it to `var' and
    performs the `then' clause.
    See also [[caselet]] [[whenlet]] [[if]] "
  (w/uniq gv
    `(let ,gv ,expr
       (if ,gv (let ,var ,gv ,then) ,@rest))))

(mac whenlet (var expr . body)
  " Checks if `expr' is true, and if so, assigns it to `var' and
    performs the `body'.
    See also [[caselet]] [[iflet]] [[when]] [[if]] "
  `(iflet ,var ,expr (do ,@body)))

(mac aif (expr . body)
  " Similar to `if' but assigns the result of 'expr' to the variable `it'.
    See also [[if]] [[awhen]] [[aand]] [[afn]] "
  `(let it ,expr
     (if it
         ,@(if (cddr body)
               `(,(car body) (aif ,@(cdr body)))
               body))))

(mac awhen (expr . body)
  " Similar to `when' but assigns the result of 'expr' to the variable `it'.
    See also [[when]] [[aif]] [[aand]] [[afn]] "
  `(let it ,expr (if it (do ,@body))))

(mac aand args
  " Similar to `and' but assigns the previous expression to the variable `it'.
    See also [[and]] [[aif]] [[awhen]] [[afn]] "
  (if (no args)
      't 
      (no (cdr args))
       (car args)
      `(let it ,(car args) (and it (aand ,@(cdr args))))))

(mac accum (accfn . body)
  " Collects or accumulates the values given to all calls to `accfn' within
    `body' and returns a list of those values.  Order is not preserved.
    See also [[accums]] [[summing]] "
  (w/uniq gacc
    `(withs (,gacc nil ,accfn [push _ ,gacc])
       ,@body
       ,gacc)))

(mac accums (accfns . body)
  " Collects or accumulates the values given to all calls to functions
    named in `accfns' in body.  Returns a list of lists of those values.
    Order is not preserved.
    See also [[accum]] "
  (let gaccs (map [uniq] accfns)
    `(withs ,(mappend (fn (gacc accfn)
                        (list gacc 'nil accfn `[push _ ,gacc]))
                      gaccs accfns)
       ,@body
       (list ,@(map [list 'rev _] gaccs)))))

; Repeatedly evaluates its body till it returns nil, then returns vals.

(mac drain (expr (o eof nil))
  " Repeatedly evaluates `expr' until it returns nil, then returns a list
    of the true values.
    See also [[while]] [[whiler]] [[whilet]] "
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
    `expr' to `var'.
    See also [[while]] [[whilet]] [[drain]] "
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
  " Adds `x' to the front of the list `y' if `x' is true.
    See also [[cons]] [[if]] [[adjoin]] "
  (if x (cons x y) y))

(def string args
  " Creates a string from its arguments
    See also [[sym]] "
  (apply + "" (map [coerce _ 'string] args)))

(def flat (x (o stringstoo))
  " Flattens a nested list.
    See also [[list]] "
  ((rfn f (x acc)
     (if (or (no x) (and stringstoo (is x "")))
          acc
         (and (atom x) (no (and stringstoo (isa x 'string))))
          (cons x acc)
         (f (car x) (f (cdr x) acc))))
   x nil))

(mac check (x test (o alt))
  " Returns `x' if it passes `test', otherwise returns `alt'.
    See also [[or]] "
  (w/uniq gx
    `(let ,gx ,x
       (if (,test ,gx) ,gx ,alt))))

(def pos (test seq (o start 0))
  " Returns the position of the first element in `seq' that passes `test'.
    See also [[some]] "
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

(def even (n) " Determines if a number is even.  See also [[odd]] " (is (mod n 2) 0))

(def odd (n) " Determines if a number is odd.  See also [[even]] " (no (even n)))

(mac after (x . ys)
  " Ensures that the body is performed after the expression `x',
    even if it fails.
    See also [[do]]"
  `(protect (fn () ,x) (fn () ,@ys)))

(let expander 
     (fn (f var name body)
       `(let ,var (,f ,name)
          (after (do ,@body) (close ,var))))

  (mac w/infile (var name . body)
    " Opens the given file `name' for input, assigning the stream to `var'.
      The stream is automatically closed on exit from the `body'.
      See also [[w/outfile]] [[w/instring]] [[w/stdin]] [[w/socket]] "
    (expander 'infile var name body))

  (mac w/outfile (var name . body)
    " Opens the given file `name' for output, assigning the stream to `var'.
      The stream is automatically closed on exit from the `body'.
      See also [[w/infile]] [[w/appendfile]] [[w/outstring]] [[w/stdout]] "
    (expander 'outfile var name body))

  (mac w/instring (var str . body)
    " Opens the given string `str' for input, assigning the stream to `var'.
      The stream is automatically closed on exit from the `body'.
      See also [[w/outstring]] [[fromstring]] [[w/infile]] [[w/stdin]]
      [[w/socket]] "
    (expander 'instring var str body))

  (mac w/socket (var port . body)
    " Opens the port for listening, assigning the stream to `var'.
      The stream is automatically closed on exit from the `body'.
      See also [[w/infile]] [[w/instring]] [[w/stdin]] "
    (expander 'open-socket var port body))
  )

(mac w/outstring (var . body)
  " Opens a string for output, assigning the stream to `var'.
    The stream is automatically closed on exit from the `body'.
    The contents of the string can be accessed via (inside `var')
    See also [[w/instring]] [[tostring]] [[w/outfile]] [[w/stdout]] "
  `(let ,var (outstring) ,@body))

(mac w/appendfile (var name . body)
  " Opens a file `name' for append, assigning the stream to `var'.
    The stream is automatically closed on exit from the `body'.
    See also [[w/outfile]] [[w/infile]] "
  `(let ,var (outfile ,name 'append)
     (after (do ,@body) (close ,var))))

; rename this simply "to"?  - prob not; rarely use

(mac w/stdout (str . body)
  " Opens the stream `str' for output; normal printed output from `body'
    is redirected to the stream.
    See also [[w/stdin]] [[w/outfile]] [[w/outstring]] "
  `(call-w/stdout ,str (fn () ,@body)))

(mac w/stdin (str . body)
  " Opens the stream `str' for input; normal read input from `body'
    is redirected from the stream.
    See also [[w/stdout]] [[w/infile]] [[w/instring]] [[w/socket]] "
  `(call-w/stdin ,str (fn () ,@body)))

(mac tostring body
  " Returns the printed standard output from `body' as a string.
    See also [[fromstring]] [[w/stdout]] [[w/outstring]] "
  (w/uniq gv
   `(w/outstring ,gv
      (w/stdout ,gv ,@body)
      (inside ,gv))))

(mac fromstring (str . body)
  " Redirects read standard input to `body' from the given string `str'.
    See also [[tostring]] [[w/stdin]] [[w/instring]] "
  (w/uniq gv
   `(w/instring ,gv ,str
      (w/stdin ,gv ,@body))))

(def readstring1 (s (o eof nil))
  " Reads a single expression from the string.
    See also [[read]] "
  (w/instring i s (read i eof)))

(def read ((o x (stdin)) (o eof nil))
  " Reads a single expression from a string or stream.
    See also [[readstring1]] [[readfile]] [[readfile1]] [[readall]] "
  (if (isa x 'string) (readstring1 x eof) (sread x eof)))

(def readfile (name)
  " Reads the expressions from the file `name', and returns a list of
    expressions read from the file.
    See also [[read]] "
  (w/infile s name (drain (read s))))

(def readfile1 (name)
  " Reads a single expression from the file `name'.
    See also [[read]] "
  (w/infile s name (read s)))

(def writefile1 (val name)
  " Writes the value to the file `name'.
    See also [[writefileraw]] "
  (w/outfile s name (write val s)) val)

(def writefileraw (val name) 
  " Write a list of bytes in val to a file.
    See also [[writefile1]] "
  (w/outfile s name (map [writeb _ s] val)))

(def readall (src (o eof nil))
  " Reads the expressions from the string or stream `src', and returns a
    list of expressions read from the file.
    See also [[read]] "
  ((afn (i)
    (let x (read i eof)
      (if (is x eof)
          nil
          (cons x (self i)))))
   (if (isa src 'string) (instring src) src)))

(def sym (x)
  " Returns the symbol for `x'.
    See also [[string]] "
  (coerce x 'sym))

(mac rand-choice exprs
  " Returns the result of one of the given `exprs', chosen at random.
    See also [[random-elt]] "
  `(case (rand ,(len exprs))
     ,@(let key -1 
         (mappend [list (++ key) _]
                  exprs))))

(mac n-of (n expr)
  " Repeats `expr' `n' times, then returns the results in a list.
    See also [[repeat]] "
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
  " Loops across the length of the sequence `s'.
    See also [[repeat]] [[each]] [[on]] "
  `(for ,var 0 (- (len ,s) 1) ,@body))

(mac on (var s . body)
  " Loops across the sequence `s', assigning each element to `var',
    and providing the current index in `index'.
    See also [[each]] [[forlen]] "
  (if (is var 'index)
      (err "Can't use index as first arg to on.")
      (w/uniq gs
        `(let ,gs ,s
           (forlen index ,gs
             (let ,var (,gs index)
               ,@body))))))

(def best (f seq)
  " Selects the best element of `seq' according to `f'.
    `f' is a comparison function between elements of `seq'.
    See also [[max]] [[most]] "
  (if (no seq)
      nil
      (let wins (car seq)
        (each elt (cdr seq)
          (if (f elt wins) (= wins elt)))
        wins)))
              
(def max args
  " Returns the highest argument.
    See also [[min]] [[best]] [[most]] "
  (best > args))
(def min args
  " Returns the lowest argument.
    See also [[max]] [[best]] [[most]] "
  (best < args))

; (mac max2 (x y)
;   (w/uniq (a b)
;     `(with (,a ,x ,b ,y) (if (> ,a ,b) ,a ,b))))

(def most (f seq) 
  " Selects the element of `seq' with the highest [f _].
    `f' is a score function for elements of `seq'.
    See also [[best]] [[least]] "
  (unless (no seq)
    (withs (wins (car seq) topscore (f wins))
      (each elt (cdr seq)
        (let score (f elt)
          (if (> score topscore) (= wins elt topscore score))))
      wins)))

(def least (f seq)
  " Selects the element of `seq' with the lowest [f _].
    `f' is a score function for elements of `seq'.
    See also [[most]] "
  (unless (no seq)
    (withs (wins (car seq) topscore (f wins))
      (each elt (cdr seq)
        (let score (f elt)
          (if (< score topscore) (= wins elt topscore score))))
      wins)))

; Insert so that list remains sorted.  Don't really want to expose
; these but seem to have to because can't include a fn obj in a 
; macroexpansion.
  
(def insert-sorted (test elt seq)
  " Inserts `elt' into a sequence `seq' sorted by `test'.
    See also [[sort]] [[insort]] [[reinsert-sorted]] "
  (if (no seq)
       (list elt) 
      (test elt (car seq)) 
       (cons elt seq)
      (cons (car seq) (insert-sorted test elt (cdr seq)))))

(mac insort (test elt seq)
  " Inserts `elt' into a sequence in the place `seq' sorted by `test'.
    See also [[insert-sorted]] [[sort]] "
  `(zap [insert-sorted ,test ,elt _] ,seq))

(def reinsert-sorted (test elt seq)
  " Inserts `elt' into a sequence `seq', partially sorted by `test'.
    See also [[insert-sorted]] [[insortnew]] [[sort]] "
  (if (no seq) 
       (list elt) 
      (is elt (car seq))
       (reinsert-sorted test elt (cdr seq))
      (test elt (car seq)) 
       (cons elt (rem elt seq))
      (cons (car seq) (reinsert-sorted test elt (cdr seq)))))

(mac insortnew (test elt seq)
  " Inserts `elt' into a sequence in the place `seq', partially sorted
    by `test'.
    See also [[reinsert-sorted]] [[sort]] "
  `(zap [reinsert-sorted ,test ,elt _] ,seq))

; Could make this look at the sig of f and return a fn that took the 
; right no of args and didn't have to call apply (or list if 1 arg).

(def memo (f)
  " Creates a function that will store results of calls to the given
    source function.
    For each set of arguments, the source function will only be called
    once; if the memo'ed function is called again with the same arguments,
    it will return the stored result instead of calling the source function.
    See also [[defmemo]] "
  (let cache (table)
    (fn args
      (or (cache args)
          (= (cache args) (apply f args))))))

(mac defmemo (name parms . body)
  " Defines a function that automatically stores the results of calls.
    For each set of arguments, this function will only execute once.
    If the function is called again with the same arguments, it will
    immediately return the stored result for that set of arguments.
    See also [[memo]] "
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
  " Determines if the given `c' is a whitespace character.
    See also [[alphadig]] [[nonwhite]] [[punc]] "
  (in c #\space #\newline #\tab #\return))

(def nonwhite (c)
  " Determines if the given `c' is not a whitespace character.
    See also [[whitec]] [[alphadig]] [[punc]] "
  (no (whitec c)))

(def alphadig (c)
  " Determines if the given `c' is an alphanumeric character.
    See also [[whitec]] [[nonwhite]] [[punc]] "
  (or (<= #\a c #\z) (<= #\A c #\Z) (<= #\0 c #\9)))

(def punc (c)
  " Determines if the given `c' is punctuation character.
    See also [[whitec]] [[nonwhite]] [[alphadig]] [[punc]] "
  (in c #\. #\, #\; #\: #\! #\?))

(def readline ((o str (stdin)))
  " Reads a string terminated by a newline from the stream `str'. "
  (when (peekc str)
    (tostring 
      ; can/should be improved by making this seamless check for
      ; \r, \r\n, \n, or \n\r terminators
      (whiler c (readc str) #\newline
        (writec c)))))

; Don't currently use this but suspect some code could.

(mac summing (sumfn . body)
  " Counts the number of times `sumfn' is called with a true value
    within `body'.
    See also [[accum]] "
  (w/uniq (gc gt)
    `(let ,gc 0
       (let ,sumfn (fn (,gt) (if ,gt (++ ,gc)))
         ,@body)
       ,gc)))

(def treewise (f base tree)
  " Traverses a list as a binary tree.
    See also [[trav]] [[tree-subst]] [[ontree]] "
  (if (atom tree)
      (base tree)
      (f (treewise f base (car tree)) 
         (treewise f base (cdr tree)))))

(def carif (x)
  " Returns the first element of a list if the argument is a list.
    See also [[car]] [[caris]] "
  (if (atom x) x (car x)))

; Could prob be generalized beyond printing.

(def prall (elts (o init "") (o sep ", "))
  " Prints several arguments with an initial header and separated by a
    given separator.
    See also [[prs]] "
  (when elts
    (pr init (car elts))
    (map [pr sep _] (cdr elts))
    elts))
             
(def prs args     
  " Prints several arguments separated by spaces.
    See also [[prall]] "
  (prall args "" #\space))

(def tree-subst (old new tree)
  " Replaces an element of a list with that list treated as a binary tree.
    See also [[treewise]] [[trav]] "
  (if (is tree old)
       new
      (atom tree)
       tree
      (cons (tree-subst old new (car tree))
            (tree-subst old new (cdr tree)))))

(def ontree (f tree)
  " Applies a function across each node of a list with that list treated
    as a binary tree.
    See also [[treewise]] [[trav]] "
  (f tree)
  (unless (atom tree)
    (ontree f (car tree))
    (ontree f (cdr tree))))

(def fill-table (table data)
  " Fills `table' with key-value pairs in the `data' list.
    See also [[table]] "
  (each (k v) (pair data) (= (table k) v))
  table)

(mac obj args
  " Creates an object with the specified entries.
    See also [[inst]] [[table]] "
  (w/uniq g
    `(let ,g (table)
       ,@(map (fn ((k v)) `(= (,g ',k) ,v))
              (pair args))
       ,g)))

(def keys (h) 
  " Returns a list of keys in the table or object `h'.
    See also [[vals]] [[table]] "
  (accum a (ontable k v h (a k))))

(def vals (h) 
  " Returns a list of values in the table or object `h'.
    See also [[keys]] [[table]] "
  (accum a (ontable k v h (a v))))

; These two should really be done by coerce.  Wrap coerce?

(def tablist (h)
  " Transforms a table or object `h' into an association list.
    See also [[listtab]] [[alref]] [[assoc]] "
  (accum a (maptable (fn args (a args)) h)))

(def listtab (al)
  " Transforms an association list into a table or object.
    See also [[tablist]] [[alref]] [[assoc]] "
  (let h (table)
    (map (fn ((k v)) (= (h k) v))
         al)
    h))

(def load-table (file (o eof))
  " Loads an association list from `file' into a table or object.
    See also [[load-tables]] [[read-table]] [[save-table]] [[listtab]] "
  (w/infile i file (read-table i eof)))

(def read-table ((o i (stdin)) (o eof))
  " Loads an association list from the stream `i' into a table or object.
    See also [[load-tables]] [[load-table]] [[write-table]] [[listtab]] "
  (let e (read i eof)
    (if (alist e) (listtab e) e)))

(def load-tables (file)
  " Loads several association lists from `file' into a list of tables or
    objects.
    See also [[load-table]] [[read-table]] "
  (w/infile i file
    (w/uniq eof
      (drain (read-table i eof) eof))))

(def save-table (h file)
  " Writes a table or object `h' to `file'.
    See also [[write-table]] [[load-table]] [[tablist]] "
  (w/outfile o file (write-table h o)))

(def write-table (h (o o (stdout)))
  " Writes a table or object `h' to the stream `o'.
    See also [[save-table]] [[read-table]] [[tablist]] "
  (write (tablist h) o))

(def copy (x . args)
  " Creates a copy of an existing argument `x'.
    See also [[rev]] "
  (let x2 (case (type x)
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
                   (err "Can't copy " x))
    (map (fn ((k v)) (= (x2 k) v))
         (pair args))
    x2))

(def abs (n)
  " Returns the absolute value of a number.
    See also [[signop]] "
  (if (< n 0) (- n) n))

(def signop (n)
  " Returns the sign of a number as the function `+' or `-'.
    See also [[abs]] "
  (if (< n 0) - +))

; The problem with returning a list instead of multiple values is that
; you can't act as if the fn didn't return multiple vals in cases where
; you only want the first.  Not a big problem.

(def round (n)
  " Rounds off a fractional value to the nearest whole number.
    See also [[roundup]] [[to-nearest]] "
  (withs (base (trunc n) rem (abs (- n base)))
    (if (> rem 1/2) ((if (> n 0) + -) base 1)
        (< rem 1/2) base
        (odd base)  ((if (> n 0) + -) base 1)
                    base)))

(def roundup (n)
  " Rounds off a fractional value to the nearest absolute highest
    whole number.
    See also [[round]] [[to-nearest]] "
  (withs (base (trunc n) rem (abs (- n base)))
    (if (>= rem 1/2) 
        ((if (> n 0) + -) base 1)
        base)))

(def to-nearest (n quantum)
  " Rounds off `n' to the nearest multiple of `quantum'.
    See also [[round]] [[roundup]] "
  (* (roundup (/ n quantum)) quantum))

(def avg (ns) " Averages all numbers in `ns'. " (/ (apply + ns) (len ns)))

; Use mergesort on assumption that mostly sorting mostly sorted lists
; benchmark: (let td (n-of 10000 (rand 100)) (time (sort < td)) 1) 

(def sort (test seq)
  " Sorts `seq' according to `test'. "
  (if (alist seq)
      (mergesort test (copy seq))
      (coerce (mergesort test (coerce seq 'cons)) (type seq))))

; Destructive stable merge-sort, adapted from slib and improved 
; by Eli Barzilay for MzLib; re-written in Arc.

(def mergesort (less? lst)
  " Sorts a list `lst' according to `less?'. "
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
  " Merges two sorted lists by `less?'. "
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
  " Returns a list of the best `n' elements of seq according to
    the comparison function `f'.
    See also [[best]] "
  (firstn n (sort f seq)))

(def split (seq pos)
  " Splits `seq' at offset `pos', returning a two-element list of the
    split.
    See also [[cut]] "
  (if (is 0 pos) (list nil seq)
      (withs (mid (nthcdr (- pos 1) seq) 
              s2  (cdr mid))
        (wipe (cdr mid))
        (list seq s2))))

(def ssplit (str (o delim whitec) (o keepdelim) (o noblanks))
  "Split `str' on chars passing the test `delim', returning a list of
   strings.  If `keepdelim' is non-nil include the delimiters.  If
   `noblanks' is non-nil empty strings are excluded."
  (if (isa delim 'string) (= delim [in _ (coerce delim 'cons)]))
  (with (acc nil j 0)
    (forlen i str
      (if (and (or (no keepdelim) (> i 0))
                   (delim (str i)))
           (do (push (cut str j i) acc)
               (= j (if keepdelim i (+ i 1)))))
      (if (and (atend i str)
               (<= j i))
          (push (cut str j (+ i 1)) acc))) ; add 1 because atend is true prematurely
    (rev (if noblanks (rem empty acc) acc))))

(mac time (expr)
  " Prints the time consumed by the `expr', returning the result. "
  (w/uniq (t1 t2)
    `(let ,t1 (msec)
       (do1 ,expr
            (let ,t2 (msec)
              (prn "time: " (- ,t2 ,t1) " msec."))))))

(mac jtime (expr)
  " Prints the time consumed by `expr', returning `ok' when the
    expression completes. "
  `(do1 'ok (time ,expr)))

(mac time10 (expr)
  " Prints the time consumed by executing `expr' 10 times "
  `(time (repeat 10 ,expr)))

(def union (f xs ys)
  (+ xs (rem (fn (y) (some [f _ y] xs))
             ys)))

(= templates* (table))

(mac deftem (tem . fields)
  " Defines an object template for field values, with inclusion for
    existing templates.
    See also [[inst]] [[templatize]] [[temread]] [[temload]] [[temloadall]] "
  (withs (name (carif tem) includes (if (acons tem) (cdr tem)))
    `(= (templates* ',name) 
        (+ (mappend templates* ',(rev includes))
           (list ,@(map (fn ((k v)) `(list ',k (fn () ,v)))
                        (pair fields)))))))

(mac addtem (name . fields)
  `(= (templates* ',name) 
      (union (fn (x y) (is (car x) (car y)))
             (list ,@(map (fn ((k v)) `(list ',k (fn () ,v)))
                          (pair fields)))
             (templates* ',name))))

(def inst (tem . args)
  " Creates an object instantiating a given template.
    See also [[deftem]] [[templatize]] [[temread]] [[temload]] [[temloadall]] "
  (let x (table)
    (each (k v) (templates* tem)
      (unless (no v) (= (x k) (v))))
    (each (k v) (pair args)
      (= (x k) v))
    x))

; To write something to be read by temread, (write (tablist x))

(def temread (tem (o str (stdin)))
  " Reads an association list from the stream `str' and creates an
    object instantiating the given template containing the data in
    the association list.
    See also [[deftem]] [[inst]] [[templatize]] [[temload]] [[temloadall]] "
  (templatize tem (read str)))

; Converts alist to inst; ugly; maybe should make this part of coerce.
; Note: discards fields not defined by the template.

(def templatize (tem raw)
  " Creates an object instantiating a given template containing the
    data in the association list `raw'.
    See also [[deftem]] [[inst]] [[temread]] [[temload]] [[temloadall]] "
  (with (x (inst tem) fields (templates* tem))
    (each (k v) raw
      (when (assoc k fields)
        (= (x k) v)))
    x))

(def temload (tem file)
  " Reads an association list from `file' and creates an object
    instantiating the given template containing the data in the
    association list.
    See also [[deftem]] [[inst]] [[templatize]] [[temread]] [[temloadall]] "
  (w/infile i file (temread tem i)))

(def temloadall (tem file)
  " Reads all association lists from `file' and creates a list
    of objects instantiating the given template containing the
    data in each association list.
    See also [[deftem]] [[inst]] [[templatize]] [[temread]] [[temload]]"
  (map (fn (pairs) (templatize tem pairs)) 
       (w/infile in file (readall in))))

(def tems ()
  " Pretty print templates defined in `templates*'.
    See also [[deftem]] [[inst]] [[templatize]] [[temread]] [[temload]] "
  (each k (keys templates*)
    (prn k " " (map car (templates* k)))))

(def number (n) " Determines if `n' is a number. " (in (type n) 'int 'num))

(def since (t1) (- (seconds) t1))

(def minutes-since (t1) (/ (since t1) 60))

(def hours-since (t1) (/ (since t1) 3600))

(def days-since (t1) (/ (since t1) 86400))

(def cache (timef valf)
  " Caches the result of a call to `valf' until a number of seconds
    greater than the result of a call to `timef' have passed. "
  (with (cached nil gentime nil)
    (fn ()
      (unless (and cached (< (since gentime) (timef)))
        (= cached  (valf)
           gentime (seconds)))
      cached)))

(mac errsafe (expr)
  " Executes `expr' and blocks any errors "
  `(on-err (fn (c) nil)
           (fn () ,expr)))

(def saferead (arg)
  " Reads an expression, blocking any errors. "
  (errsafe (read arg)))

(def safe-load-table (filename) 
  " Loads a table from `filename', blocking any errors. "
  (or (errsafe (load-table filename))
      (table)))

(def mkdir (path (o parents))
   " Creates a directory.
     If `parents' is non-nil, parent directories are created as needed."
  ((let os (which-os)
     (if
       ; If we're running Unix, MzScheme <371 has a bug
       ; where make-directory sets the sticky bit.
       ; Thus, we want to use system instead.
       (or (is os 'unix) (is os 'macosx))
        [system (string "mkdir " (if parents "-p ") _)]

       parents make-directory*
       make-directory))
   path))

(def ensure-dir (path)
  " Ensures that the specified directory exists, and creates it if not
    yet created. "
  (unless (dir-exists path)
          (mkdir path t)))

(def pad (val digits (o char #\ ))
  (= val (string val))
  (string (n-of (- digits (len val)) char) val))

(def date ((o time (seconds)))
  " Returns the date as a string in YYYY-MM-DD format. "
  (let date (datetbl time)
    (string (pad (date 'year) 4 #\0) "-"
            (pad (date 'month) 2 #\0) "-"
            (pad (date 'day) 2 #\0))))

(def count (test x)
  " Counts the number of elements in `x' which pass `test'. "
  (with (n 0 testf (testify test))
    (each elt x
      (if (testf elt) (++ n)))
    n))

(def ellipsize (str (o limit 80))
  " Trims a string `str' with `...' if it is longer than the given `limit'. "
  (if (<= (len str) limit)
      str
      (+ (cut str 0 limit) "...")))

(def random-elt (seq) 
  " Returns an element of `seq' chosen by random.
    See also [[rand-choice]] "
  (seq (rand (len seq))))

(mac until (test . body)
  " While `test' is false, perform `body' in a loop.
    See also [[while]] "
  `(while (no ,test) ,@body))

(def before (x y seq (o i 0))
  " Determines if `x' exists before `y' in `seq'. "
  (with (xp (pos x seq i) yp (pos y seq i))
    (and xp (or (no yp) (< xp yp)))))

(def par (f . args)
  " Partially apply `f' to `args'; i.e., return a function which, when called,
    calls `f' with `args' and the arguments to the new function. "
  (fn newargs (apply f (join args newargs))))

(def orf fns
  " Creates a function which returns true on its argument if any of the
    given `fns' return true on that argument. "
  (fn (x) (some [_ x] fns)))

(def andf fns
  " Creates a function which returns true on its argument if all of the
    given `fns' return true on that argument. "
  (fn (x) (all [_ x] fns)))

(def atend (i s)
  " Determines if the index `i' is at or beyond the end of the sequence `s'. "
  (> i (- (len s) 2)))

(def multiple (x y)
  " Determines if `x' is a multiple of `y'. "
  (is 0 (mod x y)))

(mac nor args
  " Computes arguments until one of them returns true, then returns nil,
    or else returns true. "
  `(no (or ,@args)))

; Consider making the default sort fn take compare's two args (when do 
; you ever have to sort mere lists of numbers?) and rename current sort
; as prim-sort or something.

; Could simply modify e.g. > so that (> len) returned the same thing
; as (compare > len).

(def compare (comparer scorer)
  " Creates a function that compares using `comparer' the result of `scorer'
    on its arguments. "
  (fn (x y) (comparer (scorer x) (scorer y))))

; Cleaner thus, but may only ever need in 2 arg case.

;(def compare (comparer scorer)
;  (fn args (apply comparer map scorer args)))

; (def only (f g . args) (aif (apply g args) (f it)))

(def only (f) 
  (fn args (if (car args) (apply f args))))

(mac conswhen (f x y)
  " Adds `x' to the front of `y' if `x' passes the test `f'. "
  (w/uniq (gf gx)
   `(with (,gf ,f ,gx ,x)
      (if (,gf ,gx) (cons ,gx ,y) ,y))))

; Could rename this get, but don't unless it's frequently used.
; Could combine with firstn if put f arg last, default to (fn (x) t).

(def firstn-that (n f xs)
  " Returns the first `n' elements of `xs' which pass `f'. "
  (if (or (<= n 0) (no xs))
       nil
      (f (car xs))
       (cons (car xs) (firstn-that (- n 1) f (cdr xs)))
       (firstn-that n f (cdr xs))))

(def dedup (xs)
  " Removes duplicated elements from `xs'. "
  (with (h (table) acc nil)
    (each x xs
      (unless (h x)
        (push x acc)
        (assert (h x))))
    (rev acc)))

(def single (x)
  " Determines if `x' is a list with only one element. "
  (and (acons x) (no (cdr x))))

(def intersperse (x ys)
  " Inserts `x' between elements of `ys'. "
  (cons (car ys)
        (mappend [list x _] (cdr ys))))

(def counts (seq (o c (table)))
  " Returns a table with elements of `seq' as keys and the number of
    occurences of that element as values. "
  (if (no seq)
      c
      (do (zap [if _ (+ _ 1) 1] (c (car seq)))
          (counts (cdr seq) c))))

(def commonest (seq)
  " Returns a two-element list containing the most common element of
    `seq' and the number of times it occured in the sequence. "
  (with (winner nil n 0)
    (ontable k v (counts seq)
      (when (> v n) (= winner k n v)))
    (list winner n)))

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
    " Applies `f' to an accumulated result on the elements of `xs'.
      Elements are processsed left-to-right. "
    ((afn (xs)
       (if (cdr xs) (self (cons (f (car xs) (cadr xs)) (cddr xs)))
           xs (car xs)
           (f)))
     (if (is init initsym) xs (cons init xs))))

  ; Right-associative
  ; Rather inefficent due to recursive call not being in the tail position.
  (def rreduce (f xs (o init initsym))
    " Applies `f' to an accumulated result on the elements of `xs'.
      Elements are processsed right-to-left. "
    ((afn (xs)
       (if (cdr xs) (f (car xs) (rreduce f (cdr xs)))
           xs (car xs)
           (f)))
     (if (is init initsym) xs (join xs (list init))))))

(let argsym (uniq)

  (def parse-format (str)
    " Parses a simple ~-format string. "
    (rev (accum a
           (with (chars nil  i -1)
             (w/instring s str
               (whilet c (readc s)
                 (case c 
                   #\# (do (a (coerce (rev chars) 'string))
                           (wipe chars)
                           (a (read s)))
                   #\~ (do (a (coerce (rev chars) 'string))
                           (wipe chars)
                           (readc s)
                           (a (list argsym (++ i))))
                       (push c chars))))
              (when chars
                (a (coerce (rev chars) 'string)))))))
  
  (mac prf (str . args)
    " Prints according to a format string, replacing ~* with arguments. "
    `(let ,argsym (list ,@args)
       (pr ,@(parse-format str))))
)

(wipe load-file-stack*)
;;why not (o hook idfn) ?
(def load (file (o hook))
  " Reads the expressions in `file' and evaluates them.  Read expressions
    may be preprocessed by `hook'.
    See also [[require]]. "
  (push current-load-file* load-file-stack*)
  (= current-load-file* file)
  (or= hook idfn)
  (after
    (w/infile f file
      (whilet e (read f)
        (eval (hook e))))
    (do (= current-load-file* (pop load-file-stack*)) nil)))

(= required-files* (table))

(def require (file)
  " Loads `file' if it has not yet been `require'd.  Can be fooled by changing
    the name ((require \"foo.arc\") as opposed to (require \"./foo.arc\")), but
    this should not be a problem.
    See also [[load]]. "
  (or (required-files* file)
      (do
        (= (required-files* file) t)
        (load file))))

(def positive (x)
  " Determines if `x' is a number and is positive. "
  (and (number x) (> x 0)))

(mac w/table (var . body)
  " Creates a table assigned to `var' for use in `body'. "
  `(let ,var (table) ,@body ,var))

(def ero args
  " Outputs `args' to error output. "
  (w/stdout (stderr) 
    (write (car args))
    (each a (cdr args)
      (writec #\space)
      (write a))
    (writec #\newline))
  (car args))

(def queue ()
  " Creates a queue.
    See also [[enq]] [[deq]] [[qlen]] [[qlist]] [[enq-limit]]"
  (list nil nil 0))

; Despite call to atomic, once had some sign this wasn't thread-safe.
; Keep an eye on it.

(def enq (obj q)
  " Adds `obj' to a queue.  See also [[queue]] "
  (atomic
    (++ (q 2))
    (if (no (car q))
        (= (cadr q) (= (car q) (list obj)))
        (= (cdr (cadr q)) (list obj)
           (cadr q)       (cdr (cadr q))))
    (car q)))

(def deq (q)
  " Removes and returns an item from a queue.  See also [[queue]] "
  (atomic (unless (is (q 2) 0) (-- (q 2)))
          (pop (car q))))

; Should redef len to do this, and make queues lists annotated queue.

(def qlen (q) " Returns the number of items in a queue.  See also [[queue]] "
  (q 2))

(def qlist (q) " Returns the queue contents as a list.  See also [[queue]] "
  (car q))

;; unsafe - suppose we have (enq-limit x q 10) and (enq-limit x q 1000)
;; somewhere else?
(def enq-limit (val q (o limit 1000))
  " Adds an item to the queue; removes a queue item if `limit' is
    exceeded.  See also [[queue]] "
  (atomic
     (unless (< (qlen q) limit)
       (deq q))
     (enq val q)))

(def median (ns)
  " Computes the median of an unsorted list. "
  ((sort > ns) (trunc (/ (len ns) 2))))

(mac noisy-each (n var val . body)
  " Performs `body' for each element of the sequence returned by `expr',
    with each element assigned to `var'; prints a `.' every `n' elements. "
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
  " Creates a form which may be exited by calling `name' from within `body'.
    See also [[catch]] [[breakable]] "
  (w/uniq g
    `(ccc (fn (,g)
            (let ,name [,g _]
              ,@body)))))

(mac catch body
  " Catches any value returned by `throw' within `body'.
    See also [[breakable]] [[point]] "
  `(point throw ,@body))

(def downcase (x)
  " Converts `x' to lowercase, if a character, string, or symbol;
    otherwise, raises an error. "
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
  " Converts `x' to uppercase, if a character, string, or symbol;
    otherwise, raises an error. "
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
  (given stopcond (if (> step 0) > <)
         acc      (cons start nil)
    (unless (stopcond start end)
      ((afn (acc tl n)
         (if (stopcond n end)
             acc
             (self acc (= (cdr tl) (cons n nil)) (+ n step))))
       acc acc (+ start step)))))

(def mismatch (s1 s2)
  " Returns the first index where `s1' and `s2' do not match. "
  (catch
    (on c s1
      (when (isnt c (s2 index))
        (throw index)))))

(def memtable (ks)
  " Creates a membership table which returns t for each element in `ks' and
    nil otherwise. "
  (let h (table)
    (each k ks (assert (h k)))
    h))

(= bar* " | ")

(mac w/bars body
  " Prints out the strings printed by each expression in `body',
    separated by vertical bars. "
  (w/uniq (out needbars)
    `(let ,needbars nil
       (do ,@(map (fn (e)
                    `(let ,out (tostring ,e)
                       (unless (is ,out "")
                         (if ,needbars
                             (pr bar* ,out)
                             (do (assert ,needbars)
                                 (pr ,out))))))
                  body)))))

(def len< (x n) (< (len x) n))

(def len> (x n) (> (len x) n))

(mac thread body 
  " Launches the expressions in `body' in a new thread, returning the
    thread ID for that thread. "
  `(new-thread (fn () ,@body)))

(mac trav (x . fs)
  " Traverses an object `x'; the object is applied to each function
    in `fs', and sub-nodes of the object may be traversed by
    (self <node>) in any of the functions.
    See also [[trav+]] [[treewise]] [[ontree]] "
  (w/uniq g
    `((afn (,g)
        (when ,g
          ,@(map [list _ g] fs)))
      ,x)))

(mac trav+ ((go n) s . body)
  " Traverses an object `s'; the object is named by `n' and sub-nodes
    of the object may be traversed by (`go' ...) in `body'.
    See also [[trav]]
    p.s. a more lisplike version of pg's trav "
  `((rfn ,go (,n)
      (when ,n ,@body))
     ,s))

(= hooks* (table))

(def hook (name . args)
  (aif (hooks* name) (apply it args)))

(mac defhook (name . rest)
  `(= (hooks* ',name) (fn ,@rest)))
  
(mac varif (name (o default))
  "Returns the value of the variable `name' if it exists, or `default'
   otherwise."
  `(if (bound ',name) ,name ,default))

(mac redef (name parms . body)
  " Redefine a function.  The old function definition may be used within
    `body' as the name `old'. "
  `(do (tostring
        (let old (varif ,name nilfn)
          (= ,name (fn ,parms ,@body))))
       ,name))

(redef table args
  " Creates a table initializing table entries from passed
    key-value pairs.
    See also [[obj]] [[inst]] "
  (let tb (old)
    (fill-table tb args)
    tb))

(mac lsrc (name) `(source* ',name))
(mac src (name)
     `(do 
	((only [prn "(from \"" _ "\")"]) (source-file* ',name))
	(ppr (lsrc ,name))))


(mac help ( (o name 'help))
   " Prints the documentation for the given symbol.  To use, type
     (help symbol) ; you may also use (help \"string\") to search
     all documentation for that string. "
   (if
     (isa name 'sym)
       `(do (pr ,(helpstr name))
            nil)
     (isa name 'string)
       `(helpsearch (downcase ,name))
     t
       `(do (pr ,(helpstr 'help)) nil) ))

(def helpsearch (str)
  " Prints all symbols whose documentation matches or partly matches `str'. "
  (prall (helpsearch-core str) "Related symbols:\n" "\n")
  (prn)
  nil)

(def helpsearch-core (str)
  " Returns a list of symbols whose documentation matches or partly matches
    `str'. "
  (let part-match
      (let rx (re (downcase str))
         [re-match rx (downcase (coerce _ 'string))])
      (sort <
        (accum add
          (ontable k (typ d) help*
            (when (or (part-match k) (part-match typ) (part-match d) (only.part-match (source-file* k)))
              (add k)))))))

(def helpstr (name (o verbose t))
  " Returns a help string for the symbol `name'. "
  (tostring
   (let h (help* name)
     (if (no h)
         (if verbose (prn name " is not documented."))
         (with (kind  (car h)
                doc   (cadr h))
           (aand verbose ((only [prn "(from \"" _ "\")"]) (source-file* name)))
           (pr "[" kind "]" (if (is kind 'mac) " " "  "))
           (prn (if (sig name)
                    (cons name (sig name))))
           (and verbose doc (prn doc)))))))

(mac fns ((o pfx "") (o test))
  "Print sigs for macros & functions starting with pfx, or that pass test if given."
  (w/uniq (p t)
    `(withs (,p (string ',pfx) ,t (or ,test [prefix ,p _]))
    	 (each f (sort < (keep ,t (map [string _] (keys sig))))
               (pr (helpstr (sym f) nil))))))

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
  (= %%% %%
     %% %)
  (tostring (mac % () expr)))

(= ^ nil
   ^^ nil
   ^^^ nil)

(def output-history-update (val)
  (= ^^^ ^^
     ^^ ^
     ^ val))

(set current-load-file* nil)


; any logical reason I can't say (push x (if foo y z)) ?
;   eval would have to always ret 2 things, the val and where it came from
; idea: implicit tables of tables; setf empty field, becomes table
;   or should setf on a table just take n args?

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
;  or something a bit more semantic?
; could uniq be (def uniq () (annotate 'symbol (list 'u))) again?
; idea: use x- for (car x) and -x for (cdr x)  (but what about math -?)
; idea: get rid of strings and just use symbols
; could a string be (#\a #\b . "") ?
; better err msg when , outside of a bq
; idea: parameter (p foo) means in body foo is (pair arg)
; idea: make ('string x) equiv to (coerce x 'string) ?  or isa?
;   quoted atoms in car valuable unused semantic space
; idea: if (defun foo (x y) ...), make (foo 1) return (fn (y) (foo 1 y))
;   probably would lead to lots of errors when call with missing args
;   but would be really dense with . notation, (foo.1 2)
; or use special ssyntax for currying: (foo@1 2)
; remember, can also double; could use foo::bar to mean something
; wild idea: inline defs for repetitive code
;  same args as fn you're in
; variant of compose where first fn only applied to first arg?
;  (> (len x) y)  means (>+len x y)
; use ssyntax underscore for a var?
;  foo_bar means [foo _ bar]
;  what does foo:_:bar mean?
; matchcase
; crazy that finding the top 100 nos takes so long:
;  (let bb (n-of 1000 (rand 50)) (time10 (bestn 100 > bb)))
;  time: 2237 msec.  -> now down to 850 msec

