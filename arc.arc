; Main Arc lib.  Ported to Scheme version Jul 06.

; don't like names of conswhen and consif

; need better way of generating strings; too many calls to string
;  maybe strings with escape char for evaluation
; make foo~bar equiv of foo:~bar (in expand-ssyntax)
; add sigs of ops defined in ac.scm
; get hold of error types within arc
; does macex have to be defined in scheme instead of using def below?
; write disp in arc
; could I get all of macros up into arc.arc?
; warn when shadow a global name
; some simple regexp/parsing plan

; compromises in this implementation:
; no objs in code
;  (mac testlit args (listtab args)) breaks when called
; separate string type
;  (= (cdr (cdr str)) "foo") couldn't work because no way to get str tail
;  not sure this is a mistake; strings may be subtly different from
;  lists of chars

(assign incompatibilities (fn () (disp
"The following behave differently from arc 3.1:

1. `for`. See (help for).
2. Templates (arc's lightweight object database system). See (help deftem).

If you find others, please report them at http://arclanguage.org/forum.
")))

(assign current-load-file* "arc.arc")
(assign source-file* (table))
(assign source* (table))
(assign help* (table))

(assign remac (annotate 'mac
                (fn (name parms . body)
                   ; remac doesn't actually make the help text available
                   ; but adding it won't do any harm
                   `(assign ,name (annotate 'mac (fn ,parms ,@body))))))

(remac document (definer name parms doc . body)
  `((fn ()
     (sref sig* ',parms ',name)
     (sref help* ,doc ',name)  ; doc will be a literal string
     (sref source-file* current-load-file* ',name)
     (sref source* '(,definer ,name ,parms ,@body) ',name))))

(document builtin fn (params . body)
"Creates an anonymous function. See the tutorial: http://ycombinator.com/arc/tut.txt")

(document builtin apply (f . args)
"(apply f '(1 2 3)) <=> (f 1 2 3)
(apply f x y '(z w)) <=> (f x y z w)")

(document builtin annotate (tag . val)
"Creates a user-defined tagged type containing 'val'. See also [[type]] and [[rep]].")

(document builtin type (x)
"Returns the type of 'x', even if 'x' is a user-defined tagged-type.")

(document builtin rep (x)
"Returns the contents of a user-defined tagged type object.")

(document builtin assign (x y)
"Set variable 'x' to value 'y'.")

(document builtin bound (x)
"Does variable 'x' currently have a value?")

(remac warn-if-bound (var)
  `(if (bound ',var)
     ((fn () (disp "*** redefining " (stderr))
             (disp ',var (stderr))
             (disp #\newline (stderr))))))

(remac mac (name parms . body)
"Defines a new *macro*, or abbreviation for some more complex code.
Macros are the hallmark of lisp, the ability to program on programs.
For more information see the tutorial: http://ycombinator.com/arc/tut.txt
Or come ask questions at http://arclanguage.org/forum"
  `((fn () (warn-if-bound ,name)
           (document mac ,name ,parms
                       ,@(if (is (type car.body) 'string)
                           body
                           (cons nil body)))
           (remac ,name ,parms ,@body))))

(assign examples* (table))

(mac examples (name . tests-and-expected-results)
"Shows some example calls of a function as an enhancement of its docstring.
Usually provided immediately after a function docstring+definition, so it
isn't underfoot when it isn't needed.

Usage: (examples name-being-tested
          expr1
          expected-result1
          expr2
          expected-result2
          ...)

Expected results are optional. When provided, they can remind you when
documentation goes out of date. To avoid printing and checking them, use an _
wildcard. For example:

  (examples foo
    (foo x)
    _
    (foo y z)
    _)

Expected results are compared directly, without evaluation. For example:

  (def foo (a b c)
    (list a b c))

  (examples foo
    (foo 1 2 3)
    (1 2 3))            <-- no quote

If the result is an object that read can't handle, use valueof. For example:

  (examples foo
    (foo x)
    (valueof (obj a x)))"
  `(sref examples* ',tests-and-expected-results ',name))

(examples assign
  (assign x 10)
  _)

(examples bound
  (do (= y 10)
      (bound 'y))
  t)

(mac do args
"Evaluates each expression in sequence and returns the result of the
last expression."
  `((fn () ,@args)))

(examples do
  (do (prn "line 1")
      (prn "line 2")
      (prn "line 3"))
  _)

(mac def (name parms . body)
"Defines a new function called 'name'. When called, the function runs
'body', parameterizing 'parms' with call arguments.
For more information see the tutorial: http://ycombinator.com/arc/tut.txt
Or come ask questions at http://arclanguage.org/forum"
  `(do (warn-if-bound ,name)
       (document def ,name ,parms
                   ,@(if (is (type car.body) 'string)
                       body
                       (cons nil body)))
       (assign ,name (fn ,parms ,@body))))

(mac redef (name parms . body)
"Defines a new function like [[def]], but doesn't warn if 'name' already exists."
  `(do (if (~help* ',name)  ; assume any existing help is still accurate
         (document redef ,name ,parms
                       ,@(if (is (type car.body) 'string)
                           body
                           (cons nil body))))
       (assign ,name (fn ,parms ,@body))))

(document builtin cons (x xs) "Returns a new list with element 'x' added to the start of list 'xs'.")
(document builtin car (xs) "Returns the first element of list 'xs'")
(document builtin cdr (xs) "Returns all elements of list 'xs' but the first")

(def caar (xs)
"Equivalent to (car (car xs))"
  (car:car xs))
(def cadr (xs)
"Equivalent to (car (cdr xs)). Returns the second element of the list 'xs'"
  (car:cdr xs))
(def cddr (xs)
"Equivalent to (cdr (cdr xs)). Returns all elements of list 'xs' but the first two."
  (cdr:cdr xs))
(def cdar (xs)
"Equivalent to (cdr (car xs))."
  (cdr:car xs))
(def cadar (xs)
"Equivalent to (car (cdar xs))."
  (car:cdar xs))

(def no (x)
"Is 'x' nil? Sometimes we say A passes if it's non-nil, in which case no.A is said to fail."
  (is x nil))

(def acons (x)
"Is 'x' a non-nil list?"
  (is type.x 'cons))

; Can return to this def once Rtm gets ac to make all rest args
; nil-terminated lists.

; (def list args args)

(def list args
"Creates a list containing the given 'args'."
  (if no.args
    nil
    (cons car.args
          (apply list cdr.args))))

(examples list
  (list 1 2 3)
  (1 2 3)
  (list "a" '(1 2) 3)
  ("a" (1 2) 3))

(document def len (x)
"Computes the size of a list, string, hash table or other user-defined type.")

(examples len
  (len '(1 2 3))
  3
  (len "abcd")
  4
  (len (obj a 1 b 2))
  2)

(def idfn (x)
"The identity function. Returns whatever is passed in."
  x)

(def map1 (f xs)
"Returns a list containing the result of function 'f' applied to every element of 'xs'."
  (if (no xs)
    nil
    (cons (f car.xs)
          (map1 f cdr.xs))))

(examples map1
  (map1 cdr '((1) (2 3) (4 5)))
  (nil (3) (5))
  (map1 [list _ (* _ 10)]
        '(1 2 3))
  ((1 10) (2 20) (3 30)))

(def pair (xs (o f list))
"Splits the elements of 'xs' into buckets of two, and optionally applies the
function 'f' to them."
  (if (no xs)
       nil
      (no cdr.xs)
       (list (list car.xs))
      (cons (f car.xs cadr.xs)
            (pair cddr.xs f))))

(examples pair
  (pair '(a b c d))
  ((a b) (c d))
  (pair '(a b c d e))
  ((a b) (c d) (e))
  (pair '(1 2 3 4) +)
  (3 7)
  (pair '(10 2 3 40 50 6) max)
  (10 40 50))

(mac make-br-fn (body)
"The function invoked on square-bracket calls.
For example, [car _] => (make-br-fn (car _)) => (fn (_) (car _))"
  `(fn (_) ,body))

(mac and args
"Stops at the first argument to fail (return nil). Returns the last argument before stopping."
  (if args
    (if (cdr args)
      `(if ,(car args) (and ,@(cdr args)))
      (car args))
    t))

(def assoc (key al)
"Finds a (key value) pair in an association list 'al' of such pairs."
  (if (no acons.al)
       nil
      (and (acons (car al)) (is (caar al) key))
       (car al)
      (assoc key (cdr al))))

(def alref (al key)
"Returns the value of 'key' in an association list 'al' of (key value) pairs"
  (cadr (assoc key al)))

(mac with (parms . body)
"Evaluates all expressions in 'body' under the bindings provided in 'parms'.
Returns value of last expression in 'body'.
For example, (with (x 1 y 2)
               (+ x y))
             => 3"
  `((fn ,(map1 car (pair parms))
     ,@body)
    ,@(map1 cadr (pair parms))))

(mac let (var val . body)
"Like [[with]] but with just one binding.
For example, (let x 1
               (+ x 1))
             => (with (x 1)
                  (+ x 1))
             => 2"
  `(with (,var ,val) ,@body))

(mac withs (parms . body)
"Like [[with]], but binding for a variable can refer to earlier variables.
For example, (withs (x 1 y (+ x 1))
               (+ x y))
             => 3"
  (if (no parms)
    `(do ,@body)
    `(let ,(car parms) ,(cadr parms)
       (withs ,(cddr parms) ,@body))))

(mac ret (var val . body)
"Like [[let]], but returns 'val' rather than the value of the final form in 'body'."
  `(let ,var ,val ,@body ,var))

(mac w/uniq (names . body)
"Assigns a set of variables to unique symbols.
Useful for avoiding name capture in macros; see the tutorial: http://ycombinator.com/arc/tut.txt"
  (if (acons names)
    `(with ,(apply + nil (map1 (fn (n) `(,n (uniq ',n)))
                           names))
       ,@body)
    `(let ,names (uniq ',names) ,@body)))

(mac do1 args
"Like [[do]], but returns the value of the first arg rather than the last."
  (w/uniq g
    `(ret ,g ,(car args)
       ,@(cdr args))))

(mac defextend (name args pred . body)
"Extends an existing function to trigger only if 'pred' is non-nil."
  (w/uniq (old allargs)
    `(let ,old ,name
       (redef ,name ,allargs
         (let ,args ,allargs
           (if ,pred
             (do ,@body)
             (apply ,old ,allargs))))
       (sref sig* ',args ',name))))

; Rtm prefers to overload + to do this

(def join args
"Concatenates/appends its arguments into a new list."
  (if (no args)
    nil
    (let a (car args)
      (if (no a)
        (apply join (cdr args))
        (cons (car a) (apply join (cdr a) (cdr args)))))))

(examples join
  (join '(1 2) nil '(3 4))
  (1 2 3 4))

(mac rfn (name parms . body)
"Like [[fn]] but permits the created function to call itself recursively as the given 'name'."
  `(let ,name nil
     (assign ,name (fn ,parms ,@body))))

(mac afn (parms . body)
"Like [[fn]] and [[rfn]] but the created function can call itself as 'self'"
  `(let self nil
     (assign self (fn ,parms ,@body))))

(examples afn
  ((afn (x)  ; powers of two
     (if (is x 0)
       1
       (* 2 (self (- x 1)))))
   5)
  32)

; a more readable variant of afn
; http://awwx.ws/xloop0; http://arclanguage.org/item?id=10055
(mac loop (withses . body)
"Like 'with', but the body can also be rerun with new bindings by calling 'recur'.
Often a more readable alternative to [[rfn]] or [[afn]].
For example, this prints numbers ad infinitum:
  (loop (x 1)
    (prn x)
    (recur (+ x 1)))"
  (let w pair.withses
    `((rfn recur ,(map1 car w) ,@body)
        ,@(map1 cadr w))))

(mac point (name . body)
"Like [[do]], but may be exited by calling 'name' from within 'body'."
  (w/uniq (g p)
    `(ccc (fn (,g)
            (let ,name (fn ((o ,p)) (,g ,p))
              ,@body)))))

; Ac expands x:y:z into (compose x y z)
; The last arg (z above) cannot be a macro unless the form is in functional
; position.
;
; Composes in functional position are transformed away by ac.

(mac compose args
"Takes a list of functions and returns a function that behaves as if all its
'args' were called in sequence.
For example, this is always true:
  ((compose f g h) a b c) <=> (f (g (h a b c))).
Be wary of passing macros to compose."
  (w/uniq g
    `(fn ,g
       ,(loop (fs args)
          (if cdr.fs
            (list car.fs (recur cdr.fs))
            `(apply ,(if car.fs car.fs 'idfn) ,g))))))

; Ac expands ~x into (complement x)
; x cannot be a macro unless the form is in functional position.
; Complement in functional position is transformed away by ac, and can handle
; macros.

(def complement (f)
"Returns a function that behaves as if the result of calling 'f' was negated.
For example, this is always true:
  ((complement f) a b) <=> (no (f a b))"
  (fn args (no (apply f args))))

(def rev (xs)
"Returns a list containing the elements of 'xs' back to front."
  (loop (xs xs acc nil)
    (if (no xs)
      acc
      (recur cdr.xs
             (cons car.xs acc)))))

(examples rev
  (rev '(1 (2 3) 4))
  (4 (2 3) 1))

(def isnt (x y) (no (is x y)))

(mac or args
"Stops at the first argument to pass, and returns its result."
  (and args
       (w/uniq g
         `(let ,g ,car.args
            (if ,g ,g
              (or ,@cdr.args))))))

(def alist (x)
"Is 'x' a (possibly empty) list?"
  (or no.x acons.x))

(mac in (x . choices)
"Does 'x' match one of the given 'choices'?"
  (w/uniq g
    `(let ,g ,x
       (or ,@(map1 (fn (c) `(is ,g ,c))
                   choices)))))

(def atom (x)
"Is 'x' a simple type? (i.e. not list, table or user-defined)"
  (in type.x 'int 'num 'sym 'char 'string))

(document builtin is (x y)
"Are 'x' and 'y' identical?")

(def iso (x y)
"Are 'x' and 'y' equal-looking to each other? Non-atoms like lists and tables can contain
the same elements (be *isomorphic*) without being identical."
  (or (is x y)
      (and (acons x)
           (acons y)
           (iso (car x) (car y))
           (iso (cdr x) (cdr y)))))

(document builtin if (test1 then1 test2 then2 ... else)
"Version 1: (if test then) runs 'then' if 'test' passes.
Version 2: (if test then else) runs 'then' or 'else' depending on whether
'test' passes or fails.
Version 3: takes arbitrary numbers of alternating tests and expressions,
running the first expression whose test passes. Optionally might take an
'else' branch to run if none of the tests pass.")

(mac when (test . body)
"Like [[if]], but can take multiple expressions to run when 'test' is not nil.
Can't take an 'else' branch."
  `(if ,test (do ,@body)))

(mac unless (test . body)
"Opposite of [[when]]; runs multiple expressions when 'test' is nil."
  `(if (no ,test) (do ,@body)))

(def reclist (f xs)
"Calls function 'f' with successive [[cdr]]s of 'xs' until one of the calls passes."
  (and xs (or (f xs) (if (acons xs) (reclist f (cdr xs))))))

(examples reclist
  (reclist [caris _ 'b] '(a b c))
  t
  (reclist [caris _ 'd] '(a b c))
  nil
  (reclist [if (is 2 len._) _] '(a b c d))
  (c d))

(def recstring (test s (o start 0))
"Calls function 'test' with successive characters in string 's' until one of the calls passes."
  (loop (i start)
    (and (< i len.s)
         (or test.i
             (recur (+ i 1))))))

(def testify (x)
"Turns an arbitrary value 'x' into a predicate function to compare with 'x'."
  (if (isa x 'fn) x [iso _ x]))

(def carif (x)
"Returns the first element of the given list 'x', or just 'x' if it isn't a list."
  (on-err (fn(_) x)
    (fn() (car x))))

(examples carif
  (carif '(1 2))
  1
  (carif 3)
  3)

(def some (test seq)
"Does at least one element of 'seq' satisfy 'test'?"
  (let f testify.test
    (reclist f:carif seq)))

(defextend some (test seq)  (isa seq 'string)
  (let f testify.test
    (recstring f:seq seq)))

(def all (test seq)
"Does every element of 'seq' satisfy 'test'?"
  (~some (complement (testify test)) seq))

(mac check (x test (o alt))
"Returns `x' if it satisfies `test', otherwise returns 'alt' (nil if it's not provided)."
  (w/uniq gx
    `(let ,gx ,x
       (if (,test ,gx) ,gx ,alt))))

(mac acheck (x test (o alt))
"Like [[check]], but 'alt' can refer to the value of expr 'x' as 'it.
Pronounced 'anaphoric check'."
  `(let it ,x
     (if (,test it)
       it
       ,alt)))

(def find (test seq)
"Returns the first element of 'seq' that satisfies `test'."
  (let f testify.test
    (reclist [check carif._ f] seq)))

(examples find
  (find 3 '(1 2 3 4))
  3
  (find odd '(1 2 3 4))
  1
  (find odd '(2 4 6))
  nil)

(defextend find (test seq) (isa seq 'string)
  (let f testify.test
    (recstring [check seq._ f] seq)))

(def mem (test seq)
"Returns suffix of 'seq' after the first element to satisfy 'test'.
This is the most reliable way to check for presence, even when searching for nil."
  (let f (testify test)
    (reclist [if (f:carif _) _] seq)))

(examples mem
  (mem odd '(2 4 5 6 7))
  (5 6 7)
  (mem 6 '(2 4 5 6 7))
  (6 7))

(def isa (x y)
"Is 'x' of type 'y'?"
  (is (type x) y))

(document builtin coerce (x type)
"Try to turn 'x' into a value of a different 'type'.")

(mac as (type expr)
"Tries to convert 'expr' into a different 'type'.
More convenient form of [[coerce]] with arguments reversed; doesn't need
'type' to be quoted."
  `(coerce ,expr ',type))

(def sym (x)
"Converts 'x' into a symbol."
  (coerce x 'sym))

(def int (x (o b 10))
"Converts 'x' into an integer, optionally in the given base 'b' (decimal by default)."
  (coerce x 'int b))

(def real (x)
"Converts 'x' into a real number."
  ($.exact->inexact x))

(def string args
"Converts all 'args' into strings and concatenates the results."
  (apply + "" (map [coerce _ 'string] args)))

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
"Successively applies corresponding elements of 'seqs' to function 'f'.
Generalizes [[map1]] to functions with more than one argument."
  (if (no cdr.seqs)
        (map1 f car.seqs)
      (all idfn seqs)
        (cons (apply f (map1 car seqs))
              (apply map f (map1 cdr seqs)))))

(defextend map (f . seqs) (some [isa _ 'string] seqs)
  (withs (n  (apply min (map1 len seqs))
          new  (newstring n))
    (loop (i 0)
      (if (is i n)
        new
        (do (sref new (apply f (map1 [_ i] seqs)) i)
            (recur (+ i 1)))))))

(examples map
  (map cdr '((1) (2 3) (4 5)))
  (nil (3) (5))
  (map [list _ (* _ 10)]
       '(1 2 3))
  ((1 10) (2 20) (3 30))
  (map + '(1 2 3) '(4 5 6))
  (5 7 9)
  (map (fn (c n) (coerce (+ n (coerce c 'int)) 'char)) "abc" '(0 2 4))
  "adg"
  (map min "bird" "elephant")
  "bied")

; common uses of map
(def mappend (f . args)
"Like [[map]] followed by append."
  (apply + nil (apply map f args)))

(examples mappend
  (mappend cdr '((1) (2 3) (4 5)))
  (3 5)
  (mappend [list _ (* _ 10)] '(1 2 3))
  (1 10 2 20 3 30))

(def subst (old new seq)
"Returns a copy of 'seq' with all values of 'old' replaced with 'new'."
  (map (fn (_)
         (if (testify.old _)
           (if (isa new 'fn) new._ new)
           _))
       seq))

(def firstn (n xs)
"Returns the first 'n' elements of 'xs'."
  (if (no n)            xs
      (and (> n 0) xs)  (cons (car xs) (firstn (- n 1) (cdr xs)))
                        nil))

(examples firstn
  (firstn 3 '(1 2))
  (1 2)
  (firstn 3 '(a b c d e))
  (a b c))

(def lastn (n xs)
"Returns the last 'n' elements of 'xs'."
  (rev:firstn n rev.xs))

(def nthcdr (n xs)
"Returns all but the first 'n' elements of 'xs'."
  (if (no n)  xs
      (> n 0) (nthcdr (- n 1) (cdr xs))
              xs))

(examples nthcdr
  (nthcdr 0 '(1 2 3))
  (1 2 3)
  (nthcdr 1 '(1 2 3))
  (2 3)
  (nthcdr 2 '(1 2 3))
  (3)
  (nthcdr 10 '(1 2 3))
  nil)

(def lastcons (xs)
"Returns the absolute last link of list 'xs'. Save this value to efficiently
append to 'xs'."
  (if cdr.xs
    (lastcons cdr.xs)
    xs))

; Generalization of pair: (tuples x) = (pair x)

(def tuples (xs (o n 2))
"Splits 'xs' up into lists of size 'n'. Generalization of [[pair]]."
  (if (no xs)
    nil
    (cons (firstn n xs)
          (tuples (nthcdr n xs) n))))

(examples tuples
  (tuples '(1 2 3 4 5) 1)
  ((1) (2) (3) (4) (5))
  (tuples '(1 2 3 4 5) 2)
  ((1 2) (3 4) (5))
  (tuples '(1 2 3 4 5) 3)
  ((1 2 3) (4 5)))

; If ok to do with =, why not with def?  But see if use it.

(mac defs args
  `(do ,@(map [cons 'def _] (tuples args 3))))

(def caris (x val)
  (and (acons x) (is (car x) val)))

(examples caris
  (caris '(1 2) 1)
  t
  (caris 1 1)
  nil)

(def warn (msg . args)
"Displays args to screen as a warning."
  (disp (+ "Warning: " msg ". "))
  (map [do (write _) (disp " ")] args)
  (disp #\newline))

(mac atomic body
"Runs expressions in 'body' with exclusive access to system resources.
Currently invoked for you anytime you modify a variable. This can slow things down, but
prevents multiple threads of execution from stepping on each other's toes by, say,
writing to a variable at the same time."
  `(atomic-invoke (fn () ,@body)))

(mac atlet args
"Like [[let]], but [[atomic]]."
  `(atomic (let ,@args)))

(mac atwith args
"Like [[with]], but [[atomic]]."
  `(atomic (with ,@args)))

(mac atwiths args
"Like [[withs]], but [[atomic]]."
  `(atomic (withs ,@args)))

(document builtin sref (aggregate value . indices)
"Sets position 'indices' in 'aggregate' (which might be a list, string, hash
table, or other user-defined type) to 'value'.")

(examples sref
  (ret x '(1 2 3)
    (sref x 4 1))
  (1 4 3)
  (ret x "abc"
    (sref x #\d 0))
  "dbc"
  (ret x (obj a 1 b 2)
    (sref x 3 'd))
  (valueof (obj a 1 b 2 d 3)))

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

(assign setter (table))

(mac defset (name parms . body)
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
                   `(fn (,h) (assign ,expr ,h)))))
        ; make it also work for uncompressed calls to compose
        (and (acons expr) (metafn (car expr)))
         (setforms (expand-metafn-call (ssexpand (car expr)) (cdr expr)))
        (and (acons expr) (acons (car expr)) (is (caar expr) 'get))
         (setforms (list (cadr expr) (cadr (car expr))))
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
                            `(fn (,h) (sref ,g ,h ,(car argsyms))))))))))))

(def metafn (x)
  (or (ssyntax x)
      (and (acons x) (in (car x) 'compose 'complement))))

(def expand-metafn-call (f args)
  (if (is (car f) 'compose)
       (loop (fs cdr.f)
         (if (caris car.fs 'compose)  ; nested compose
              (recur (join cdar.fs cdr.fs))
             (cdr fs)
              (list car.fs (recur cdr.fs))
             :else
              (cons car.fs args)))
      (is (car f) 'no)
       (err "Can't invert " (cons f args))
      :else
       (cons f args)))

(def expand= (place val)
  (if (and (isa place 'sym) (~ssyntax place))
    `(assign ,place ,val)
    (let (vars prev setter) (setforms place)
      (w/uniq g
        `(atwith ,(+ vars (list g val))
           (,setter ,g))))))

(def expand=list (terms)
  `(do ,@(map (fn ((p v)) (expand= p v))  ; [apply expand= _]
                  (pair terms))))

(mac = args
"(= var val) saves 'val' in 'var'.
(= var1 val1 var2 val2) saves 'val's in corresponding 'var's.
'var's can be complex expressions like (car x), and so on. See [[defset]].
When you run multiple threads at once, only one will ever be modifying a variable at once.
See [[atomic]]."
  (expand=list args))

(examples =
  (= x 1)
  _
  (= x 2 y 4)
  _)

; http://arclanguage.org/item?id=12229
(mac for (var init test update . body)
"Loops through expressions in 'body' as long as 'test' passes, first binding 'var' to 'init'. At the end of each iteration it runs 'update', which usually will modify 'var'.
Can also be terminated from inside 'body' by calling '(break)', or interrupt a single iteration by calling '(continue)'.
If you nest multiple loops with different 'var's like i and j, you can break out of either of them by calling (break-i), (break-j), etc.
Always returns nil.

Incompatibility alert: 'for' is different in anarki from vanilla arc. To get vanilla arc's behavior, use [[up]]. For more information, see CHANGES/for."
  ; simple heuristic to alert on the incompatibility at call time. If you need
  ; to check a flag variable you should probably be using 'while' anyway.
  (unless (acons test)
    (err "`for` has changed in anarki. See (help for) for more information."))
  `(point break
     (let ,(sym:string "break-" var) break
       (loop (,var ,init)
          (when ,test
            (do1 nil
                 (point continue
                   (let ,(sym:string "continue-" var) continue
                     ,@body))
                 ,update
                 ,(if (acons var)
                    `(recur (list ,@var))
                    `(recur ,var))))))))

(mac up (v init max . body)
"Counts 'v' up from 'init' (inclusive) to 'max' (exclusive), running 'body'
with each value. Can also (break) and (continue) inside 'body'; see [[for]]."
  `(for ,v ,init (< ,v ,max) (assign ,v (+ ,v 1))
     ,@body))

(mac down (v init min . body)
"Counts 'v' down from 'init' (inclusive) to 'min' (exclusive), running 'body'
with each value. Can also (break) and (continue) inside 'body'; see [[for]]."
  `(for ,v ,init (> ,v ,min) (assign ,v (- ,v 1))
     ,@body))

(mac repeat (n . body)
"Runs 'body' expression by expression 'n' times."
  (w/uniq gi
    `(up ,gi 0 ,n
       ,@body)))

(mac forlen (var s . body)
"Loops through the length of sequence 's', binding each element to 'var'."
  `(up ,var 0 (len ,s)
     ,@body))

(def walk (seq f)
"Calls function 'f' on each element of 'seq'. See also [[map]]."
  (loop (l seq)
    (when acons.l
      (f car.l)
      (recur cdr.l))))

(mac each (var expr . body)
"Loops through expressions in 'body' with 'var' bound to each successive
element of 'expr'."
  `(walk ,expr (fn (,var) ,@body)))

(defextend walk (seq f) (isa seq 'table)
  (maptable (fn (k v)
              (f (list k v)))
            seq))

(defextend walk (seq f) (isa seq 'string)
  (forlen i seq
    (f seq.i)))

; ; old definition of 'each. possibly faster, but not extendable.
; (mac each (var expr . body)
;   (w/uniq (gseq gf gv)
;     `(let ,gseq ,expr
;        (if (alist ,gseq)
;             ((rfn ,gf (,gv)
;                (when (acons ,gv)
;                  (let ,var (car ,gv) ,@body)
;                  (,gf (cdr ,gv))))
;              ,gseq)
;            (isa ,gseq 'table)
;             (maptable (fn ,var ,@body)
;                       ,gseq)
;             (up ,gv 0 (len ,gseq)
;               (let ,var (,gseq ,gv) ,@body))))))

; Destructuring means ambiguity: are pat vars bound in else? (no)

(mac iflet (var expr . branches)
"If 'expr' is not nil, binds 'var' to it before running the first branch.
Can be given multiple alternating test expressions and branches. The first
passing test expression is bound to 'var' before running its corresponding branch.

For examples, see [[aif]]."
  (if branches
    (w/uniq gv
      `(let ,gv ,expr
         (if ,gv
           (let ,var ,gv
             ,(car branches))
           ,(if (cdr branches)
              `(iflet ,var ,@(cdr branches))))))
    expr))

(mac whenlet (var expr . body)
"Like [[when]] but also puts the value of 'expr' in 'var' so 'body' can access it."
  `(iflet ,var ,expr (do ,@body)))

(mac let-or (var expr else . body)
"Like [[iflet]] but provides an immediate escape hatch first if 'expr' is nil.
Use let-or for [[iflet]] forms with just one test, many things to do if it
passes, and a simple expression or error if it fails."
  `(iflet ,var ,expr
     (do ,@body)
     ,else))

(examples let-or
  (let-or x (+ 3 4)  (err "Error in adding 3 and 4")
    ++.x
    (+ x 3))
  11)

(mac aif (expr . branches)
"Like [[if]], but also puts the value of 'expr' in variable 'it'."
  `(iflet it ,expr ,@branches))

(examples aif
  (aif (> 1 2) (+ it 1)
       42      (+ it 2))
  44
  (let h (obj a 1)
    (aif h!a (+ it 1)))
  2)

(mac awhen (expr . body)
"Like [[when]], but also puts the value of 'expr' in variable 'it'."
  `(let it ,expr (if it (do ,@body))))

(examples awhen
  (awhen (* 2 3)
    (+ it 1))
  7)

(mac aand args
"Like [[and]], but each expression in 'args' can access the result of the
previous one in variable 'it'."
  (if (no args)
       t
      (no (cdr args))
       (car args)
      `(let it ,(car args) (and it (aand ,@(cdr args))))))

(examples aand
  (aand 1 (+ it 2) (* it 10))
  30)


; (nthcdr x y) = (cut y x).

(def cut (seq start (o end))
"Extract a chunk of 'seq' from index 'start' (inclusive) to 'end' (exclusive). 'end'
can be left out or nil to indicate everything from 'start', and can be
negative to count backwards from the end."
  (firstn (- (range-bounce end len.seq)
             start)
          (nthcdr start seq)))

(examples cut
  (cut '(a b c d e) 2)
  (c d e)
  (cut "abcde" 2 4)
  "cd")

(defextend cut (seq start (o end))  (isa seq 'string)
  (let end (range-bounce end len.seq)
    (ret s2 (newstring (- end start))
      (up i 0 (- end start)
        (= s2.i (seq (+ start i)))))))

(def range-bounce (i max)
"Munges index 'i' in slices of a sequence of length 'max'. First element starts
 at index 0. Negative indices count from the end. A nil index denotes the end."
  (if (no i)  max
      (< i 0)  (+ max i)
      (>= i max) max
      :else  i))

(def last (xs)
"Returns the last element of 'xs'."
  (if (cdr xs)
    (last (cdr xs))
    (car xs)))

(examples last
  (last '(1 2 3))
  3)

(def rem (test seq)
"Returns all elements of 'seq' except those satisfying 'test'."
  (let f (testify test)
    (loop (s seq)
      (if (no s)        nil
          (f car.s)     (recur cdr.s)
          :else         (cons car.s (recur cdr.s))))))

(examples rem
  (rem odd '(1 2 3 4 5))
  (2 4)
  (rem 3 '(1 2 3 4 5))
  (1 2 4 5)
  (rem #\d "abcde")
  "abce"
  (rem [in _ #\a #\b] "abcde")
  "cde")

(defextend rem (test seq) (isa seq 'string)
  (as string
      (rem test (as cons seq))))

; Seems like keep doesn't need to testify-- would be better to
; be able to use tables as fns.  But rem does need to, because
; often want to rem a table from a list.  So maybe the right answer
; is to make keep the more primitive, not rem.

(def keep (test seq)
"Returns all elements of 'seq' for which 'test' passes."
  (rem (complement (testify test)) seq))

(examples keep
  (keep odd '(1 2 3 4 5))
  (1 3 5)
  (keep 3 '(1 2 3 4 5))
  (3)
  (keep 3 '(1 3 1 3 1))
  (3 3)
  (keep [in _ #\a #\b] "banana")
  "baaa")

;(def trues (f seq)
;  (rem nil (map f seq)))

(def trues (f xs)
"Returns (map f xs) dropping any nils."
  (and xs
       (iflet fx (f car.xs)
         (cons fx (trues f cdr.xs))
         (trues f cdr.xs))))

(examples trues
  (trues cdr '((1 2) (3) (4 5)))
  ((2) (5)))

; Would like to write a faster case based on table generated by a macro,
; but can't insert objects into expansions in Mzscheme.

(mac caselet (var expr . args)
"Like [[case]], but 'expr' is also bound to 'var' and available inside the 'args'."
  `(let ,var ,expr
     ,(loop (args args)
        (if (no cdr.args)
          car.args
          `(if (is ,var ',car.args)
             ,cadr.args
             ,(recur cddr.args))))))

(mac case (expr . args)
"Usage: (case expr test1 then1 test2 then2 ...)
Matches 'expr' to the first satisfying 'test' and runs the corresponding 'then' branch."
  `(caselet ,(uniq) ,expr ,@args))

(mac push (x place)
"Adds 'x' to the start of the sequence at 'place'."
  (w/uniq gx
    (let (binds val setter) (setforms place)
      `(let ,gx ,x
         (atwiths ,binds
           (,setter (cons ,gx ,val)))))))

(mac swap (place1 place2)
"Exchanges the values of 'place1' and 'place2'."
  (w/uniq (g1 g2)
    (with ((binds1 val1 setter1) (setforms place1)
           (binds2 val2 setter2) (setforms place2))
      `(atwiths ,(+ binds1 (list g1 val1) binds2 (list g2 val2))
         (,setter1 ,g2)
         (,setter2 ,g1)))))

(mac rotate places
"Like [[swap]] but for more than two places.
For example, after (rotate place1 place2 place3), place3 is moved to place2,
place2 to place1, and place1 to place3."
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
"Opposite of [[push]]: removes the first element of the sequence at 'place' and returns it."
  (w/uniq g
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ binds (list g val))
         (do1 (car ,g)
              (,setter (cdr ,g)))))))

(def adjoin (x xs)
  (if (some x xs)
    xs
    (cons x xs)))

(mac pushnew (x place)
"Like [[push]] but first checks if 'x' is already present in 'place'."
  (let (binds val setter) (setforms place)
    `(atwiths ,binds
       (,setter (adjoin ,x ,val)))))

(mac pull (test place)
"Removes all elements from 'place' that satisfy 'test'."
  (let (binds val setter) (setforms place)
    `(atwiths ,binds
       (,setter (rem ,test ,val)))))

(mac togglemem (x place)
  (w/uniq gx
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ (list gx x) binds)
         (,setter (if (mem ,gx ,val)
                    (rem ,gx ,val)
                    (adjoin ,gx ,val)))))))

(mac ++ (place (o i 1))
"Increments 'place' by 'i' (1 by default)."
  (if (isa place 'sym)
    `(= ,place (+ ,place ,i))
    (w/uniq gi
      (let (binds val setter) (setforms place)
        `(atwiths ,(+ binds (list gi i))
           (,setter (+ ,val ,gi)))))))

(mac -- (place (o i 1))
"Decrements 'place' by 'i' (1 by default)."
  (if (isa place 'sym)
    `(= ,place (- ,place ,i))
    (w/uniq gi
      (let (binds val setter) (setforms place)
        `(atwiths ,(+ binds (list gi i))
           (,setter (- ,val ,gi)))))))

; E.g. (++ x) equiv to (zap + x 1)

(mac zap (op place . args)
"Replaces 'place' with (apply op place args)"
  (let (binds val setter) setforms.place
    `(atwiths ,binds
      (,setter (,op ,val ,@args)))))

(mac wipe args
"Sets each place in 'args' to nil."
  `(do ,@(map (fn (a) `(= ,a nil)) args)))

(mac set args
"Sets each place in 'args' to t."
  `(do ,@(map (fn (a) `(= ,a t)) args)))

(mac accum (accfn . body)
"Runs 'body' (usually containing a loop) and then returns in order all the
values that were called with 'accfn' in the process.
Can be cleaner than map for complex anonymous functions."
  (w/uniq gacc
    `(withs (,gacc nil ,accfn [push _ ,gacc])
       ,@body
       (rev ,gacc))))

(examples accum
  (accum accfn (each x '(1 2 3) (accfn (* x 10))))
  (10 20 30)
  (accum yield (each x "abcd" (yield x)))
  (#\a #\b #\c #\d))

(mac whilet (var test . body)
"Like [[while]], but successive values of 'test' are bound to 'var'."
  `(point break
     (loop (,var ,test)
       (when ,var
         (point continue
           ,@body)
         (recur ,test)))))

(mac while (test . body)
"Loops through the expressions in 'body' as long as 'test' passes.
Can also terminate by calling '(break)', or terminate just one iteration by calling
'(continue)'."
  (w/uniq gp
    `(whilet ,gp ,test
       ,@body)))

(mac forever body
"Loops through the expressions in 'body' forever.
May still terminate by calling '(break)'."
  `(while t ,@body))

; For the common C idiom while x = snarfdata != end.
; Rename this if use it often.

(mac whiler (var expr end . body)
"Repeatedly binds 'var' to 'expr' and runs 'body' until 'var' matches 'end'."
  (w/uniq gendf
    `(withs (,var nil ,gendf (testify ,end))
       (while (no (,gendf (= ,var ,expr)))
         ,@body))))

; Repeatedly evaluates its body till it returns eos (end of stream being
; drained), then returns vals.

(mac drain (expr (o eos nil))
"Repeatedly evaluates 'expr' until it returns 'eos' (nil by default). Returns
a list of the results."
  (w/uniq (gacc gres)
    `(accum ,gacc
       (whiler ,gres ,expr ,eos
         (,gacc ,gres)))))

;(def macex (e)
;  (if (atom e)
;    e
;    (let op (and (atom (car e)) (eval (car e)))
;      (if (isa op 'mac)
;        (apply (rep op) (cdr e))
;        e))))

(def consif (x xs)
"Like [[cons]] on 'x' and 'xs' unless 'x' is nil."
  (if x (cons x xs) xs))

(examples consif
  (consif 1 '(2 3))
  (1 2 3)
  (consif nil '(2 3))
  (2 3))

(def flat x
"Flattens a list of lists."
  (loop (x x  acc nil)
    (if no.x        acc
        (~acons x)  (cons x acc)
        :else       (recur car.x (recur cdr.x acc)))))

(examples flat
  (flat '(1 2 () 3 (4 (5))))
  (1 2 3 4 5))

(def pos (test seq (o start 0))
"Returns the index of the first element of 'seq' matching 'test', starting
from index 'start' (0 by default)."
  (let f testify.test
    (loop (seq (nthcdr start seq)
           n   start)
      (if (no seq)
           nil
          (f car.seq)
           n
          (recur cdr.seq (+ n 1))))))

(defextend pos (test seq (o start 0))  (isa seq 'string)
  (let f testify.test
    (recstring [if (f (seq _)) _] seq start)))

(examples pos
  (pos 'c '(a b c d))
  2
  (pos 'x '(a b c d))
  nil
  (pos #\b "abcba")
  1
  (pos #\b "abcba" 2)
  3
  (pos odd '(2 4 5 6 7))
  2)

(def even (n)
"Is n even?"
  (is (mod n 2) 0))

(def odd (n)
"Is n odd?"
  (no (even n)))

(mac after (x . ys)
"Runs all 'ys' after 'x', even if 'x' throws an error.
Returns result of 'ys' on success, and nothing on error."
  `(protect (fn () ,x) (fn () ,@ys)))

(let expander
     (fn (f var name body)
       `(let ,var (,f ,name)
          (after (do ,@body) (close ,var))))

  (mac w/infile (var name . body)
  "Opens file 'name' into stream 'var' to [[read]] from it in 'body'.
Reliably closes the file when it's done.
See also [[read]]."
    (expander 'infile var name body))

  (mac w/outfile (var name . body)
  "Opens file 'name' into stream 'var' to [[write]] to it in 'body'.
Reliably closes the file when it's done."
    (expander 'outfile var name body))

  (mac w/instring (var str . body)
  "Creates a stream 'var' to [[read]] from 'str' in 'body'.
Reliably closes the stream when it's done."
    (expander 'instring var str body))

  (mac w/socket (var port . body)
  "Creates a stream 'var' to listen to and [[read]] from 'port'.
Reliably closes the stream when it's done."
    (expander 'open-socket var port body))
  )

(mac w/outstring (var . body)
"Create an in-memory string and [[write]] to it in 'body'.
The contents of the string can be accessed by calling 'inside'."
  `(let ,var (outstring) ,@body))

; what happens to a file opened for append if arc is killed in
; the middle of a write?

(mac w/appendfile (var name . body)
"Opens file 'name' into stream 'var' to [[write]] to it in 'body'.
Unlike [[w/outfile]], appends to existing contents of the file.
Reliably closes the file when it's done."
  `(let ,var (outfile ,name 'append)
     (after (do ,@body) (close ,var))))

; rename this simply "to"?  - prob not; rarely use

(mac w/stdout (str . body)
"Redirects writes to (stdout) inside 'body' using calls to [[write]], [[prn]],
etc. to write to the stream 'str'."
  `(call-w/stdout ,str (fn () ,@body)))

(mac w/stdin (str . body)
"Redirects reads from (stdin) inside 'body' using calls to [[read]], etc. to
read from the stream 'str'."
  `(call-w/stdin ,str (fn () ,@body)))

(mac tostring body
"Runs 'body' then collect all output to (stdout) and return it as a string."
  (w/uniq gv
   `(w/outstring ,gv
      (w/stdout ,gv ,@body)
      (inside ,gv))))

(mac fromstring (str . body)
"Runs 'body', reading from 'str' as stdin."
  (w/uniq gv
   `(w/instring ,gv ,str
      (w/stdin ,gv ,@body))))

(mac pipe-to (dest . body)
"Redirects stdout for 'body' into stdin of 'dest'."
  `(fromstring
     (tostring ,@body)
     ,dest))

(def readstring1 (s (o eof nil))
"Reads a single expression from string 's'. Returns 'eof' if there's nothing to read.
Caller is responsible for picking an unambiguous 'eof' indicator."
  (w/instring i s (read i eof)))

(def read ((o x (stdin)) (o eof nil))
"Reads a single expression from string or stream 'x'. Returns 'eof' if there's
nothing to read. Caller is responsible for picking an unambiguous 'eof' indicator."
  (if (isa x 'string)
    (readstring1 x eof)
    (sread x eof)))

; encapsulate eof management
(def ifread-fn (port then else)
  (withs (eof list.nil          ; a unique value
          val (read port eof))
   (if (is eof val)
     (else)
     then.val)))

(mac ifread (var port then (o else))
  `(ifread-fn ,port (fn (,var) ,then) (fn () ,else)))

(mac reading (var port . body)
  `(ifread ,var ,port (do ,@body)))

(mac fromfile (f . body)
"Redirects standard input from the file 'f' within 'body'."
  (w/uniq gf
    `(w/infile ,gf ,f
       (w/stdin ,gf
         ,@body))))

(mac tofile (f . body)
"Redirects stdout to the file 'f' within 'body'."
  (w/uniq (gf gs)
    `(let ,gs (mktemp:basename ,f)
       (w/outfile ,gf ,gs
         (w/stdout ,gf
           ,@body))
       (mvfile ,gs ,f))))

(mac ontofile (f . body)
  (w/uniq gf
    `(w/appendfile ,gf ,f
       (w/stdout ,gf
         ,@body))))

(def readfile (name)
"Slurps the entire contents of file 'name' using [[read]] and returns the
expressions  as a list."
  (fromfile name
    (drain:read)))

(def readfile1 (name)
"Returns the first expression [[read]] from file 'name'."
  (fromfile name
    (read)))

(def writefile (val name)
"Outputs 'val' to file 'name' using [[write]]."
  (tofile name
    (write val)))

(def readall (src (o eof nil))
"Like [[readfile]], but can also accept a string 'src'."
  (loop (in (if (isa src 'string)
              instring.src
              src))
    (let x (read in eof)
      (if (is x eof)
        nil
        (cons x recur.in)))))

(def allchars (str)
  (tostring (whiler c (readc str nil) no
              (writec c))))

(def filechars (name)
  (w/infile s name (allchars s)))

; Can't simply mod pr to print strings represented as lists of chars,
; because empty string will get printed as nil.  Would need to rep strings
; as lists of chars annotated with 'string, and modify car and cdr to get
; the rep of these.  That would also require hacking the reader.

(def pr args
"Prints all its 'args' to screen. Returns the first arg."
  (map1 disp args)
  (car args))

(def prt args
"Like [[pr]], but doesn't print nils."
  (map1 only.pr args)
  (car args))

(def prn args
"Prints all its 'args' to screen followed by a newline. Returns the first arg."
  (do1 (apply pr args)
       (pr #\newline))) ; writec doesn't implicitly flush

(def prrn args  ; print with \r\n at the end
"Like [[prn]], but prints both carriage return and newline, usually because the HTTP
protocol requires them."
  (do1 (apply pr args)
       (pr #\return #\newline)))

(def ero args
"Like [[prn]] but prints to stderr rather than stdout."
  (w/stdout (stderr)
    (apply prn args)))

(= ac-denil       ($ ac-denil))
(= ac-global-name ($ ac-global-name))
(= ac-niltree     ($ ac-niltree))

; for when we can't use assign

(mac ac-set-global (name val)
  (w/uniq (gname v)
    `(with (,gname (ac-global-name ,name)
            ,v ,val)
       ($ (namespace-set-variable-value! ,gname ,v))
       nil)))

(= scheme-f (read "#f"))
(= scheme-t (read "#t"))

(= defined-variables* (table))

(redef ac-defined-var? (name)
  (if defined-variables*.name scheme-t scheme-f))

(mac defvar (name impl)
  `(do (ac-set-global ',name ,impl)
       (set (defined-variables* ',name))
       nil))

(mac defvar-impl (name)
  (let gname (ac-global-name name)
    `($ ,gname)))

(mac undefvar (name)
  `(do (wipe (defined-variables* ',name))
       (ac-set-global ',name nil)))

(mac parameterize (var val . body)
  (w/uniq f
    `(let ,f (fn () ,@body)
       (parameterize-sub ,var ,val ,f))))

(def thread-cell (var (o inherit))
  ($:make-thread-cell ,var ,(if inherit scheme-t scheme-f)))

(mac thread-local (name val)
  (w/uniq storage
    `(defvar ,name
       (let ,storage (thread-cell ,val)
         (fn args
           (if args
             (ac-niltree:$:thread-cell-set! ,storage (car args))
             (ac-niltree:$:thread-cell-ref ,storage)))))))

(mac rand-choice exprs
"Runs one of the given 'exprs' at random and returns the result."
  `(case (rand ,(len exprs))
     ,@(let key -1
         (mappend [list (++ key) _]
                  exprs))))

(mac n-of (n expr)
"Runs 'expr' 'n' times, and returns a list of the results."
  (w/uniq ga
    `(let ,ga nil
       (repeat ,n (push ,expr ,ga))
       (rev ,ga))))

(examples n-of
  (n-of 5 "a")
  ("a" "a" "a" "a" "a")
  (w/instring ins "abcdefg"
    (n-of 5 readc.ins))
  (#\a #\b #\c #\d #\e))

; rejects bytes >= 248 lest digits be overrepresented

(def rand-string (n)
"Generates a random string of letters and numbers."
  (let c "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    (with (nc 62 s (newstring n) i 0)
      (w/infile str "/dev/urandom"
        (while (< i n)
          (let x (readb str)
             (unless (> x 247)
               (= (s i) (c (mod x nc)))
               (++ i)))))
      s)))

(def basename (s)
  (last:tokens s #\/))

(mac on (var s . body)
"Like [[each]], but also maintains a variable calles 'index' counting the iterations."
  (if (is var 'index)
    (err "Can't use index as first arg to on.")
    (w/uniq gs
      `(let ,gs ,s
         (forlen index ,gs
           (let ,var (,gs index)
             ,@body))))))

(def best (f seq)
"Maximizes comparator function 'f' throughout seq."
  (whenlet wins carif.seq
    (each elt cdr.seq
      (if (f elt wins)
        (= wins elt)))
    wins))

(examples best
  (best > '(3 1 4 5 9 6))
  9)

(def max args
"Returns the greatest of 'args'."
  (best > args))
(def min args
"Returns the least of 'args'."
  (best < args))

; (mac max2 (x y)
;   (w/uniq (a b)
;     `(with (,a ,x ,b ,y) (if (> ,a ,b) ,a ,b))))

(def most (f seq)
"Like [[best]], but function 'f' is a scorer for each element rather than a
comparator between elements."
  (if seq
    (withs (wins (car seq) topscore (f wins))
      (each elt (cdr seq)
        (let score (f elt)
          (if (> score topscore) (= wins elt topscore score))))
      wins)))

(examples most
  (most len '("cat" "bird" "dog"))
  "bird"
  (most abs '(3 -10 5))
  -10)

; Insert so that list remains sorted.  Don't really want to expose
; these but seem to have to because can't include a fn obj in a
; macroexpansion.

(def insert-sorted (test elt seq)
"Inserts 'elt' into a sequence 'seq' that is assumed to be sorted by 'test'."
  (if (no seq)
       (list elt)
      (test elt car.seq)
       (cons elt seq)
      :else
       (cons car.seq (insert-sorted test elt cdr.seq))))

(examples insert-sorted
  (insert-sorted > 5 '(10 3 1))
  (10 5 3 1)
  (insert-sorted > 5 '(10 5 1))
  (10 5 5 1))

(mac insort (test elt seq)
"Like [[insert-sorted]] but modifies 'seq' in place'."
  `(zap [insert-sorted ,test ,elt _] ,seq))

(def reinsert-sorted (test elt seq)
  (if (no seq)
       (list elt)
      (is elt car.seq)
       (reinsert-sorted test elt cdr.seq)
      (test elt car.seq)
       (cons elt (rem elt seq))
      :else
       (cons car.seq (reinsert-sorted test elt cdr.seq))))

(mac insortnew (test elt seq)
"Like [[insort]], but only inserts 'elt' if it doesn't exist."
  `(zap [reinsert-sorted ,test ,elt _] ,seq))

(examples insortnew
  (ret x '(10 3 1) (insortnew > 5 x))
  (10 5 3 1)
  (ret x '(10 5 1) (insortnew > 5 x))
  (10 5 1))

; Could make this look at the sig of f and return a fn that took the
; right no of args and didn't have to call apply (or list if 1 arg).

(def memo (f)
"Turns function 'f' into a _memoized_ version that also stores results returned
by args passed in, so that future calls with the same inputs can save work."
  (with (cache (table) nilcache (table))
    (fn args
      (or (cache args)
          (and (no (nilcache args))
               (aif (apply f args)
                    (= (cache args) it)
                    (do (set (nilcache args))
                        nil)))))))


(mac defmemo (name parms . body)
"Like [[def]] but defines a memoized function. See [[memo]]."
  `(do (warn-if-bound ,name)
       (document defmemo ,name ,parms
                   ,@(if (is (type car.body) 'string)
                       body
                       (cons nil body)))
       (assign ,name (memo (fn ,parms ,@body)))))

(def <= args
"Is each element of 'args' lesser than or equal to all following elements?"
  (or (no args)
      (no (cdr args))
      (and (no (> (car args) (cadr args)))
           (apply <= (cdr args)))))

(def >= args
"Is each element of 'args' greater than or equal to all following elements?"
  (or (no args)
      (no (cdr args))
      (and (no (< (car args) (cadr args)))
           (apply >= (cdr args)))))

(def whitec (c)
"Is 'c' a whitespace char?"
  ($.char-whitespace? c))

(def nonwhite (c)
"Is 'c' a non-whitespace char?"
  (~whitec c))

(def letter (c)
"Is 'c' a letter?"
  ($.char-alphabetic? c))

(def digit (c)
"Is 'c' a digit?"
  ($.char-numeric? c))

(def alphadig (c)
"Is 'c' a latter or a digit?"
  (or letter.c digit.c))

(def punc (c)
"Is 'c' a punctuation char?"
  ($.char-punctuation? c))

; a version of readline that accepts both lf and crlf endings
; adapted from Andrew Wilcox's code (http://awwx.ws/readline) by Michael
; Arntzenius <daekharel@gmail.com>

(def readline ((o str (stdin)))
"Reads a string terminated by a newline from the stream 'str'."
  (awhen readc.str
    (tostring
      (loop (c it)
        (if (is c #\return)
             (if (is peekc.str #\newline)
               readc.str)
            (is c #\newline)
             nil
            :else
             (do writec.c
                 (aif readc.str recur.it)))))))

(def readlines ((o str (stdin)))
"Slurps contents of stream 'str' as a list of lines."
  (drain:readline str))

(def sum (f xs)
"Returns total of all elements in (map f xs)."
  (ret n 0
    (each x xs
      (++ n f.x))))

(examples sum
  (sum idfn '(1 2 3 4))
  10
  (sum len '("this" "is" "a" "sentence"))
  15
  (sum cadr (obj a 1 b 2 c 3))
  6
  (sum int "abc")
  294) ; 97 + 98 + 99

; Could prob be generalized beyond printing.

(def prall (elts (o init "") (o sep ", "))
"Prints elements of list 'elts' prefixed with 'init' and separated by 'sep'.
Returns 'elts'."
  (when elts
    (pr init car.elts)
    (each e cdr.elts
      (pr sep e))
    elts))

(def prs args
"Prints elements of list 'args' separated by spaces."
  (prall args "" #\space))

(def dotted (x)
"Is 'x' an _improper_ list terminating in something other than nil?
Name comes from (cons 1 2) being printed with a dot: (1 . 1)."
  (aand acons.x
        cdr.x
        ((orf ~acons dotted) it)))

(def fill-table (table data)
"Populates 'table' with alternating keys and values in 'data'."
  (do1 table
    (each (k v) pair.data
      (= table.k v))))

(def keys (h)
"Returns list of keys in table 'h'."
  (accum a (each (k v) h (a k))))

(def vals (h)
"Returns list of values in table 'h'."
  (accum a (each (k v) h (a v))))

(def tablist (h)
"Converts table 'h' into an association list of (key value) pairs. Reverse of
[[listtab]]."
  (accum a (maptable (fn args (a args)) h)))

(def listtab (al)
"Converts association list 'al' of (key value) pairs into a table. Reverse of
[[tablist]]."
  (let h (table)
    (map (fn ((k v)) (= (h k) v))
         al)
    h))

(mac obj args
"Creates a table out of a list of alternating keys and values."
  `(listtab (list ,@(map (fn ((k v))
                           `(list ',k ,v))
                         (pair args)))))

(def load-table (file (o eof))
"Reads an association list from 'file' and turns it into a table."
  (w/infile i file (read-table i eof)))

(def read-table ((o i (stdin)) (o eof))
"Reads an association list from a stream 'i' (stdin by default) and turns it
into a table."
  (let e (read i eof)
    (if (alist e) (listtab e) e)))

(def load-tables (file)
"Reads multiple association lists from 'file' and returns a corresponding list
of tables."
  (w/infile i file
    (w/uniq eof
      (drain (read-table i eof) eof))))

(def save-table (h file)
"Writes table 'h' to 'file'."
  (tofile file
    (write-table h)))

(def write-table (h (o o (stdout)))
"Writes table as an association list to stream 'o' (stdout by default)."
  (write tablist.h o))

; Optional predicate-based typing

(= type-predicates* (table))

(mac def-isa (name . body)
"Declares a new predicate-based type that can be checked with 'isa'."
  `(= (type-predicates* ',name) (fn (_) ,@body)))

(defextend isa (x y) (~is type.x y)
  (aand (type-predicates* y)
        (it x)))

(def copylist (xs)
  (if acons.xs
    (cons car.xs
          (copylist cdr.xs))
    xs))

(def copy (x)
"Creates a deep copy of 'x'. Future changes to any part of 'x' are guaranteed
to be isolated from the copy."
  (if (atom x)
    x
    (cons (copy car.x)
          (copy cdr.x))))

(defextend copy (x) (isa x 'string)
  (ret new (newstring len.x)
    (forlen i x
      (= new.i x.i))))

(defextend copy (x . args) (isa x 'table)
  (ret new (table)
    (each (k v) x
      (= new.k copy.v))
    (each (k v) pair.args
      (= new.k v))))

(document builtin shl (n m)
"Shifts the binary twos-complement representation of 'n' left by 'm' bits.")

(def shr (n m)
"Shifts the binary twos-complement representation of 'n' right by 'm' bits."
  (shl n (- m)))

(def abs (n)
"Returns the absolute value of 'n'."
  (if (< n 0) (- n) n))

; The problem with returning a list instead of multiple values is that
; you can't act as if the fn didn't return multiple vals in cases where
; you only want the first.  Not a big problem.

; todo:
;   bug: round(0.5) vs round(1.5)
;   use racket's round
;   single round variant

(def round (n)
"Approximates a fractional value to the nearest integer.
Exact halves are rounded down to the lower integer.
Negative numbers are always treated exactly like their positive variants
barring the sign."
  (withs (base (trunc n) rem (abs (- n base)))
    (if (> rem 1/2) ((if (> n 0) + -) base 1)
        (< rem 1/2) base
        (odd base)  ((if (> n 0) + -) base 1)
                    base)))

(def roundup (n)
"Like [[round]] but halves are rounded up rather than down."
  (withs (base (trunc n) rem (abs (- n base)))
    (if (>= rem 1/2)
      ((if (> n 0) + -) base 1)
      base)))

(def nearest (n quantum)
"Like [[round]] but generalized to arbitrary units."
  (* (roundup (/ n quantum)) quantum))

(def avg (ns)
"Returns the arithmetic mean of a list of numbers 'ns'."
  (/ (apply + ns) len.ns))

(def med (ns (o test >))
"Returns the median of a list of numbers 'ns' according to the comparison 'test'."
  ((sort test ns) (round (/ len.ns 2))))

; Use mergesort on assumption that mostly sorting mostly sorted lists
; benchmark: (let td (n-of 10000 (rand 100)) (time (sort < td)) 1)

(def sort (test seq)
"Orders a list 'seq' by comparing its elements using 'test'."
  (if (alist seq)
    (mergesort test (copylist seq))
    (coerce (mergesort test (coerce seq 'cons)) (type seq))))

(examples sort
  (sort < '(3 0 10 -7))
  (-7 0 3 10)
  (sort (fn (a b) (< len.a len.b))
        '("horse" "dog" "elephant" "cat"))
  ("dog" "cat" "horse" "elephant")
  (sort > "Test word")
  "wtsroedT ")

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
        (loop (n n)
           (if (> n 2)
                ; needs to evaluate L->R
                (withs (j (/ (if (even n) n (- n 1)) 2) ; faster than round
                        a (recur j)
                        b (recur (- n j)))
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
               nil)))))

; Also by Eli.

(def merge (less? x y)
  (if (no x) y
      (no y) x
      (let lup nil
        (assign lup
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
"Returns a list of the top 'n' elements of 'seq' ordered by 'f'."
  (firstn n (sort f seq)))

(examples bestn
  (bestn 3 > '(3 1 4 5 9 6))
  (9 6 5)
  (bestn 3 < '(3 1 4 5 9 6))
  (1 3 4))

(def split (seq pos)
"Partitions 'seq' at index 'pos'."
  (list (cut seq 0 pos) (cut seq pos)))

(examples split
  (split '(a b c) 0)
  (nil (a b c))
  (split '(a b c) 1)
  ((a) (b c))
  (split '(a b c) 2)
  ((a b) (c))
  (split '(a b c) 3)
  ((a b c) nil)
  (split '(a b c) 4)
  ((a b c) nil))

(def split-at (s delim)
"Partitions string s at first instance of delimiter, dropping delimiter."
  (iflet i (posmatch delim s)
    (list (cut s 0 i)
          (cut s (+ i len.delim)))
    (list s)))

(defextend split-at (s delim)  (isa s 'cons)
  (iflet i (pos delim s)
    (list (cut s 0 i)
          (cut s (+ i 1)))))

(def strip-after (s delim)
  (car:split-at s delim))

(mac time (expr)
"Runs 'expr', then prints the amount of time it took to do so."
  (w/uniq (t1 t2)
    `(let ,t1 (msec)
       (do1 ,expr
            (let ,t2 (msec)
              (ero "time: " (- ,t2 ,t1) " msec."))))))

(mac jtime (expr)
"Like [[time]] but always returns 'ok'."
  `(do1 'ok (time ,expr)))

(mac time10 (expr)
"Like [[time]] but runs 'expr' 10 times."
  `(time:repeat 10 ,expr))

(def union (f xs ys)
"Merges 'xs' and 'ys', while filtering out duplicates using 'f'. Ordering is
not preserved."
  (+ xs (rem (fn (y) (some [f _ y] xs))
             ys)))

(examples union
  (union is '(1 2 3) '(2 3 4))
  (1 2 3 4)
  (union is "ab" "banana")
  "abnn"
  (union (fn (a b) (is (mod a 10)
                       (mod b 10)))
         '(1 2 3) '(13 24 35))
  (1 2 3 24 35))

(def number (n)
"Is 'n' a number?"
  (in type.n 'int 'num))

(def since (t1) (- (seconds) t1))

(def minutes-since (t1) (/ (since t1) 60))
(def hours-since (t1)   (/ (since t1) 3600))
(def days-since (t1)    (/ (since t1) 86400))

; could use a version for fns of 1 arg at least

(def cache (timef valf)
"Converts a function 'valf' into a version that saves and reuses the results
of calls for a certain time. For greater configurability the caching time is
determined by calling 'timef' rather than directly passing in a number."
  (with (cached nil gentime nil)
    (fn ()
      (unless (and cached (< (since gentime) (timef)))
        (= cached  (valf)
           gentime (seconds)))
      cached)))

(mac defcache (name lasts . body)
  `(do (warn-if-bound ,name)
       (document defcache ,name (,lasts)
                   ,@(if (is (type car.body) 'string)
                        body
                        (cons nil body)))
       (assign ,name (cache (fn () ,lasts)
                            (fn () ,@body)))))

(mac errsafe (expr)
"Runs 'expr' and returns the result, or nil if there were any errors."
  `(on-err (fn (c) nil)
           (fn () ,expr)))

(def safe-load-table (filename)
"Loads a table from 'filename', or an empty table on any errors."
  (on-err (fn (c) (table))
          (fn () (load-table filename))))

(def ensure-dir (path)
"Creates the directory 'path' if it doesn't exist."
  (unless (dir-exists path)
    (system (string "mkdir -p " path))))

(def date ((o s (seconds)))
"Converts time in seconds-since-epoch (now by default) into a list '(year month date)."
  (rev (nthcdr 3 (timedate s))))

(def datestring ((o s (seconds)))
"Converts time in seconds-since-epoch (now by default) into a string \"YYYY-MM-DD\"."
  (let (y m d) (date s)
    (string y "-" (if (< m 10) "0") m "-" (if (< d 10) "0") d)))

(def count (test x)
"Returns the number of elements of 'x' that pass 'test'."
  (with (n 0 testf testify.test)
    (each elt x
      (if testf.elt ++.n))
    n))

(examples count
  (count #\a "banana")
  3
  (count odd '(1 2 3 4))
  2
  (count odd:cadr (obj a 1 b 2))
  1)

(def ellipsize (str (o limit 80))
"Trim string 'str' and append ellipses '...' if its length exceeds 'limit'."
  (if (<= (len str) limit)
    str
    (+ (cut str 0 limit) "...")))

(def rand-elt (seq)
"Returns a random element of 'seq'. See also [[rand-choice]]."
  (seq (rand (len seq))))

(mac until (test . body)
"Like [[while]], but negates 'test'; loops through 'body' as long as 'test' fails."
  `(while (no ,test) ,@body))

(def orf fns
"Returns a function which calls all the functions in 'fns' on its args, and
[[or]]s the results. ((orf f g) x y) <=> (or (f x y) (g x y))"
  (fn args
    (loop (fs fns)
      (and fs
           (or (apply car.fs args)
               (recur cdr.fs))))))

(def andf fns
"Returns a function which calls all the functions in 'fns' on its args, and
[[and]]s the results. For example, ((andf f g) x y) <=> (and (f x y) (g x y)).
Simple syntax: f&g <=> (andf f g)"
  (fn args
    (loop (fs fns)
      (if no.fs          t
          (no cdr.fs)    (apply car.fs args)
          :else          (and (apply car.fs args)
                              (recur cdr.fs))))))

(def before (x y seq (o i 0))
"Does 'x' lie before 'y' in 'seq' (optionally starting from index 'i')?"
  (aand (pos (orf testify.x testify.y) seq i)
        (iso x seq.it)))

(examples before
  (before 2 3 '(1 2 3 4))
  t
  (before 2 1 '(1 2 3 4))
  nil
  (before 1 even '(1 2 3 4))
  t
  (before #\a #\n "banana")
  t
  (before #\a #\n "banana" 2)
  nil)

(def atend (i s)
"Is index 'i' at or past the end of sequence 's'?"
  (>= i (- len.s 1)))

(def multiple (x y)
"Is 'x' a multiple of 'y'?"
  (is 0 (mod x y)))

(mac nor args
"Computes args until one of them passes, then returns nil.
Returns t if none of the args passes."
  `(no (or ,@args)))
(mac nand args
"Computes args until one of them fails, then returns t.
Returns nil if none of the args fails."
  `(no (and ,@args)))

; Consider making the default sort fn take compare's two args (when do
; you ever have to sort mere lists of numbers?) and rename current sort
; as prim-sort or something.

; Could simply modify e.g. > so that (> len) returned the same thing
; as (compare > len).

(def compare (comparer scorer)
"Creates a function to score two args using 'scorer' and compare them using
'comparer'. Often passed to [[sort]]."
  (fn (x y) (comparer scorer.x scorer.y)))

(examples compare
  ((compare < len) "yz" "abc")
  t
  ((compare < len) '(1 2 3) '(4 5))
  nil)

; Cleaner thus, but may only ever need in 2 arg case.

;(def compare (comparer scorer)
;  (fn args (apply comparer map scorer args)))

; (def only (f g . args) (aif (apply g args) (f it)))

(def only (f)
"Transforms a function 'f' info a variant that runs only if its first arg is
non-nil."
  (fn args (if (car args) (apply f args))))

(examples only
  (only.+ 1 2 3)
  6
  (only.+ nil 1 2 3)
  nil
  (only.+)
  nil)

(mac conswhen (f x y)
"Adds 'x' to the front of 'y' if 'x' satisfies test 'f'."
  (w/uniq (gf gx)
   `(with (,gf ,f  ,gx ,x)
      (if (,gf ,gx) (cons ,gx ,y) ,y))))

(examples conswhen
  (conswhen [< _ 3] 2 '(3 4))
  (2 3 4)
  (conswhen [< _ 3] 4 '(5 6))
  (5 6))

; Could combine with firstn if put f arg last, default to (fn (x) t).

(def retrieve (n f xs)
"Returns the first 'n' elements of 'xs' that satisfy 'f'."
  (if (no n)                 (keep f xs)
      (or no.xs (<= n 0))    nil
      (f car.xs)             (cons car.xs (retrieve (- n 1) f cdr.xs))
                             (retrieve n f cdr.xs)))

(examples retrieve
  (retrieve 3 odd '(1 2 3 4 5 6 7 8))
  (1 3 5)
  (retrieve 3 odd '(2 4 6 8))
  nil)

(def dedup (xs)
"Returns list of elements in 'xs' with duplicates dropped."
  (let h (table)
    (accum yield
      (each x xs
        (unless h.x
          (yield x)
          (set h.x))))))

(examples dedup
  (dedup '(1 2 3 2 1))
  (1 2 3)
  (dedup "abcba")
  (#\a #\b #\c))

(def single (x)
"Is 'x' a list with just one element?"
  (and acons.x (no cdr.x)))

(examples single
  (single 1)
  nil
  (single '())
  nil
  (single '(1))
  t
  (single '(1 2))
  nil)

(def intersperse (x ys)
"Inserts 'x' between the elements of 'ys'."
  (and ys (cons (car ys)
                (mappend [list x _] (cdr ys)))))

(examples intersperse
  (intersperse 1 '(a b (c d) e))
  (a 1 b 1 (c d) 1 e)
  (intersperse nil '(1 2 3))
  (1 nil 2 nil 3))

(def counts (seq)
"Returns a table with counts of each unique element in 'seq'."
  (ret ans (table)
    (each x seq
      (++ (ans x 0)))))

(examples counts
  (counts '(b a n a n a))
  (valueof (obj b 1 a 3 n 2)))

(def commonest (seq)
"Returns the most common element of 'seq' and the number of times it occurred
in 'seq'."
  (best (compare > counts.seq) seq))

(examples commonest
  (commonest '(b a n a n a))
  a
  (commonest nil)
  nil)

(def sort-by-commonest (seq (o f idfn))
"Reorders 'seq' with most common elements first."
  (let h (counts:map f seq)
    (sort (compare > h:f) seq)))

(def reduce (f xs)
"Accumulates elements of 'xs' using binary function 'f'."
  (if (cddr xs)
    (reduce f (cons (f car.xs cadr.xs)
                    cddr.xs))
    (apply f xs)))

(examples reduce
  (reduce + '(1 2 3 4 5))
  15
  (reduce + '("a" "b" "c"))
  "abc"
  (reduce / '(1 2 3))
  1/6)

(def rreduce (f xs)
"Like [[reduce]] but accumulates elements of 'xs' in reverse order."
  (if (cddr xs)
    (f (car xs) (rreduce f (cdr xs)))
    (apply f xs)))

(examples rreduce
  (rreduce + '(1 2 3 4 5))
  15
  (rreduce / '(1 2 3))
  3/2)

(def parse-format (str args)
  (accum yield
    (with (chars nil  i -1)
      (w/instring s str
        (whilet c readc.s
          (case c
            #\# (do (yield (coerce rev.chars 'string))
                    (wipe chars)
                    (yield read.s))
            #\~ (do (yield (coerce rev.chars 'string))
                    (wipe chars)
                    (yield car.args)
                    (zap cdr args))
                (push c chars))))
       (when chars
         (yield (coerce rev.chars 'string))))))

(mac prf (str . args)
"Prints 'str' interpolating #exprs and replacing instances of ~ with
successive elements of 'args'."
  `(pr ,@(parse-format str args)))

(examples prf
  (let x 3
    (prf "the square of #x is ~." 9))
  _)  ; should print "the square of 3 is 9."

(wipe load-file-stack*)
(def load (file)
"Successively reads and runs all expressions in 'file'."
  (push current-load-file* load-file-stack*)
  (= current-load-file* file)
  (after (w/infile f file
           (w/uniq eof
             (whiler e (sread f eof) eof
               (eval e))))
    (= current-load-file* (pop load-file-stack*))))

(def positive (x)
  (and (number x) (> x 0)))

(mac w/table (var . body)
"Runs 'body' to add to table 'var' and finally return it."
  `(let ,var (table) ,@body ,var))

(mac noisy-each (n var val . body)
"Like [[each]] but print a progress indicator every 'n' iterations."
  (w/uniq (gn gc)
    `(with (,gn ,n ,gc 0)
       (each ,var ,val
         (when (multiple (++ ,gc) ,gn)
           (pr ".")
           (flushout)
           )
         ,@body)
       (prn)
       (flushout))))

(mac catch body
"Runs 'body', but any call to (throw x) immediately returns x."
  `(point throw ,@body))

(def downcase (s)
"Converts 'x' to lowercase."
  ($.string-downcase s))
(defextend downcase (c)  (isa c 'char)
  ($.char-downcase c))
(defextend downcase (s)  (isa s 'sym)
  (sym:downcase:string s))

(def upcase (s)
"Converts 'x' to uppercase."
  ($.string-upcase s))
(defextend upcase (c)  (isa c 'char)
  ($.char-upcase c))
(defextend upcase (s)  (isa s 'sym)
  (sym:upcase:string s))

(def inc (x (o n 1))
  (coerce (+ (coerce x 'int) n) (type x)))

(def range (start end)
"Returns the list of integers from 'start' to 'end' (both inclusive)."
  (if (> start end)
    nil
    (cons start (range (inc start) end))))

(examples range
  (range 0 10)
  (0 1 2 3 4 5 6 7 8 9 10))

(def mismatch (s1 s2)
"Returns the first index where 's1' and 's2' do not match."
  (catch
    (on c s1
      (when (isnt c (s2 index))
        (throw index)))))

(examples mismatch
  (mismatch '(1 2 3) '(1 2 4))
  2
  (mismatch "abc" "acc")
  1
  (mismatch "abc" "abc")
  nil)

(def memtable ((o keys nil) (o val t))
"Turns a list into a table indicating membership of all elements."
  (w/table tbl
    (each key keys
      (= tbl.key val))))

(= bar* " | ")

(mac w/bars body
"Assumes each expression in 'body' will print something, and intersperses them
with '|'s."
  (w/uniq (out needbars)
    `(let ,needbars nil
       (do ,@(map (fn (e)
                    `(let ,out (tostring ,e)
                       (unless (is ,out "")
                         (if ,needbars
                           (pr bar* ,out)
                           (do (set ,needbars)
                               (pr ,out))))))
                  body)))))

(def len< (x n)
"Is [[len]] of 'x' less than 'n'?"
  (< len.x n))

(examples len<
  (len< (obj a 1 b 2) 3)
  t
  (len< '(1 2 3) 3)
  nil)

(def len> (x n)
"Is [[len]] of 'x' greater than 'n'?"
  (> len.x n))

(examples len<
  (len> '(1 2 3) 2)
  t
  (len> (obj a 1 b 2) 3)
  nil)

(mac thread body
"Concurrently run expressions in 'body', returning an id that can be used to
check their progress, interrupt them, etc.

Creating multiple threads doesn't currently cause arc to use more than one
processor. See http://docs.racket-lang.org/guide/concurrency.html. Programs
will still speed up while waiting for keyboard input, reading/writing files,
or communicating over the network."
  `(new-thread (fn () ,@body)))
(def kill-thread (th)
"Abruptly interrupt a [[thread]] of concurrently running expressions given its id."
  (atomic ($:kill-thread th)))
(def break-thread (th)
"Politely tries to interrupt a [[thread]] of concurrently running expressions
when it's ready to be interrupted."
  (atomic ($:break-thread th)))

(def thread-send (thd v)
  (ac-niltree:$:thread-send thd v))
(def thread-receive ()
  (ac-niltree:$:thread-receive))
(def thread-try-receive ()
  (ac-niltree:$:thread-try-receive))
(def thread-rewind-receive args
  (ac-niltree:$:thread-rewind-receive (ac-denil ,args)))

(= tmpdir* 'nil) ;default tmp directory
(def mktemp ((o prefix "arc") (o dir tmpdir*))
  (let f scheme-f
      ($ (path->string (make-temporary-file (string-append prefix ".~a")
                                            f
                                            (if (eq? dir 'nil) f dir))))))

(mac trav (x . fs)
"Applies each function in 'fs' to 'x', letting the functions recurse on parts
of 'x' by calling 'self'."
  (w/uniq g
    `((afn (,g)
        (when ,g
          ,@(map [list _ g] fs)))
      ,x)))

(examples trav
  (accum acc
    (trav '(1 2 3 4) [acc _]
                     [self cdr._]))
  ((1 2 3 4)
   (2 3 4)
   (3 4)
   (4)))

(mac or= (place expr)
  (let (binds val setter) (setforms place)
    `(atwiths ,binds
       (or ,val (,setter ,expr)))))

(defextend iso (x y) (isa x 'table)
  (and (isa x 'table)
       (isa y 'table)
       (is (len keys.x) (len keys.y))
       (all
         (fn ((k v))
           (iso y.k v))
         tablist.x)))

; default impl for tagged types
(defextend iso (a b) ($.vector? a)
  (iso ($.vector->list a)
       ($.vector->list b)))

(defextend len (x) (isa x 'cons)
  (if
    (acons cdr.x)   (+ 1 (len cdr.x))
    (no cdr.x)  1
                2)) ; dotted list

(defextend len (x) (isa x 'sym)
  0)

(defextend len (x) (isa x 'vector)
  ($.vector-length x))

(defextend len (x) (isa x 'string)
  ($.string-length x))

(defextend len (x) (isa x 'table)
  ($.hash-table-count x))

; most types need define just len
(def empty (seq)
"Is 'seq' an empty container? Usually checks 'seq's [[len]]."
  (iso 0 len.seq))

; optimization: empty list nil is of type sym
(defextend empty (x) (isa x 'cons)
  nil)

; User-definable calling for given types via coerce* extension
(def set-coercer (to from fun)
  " Makes 'fun the coercion function from 'from to 'to.
    See also: [[defcoerce]] "
  (let cnv (or coerce*.to (= coerce*.to (table)))
    (= cnv.from fun)))

(mac defcoerce (to from parms . body)
  " Defines the coercion function from 'from to 'to.
    See also: [[set-coercer]] [[defcall]] "
  `(set-coercer ',to ',from (fn ,parms ,@body)))

(mac defcall (type-name parms . body)
  " Defines the calling function for type 'type-name.
    See also: [[defcoerce]] "
  (w/uniq (fnobj args)
    `(defcoerce fn ,type-name (,fnobj)
       (fn ,args (apply (fn ,parms ,@body) ,fnobj ,args)))))

(defcoerce cons sym (x)  ; only for nil
  nil)

(defcoerce cons table (h)
  (tablist h))

(defcoerce table sym (x)  ; only for nil
  (table))

(defcoerce table cons (al)
  (listtab al))



(def serialize (x)
  x)

(defextend serialize (x) (isa x 'string)
  x)

(defextend serialize (x) (isa x 'cons)
  (cons (serialize (car x))
        (serialize (cdr x))))

(defextend serialize (x) (isa x 'table)
  (list 'tagged 'table
    (accum a
      (maptable (fn (k v)
                  (a (list k serialize.v)))
                x))))

(def unserialize (x)
  (if (acons x)
    (cons (unserialize car.x)
          (unserialize cdr.x))
    x))

(def type* (x)
  (if (and (acons x)
           (is 'tagged car.x)
           (is 3 len.x))
    x.1
    type.x))

(def isa* (x a)
  (is a type*.x))

(def rep* (x)
  (if (and (acons x)
           (is 'tagged car.x)
           (is 3 len.x))
    x.2
    rep.x))

(defextend unserialize (x) (isa* x 'table)  ; (tagged table ((k1 v1) (k2 v2) ..))
  (w/table h
    (map (fn ((k v)) (= h.k unserialize.v))
         rep*.x)))

(redef read ((o x (stdin)) (o eof nil))
  (if (isa x 'string)
    (readstring1 x eof)
    (unserialize:sread x eof)))

(def write (x (o port (stdout)))
  (swrite serialize.x port))

(= hooks* (table))

(def hook (name . args)
  (aif (hooks* name) (apply it args)))

(mac defhook (name . rest)
  `(= (hooks* ',name) (fn ,@rest)))

(mac out (expr) `(pr ,(tostring (eval expr))))

; if renamed this would be more natural for (map [_ user] pagefns*)

(def get (i)
"Returns a function to pass 'i' to its input.
Useful in higher-order functions, or to index into lists, strings, tables, etc."
  [_ i])

(examples get
  (get.2 '(1 2 3 4))
  3
  (get!b (obj a 10 b 20))
  20
  (get.9 sqrt)
  3
  (map get.2 '((a b c) (1 2 3) (p q r)))
  (c 3 r))

(= savers* (table))

(mac fromdisk (var file init load save)
  (w/uniq (gf gv)
    `(unless (bound ',var)
       (do1 (= ,var (iflet ,gf (file-exists ,file)
                      (,load ,gf)
                      ,init))
            (= (savers* ',var) (fn (,gv) (,save ,gv ,file)))))))

(mac diskvar (var file)
  `(fromdisk ,var ,file nil readfile1 writefile))

(mac disktable (var file)
  `(fromdisk ,var ,file (table) load-table save-table))

(mac todisk (var (o expr var))
  `((savers* ',var)
    ,(if (is var expr) var `(= ,var ,expr))))

(mac evtil (expr test)
  (w/uniq gv
    `(let ,gv ,expr
       (while (no (,test ,gv))
         (= ,gv ,expr))
       ,gv)))

(def rand-key (h)
  (if (empty h)
    nil
    (let n (rand (len h))
      (catch
        (each (k v) h
          (when (is (-- n) -1)
            (throw k)))))))

(def ratio (test xs)
  (if (empty xs)
    0
    (/ (count test xs) len.xs)))

(def butlast (x)
"Returns all elements of 'x' except the last."
  (cut x 0 (- len.x 1)))

(mac between (var expr within . body)
"Like [[each]] but run 'within' between iterations."
  (w/uniq first
    `(let ,first t
       (each ,var ,expr
         (if ,first
           (wipe ,first)
           ,within)
         ,@body))))

(def cars (xs) (map car xs))
(def cdrs (xs) (map cdr xs))

(mac mapeach (var lst . body)
  `(map (fn (,var) ,@body) ,lst))

(def load-just (file name)
  (w/infile f file
    (w/uniq eof
      (whiler e (read f eof) eof
        (if (is e.1 name)
          (eval e))))))

(def l (f)
  (load (+ string.f ".arc")))

(wipe current-load-file*)

; any logical reason I can't say (push x (if foo y z)) ?
;   eval would have to always ret 2 things, the val and where it came from
; idea: implicit tables of tables; setf empty field, becomes table
;   or should setf on a table just take n args?

; idea: use constants in functional position for currying?
;       (1 foo) would mean (fn args (apply foo 1 args))
; another solution would be to declare certain symbols curryable, and
;  if > was, >_10 would mean [> _ 10]
;  or just say what the hell and make _ ssyntax for currying
; idea: make >10 ssyntax for [> _ 10]
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
; idea: atable that binds it to table, assumes input is a list
; crazy that finding the top 100 nos takes so long:
;  (let bb (n-of 1000 (rand 50)) (time10 (bestn 100 > bb)))
;  time: 2237 msec.  -> now down to 850 msec

