; lib/util.arc: A collection of useful miscellanea

; Contributors:
; - Michael Arntzenius <daekharel@gmail.com>

; Persons we've shamelessly ripped off:
; - Conan Dalton <conan@conandalton.net>
; - absz (http://arclanguage.org/user?id=absz)
; - fallintothis (http://arclanguage.org/user?id=fallintothis)

; License: Do what you want, but it's not my fault!
; This license applies to all code in this file UNLESS OTHERWISE NOTED.

; Feel free to make additions to this file and push them to anarki. Keep it to
; stuff that is widely applicable. Don't remove or change the semantics of stuff
; other people added. Add yourself to the contributors list above if you want.

; If you wish to use a different license, make a comment around the code you've
; contributed noting the license. No licenses that don't permit modification
; (duh) and redistribution (double duh).

; Try and keep track of what is whose in sections with unknown licenses
; ("ripoffs").


; miscellaneous

(def fst (a . _) " Returns its first argument. See also [[snd]] " a)
(def snd (a b . _) " Returns its second argument. See also [[fst]] " b)

(def bool (x)
  " Returns `t' if x is not nil, and `nil' otherwise. "
  (if x t))

(def uniqs (lst)
  " Returns a list of gensyms, one for each element of `lst'. Elements
    of `lst' that are symbols are used as the base names for their
    corresponding gensyms.
    See also [[uniq]] "
  (map1 (iff asym uniq [uniq]) lst))

; type checkers
(def asym (x) " `t' iff `x' is a symbol. " (isa x 'sym))
(def astring (x) " `t' iff `x' is a string. " (isa x 'string))
(def atable (x) " `t' iff `x' is a table. " (isa x 'table))
(def aint (x) " `t' iff `x' is an int. " (isa x 'int))
(def anum (x) " `t' iff `x' is a num. Note that ints are not nums. "
  (isa x 'num))


; list manipulation

; 'reduce and 'rreduce have somewhat quirky behavior, well-suited to arithmetic
; functions, but otherwise hard to reason about. I usually prefer 'foldl and
; 'foldr.
(def foldl (f v l)
  (if l (foldl f (f v car.l) cdr.l) v))

(def foldr (f v l)
  (foldl flip.f v rev.l))

(def foot (l)
  " Gets the last cons in a proper list. (Fails on dotted lists.) "
  (aif cdr.l foot.it l))

(def join/d ls
  " Destructive join.
    See also [[join]]"
  (foldr (afn (a b) (if b (aif foot.a (do (scdr it b) a) b) a)) nil ls))

(def partition (test seq)
  " Equivalent to but more efficient than
   `(list (keep test seq) (rem test seq))'. See also [[keep]] [[rem]] "
  (let (passed failed) nil
    (each e seq
      (if test.e
        (push e passed)
        (push e failed)))
    (list rev.passed rev.failed)))

(def zip ls
  " Returns a list of lists; the n-th element of the result is a list of the
    n-th elements of the lists in `ls'. The length of the result is the length
    of the shortest list in `ls'; extra elements in other lists are discarded. "
  (apply map list ls))

(def mklist (x)
  " Wraps atoms in a list; does nothing if `x' is already a list."
  (check x alist list.x))


; combinators

(def applied (f)
  " Returns a fn that calls `f' on the list of its arguments.
    For example, 'max is equivalent to `(applied [best > _])'. "
  (fn a (f a)))

(def flip (f)
  " Flips the order of the first two arguments of `f'.
    For example: ((flip cons) 1 2) => (2 . 1) "
  (fn (x y . zs) (apply f y x zs)))

(def curry (f . xs)
  " Partially applies (\"curries\") `f' to `xs'. "
  (fn ys (apply f (join xs ys))))

(def const (x) 
  " Creates a fn that takes any number of arguments and returns `x'. "
  (fn _ x))

(def tfn _ " Ignores its arguments and returns t. " t)
(def nilfn _ " Ignores its arguments and returns nil. " nil)

(def norf fns
  " Creates a function which returns `t' iff none of `fns' return `t'
    on its arguments.
    See also [[orf]] [[andf]] [[nor]] "
  (complement (apply orf fns)))

(def iff funs
  " Put simply: iff is to if as andf is to and. Specifically: 

    (iff) => idfn
    (iff fun) => fun
    (iff test fun rest ...) => a fn that applies `fun' to its arguments if they
    pass `test', otherwise applies `(iff rest...)' to them.

    Examples:

      arc> ((iff alist car) '(x))
      x
      arc> ((iff alist car) 2)
      2
      arc> ((iff < (fn (x y) x) (fn (x y) y)) 1 2)
      1

    See also [[andf]] [[orf]] [[check]] [[idfn]] "
  (case len.funs
    0 idfn
    1 funs.0
      (withs ((test fun . rest) funs
              restfun (apply iff rest))
        (fn a (if (apply test a) (apply fun a) 
                  (apply restfun a))))))


; macros

(mac mapeach (var lst . body)
  " Maps `(fn (,var) ,@body)' over `lst'.
    See also [[each]] [[map]] [[mappendeach]] "
  `(map (fn (,var) ,@body) ,lst))

(mac mappendeach (var lst . body)
  " As 'mapeach, but using 'mappend instead of 'map.
    See also [[mapeach]] [[mappend]] [[each]] "
  `(mappend (fn (,var) ,@body) ,lst))

(mac ado body
  " Anaphoric do. 
    See also [[aif]] [[awhen]] [[aand]] "
  (aif cdr.body `(let it ,car.body (ado ,@it))
       car.body))

; now that pg has renamed 'assert to 'set, we're free to use it in its more
; conventional sense
(mac assert (exp (o msg (+ "Assertion failed: " 
                           (tostring:ppr exp (len "Assertion failed: ") t))))
  " Errors with `msg' if `exp' evaluates to nil. "
  `(unless ,exp (err ,msg)))

(mac asserts args
  " Asserts each expr in `args', with the default error message. "
  `(do ,@(map [list 'assert _] args)))

(mac switchlet (var expr . cases)
  " Like 'caselet, except it (lazily) evals the expressions compared against.
    See also [[switch]] [[caselet]] [[case]]"
  `(let ,var ,expr
     ,((afn (args)
         (if (no cdr.args) car.args
           `(if (is ,var ,car.args) ,cadr.args
              ,(self cddr.args)))) cases)))

(mac switch (expr . cases)
  " 'switch is to 'switchlet as 'case is to 'caselet.
    See also [[switchlet]] [[case]] [[caselet]]"
  `(switchlet ,(uniq) ,exp ,@cases))

(mac dol (parms (test result) . body)
  " Like the standard lisp/scheme do loop, but with redundant inner parens 
    removed."
  (w/uniq loop-name
    (let parms (tuples parms 3)
      `((rfn ,loop-name ,(map1 [_ 0] parms)
          (if ,test ,result
            (do ,@body (,loop-name ,@(map1 [_ 2] parms)))))
         ,@(map1 [_ 1] parms)))))

(mac w/stdoutfile (name . body)
  " Redirects stdout to the file `name' within `body'. 
    See also [[w/stdinfile]] [[w/outfile]] [[w/stdout]] "
  (w/uniq str `(w/outfile ,str ,name (w/stdout ,str ,@body))))

(mac w/stdinfile (name . body)
  " Redirects standard input from the file `name' within `body'.
    See also [[w/stdoutfile]] [[w/infile]] [[w/stdin]] "
  (w/uniq str `(w/infile ,str ,name (w/stdin ,str ,@body))))


; binding forms

(mac with/p (vars-vals . body)
  " Scheme/Common Lisp's `let' - ie: 'with with the parens added back.
    Easier to use in macro expansions than 'with.
    See also [[with]] [[withs/p]] "
  `(with ,(apply join vars-vals) ,@body))

(mac withs/p (vars-vals . body)
  " Like Scheme/Common Lisp's `let*' - ie: 'withs with the parens added back.
    Easier to use in macro expansions than 'withs.
    See also [[withs]] [[with/p]] "
  `(withs ,(apply join vars-vals) ,@body))

; a 'with that works for defining recursive fns
(mac withr/p (bindings . body)
  " Scheme's 'letrec. 
    See also [[withr]] "
  `(let ,(map1 car bindings) nil
     ,@(map [cons 'assign _] bindings)
     ,@body))

(mac withr (bindings . body)
  " Scheme's 'letrec, with the redundant inner parens removed. 
    See also [[withf]] [[letf]] [[withr/p]] "
  `(withr/p ,pair.bindings ,@body))

; mutually recursive local fns
(mac withf/p (fns . body)
  " Like 'withf, only with extra parens, as in 'with/p compared to 'with.
    See also [[withf]] [[with/p]] [[withr]] [[withr/p]] "
  `(withr/p ,(mapeach (name . rest) fns `(,name (fn ,@rest)))
     ,@body))

(mac withf (fns . body)
  " Defines a set `fns' of mutually recursive local fns within `body'. Each 
    three elements of `fn' correspond to a fn name, argument list, and body,
    so you'll need to use 'do if you want a multi-expression fn body.
    Example:

      arc> (withf (is-even (x) (case x 0 t (is-odd (- x 1)))
                   is-odd (x) (case x 0 nil (is-even (- x 1))))
             (keep is-odd (range 0 5)))
      (1 3 5)

    See also [[letf]] [[withr]] "
  `(withf/p ,(tuples fns 3) ,@body))

(mac letf (name args expr . body)
  " Defines a (possibly recursive) local fn `(fn ,args ,expr)' named `name'
    within `body'. Example:

      arc> (letf last (x) (aif cdr.x last.it car.x)
             (last '(x y z)))
      z

    See also [[withf]] [[withr]] "
  `(withf (,name ,args ,expr) ,@body))


; ripoffs - licenses unknown

; once-only by fallintothis
; http://arclanguage.org/item?id=9939
; CHANGED 2009-08-20:
;   + take advantage of 'uniqs, 'with/p, 'zip
;   + wrap 'names in a list if it's an atom
;   + alter indentation slightly
;  - Michael Arntzenius

(mac once-only (names . body)
  (withs (names (check names alist list.names)
          gensyms (uniqs names))
    `(w/uniq ,gensyms
       `(with ,(list ,@(mappend list gensyms names))
          ,(with/p ,(zip names gensyms)
             ,@body)))))

; afnwith by Conan Dalton
; http://arclanguage.org/item?id=10055
; CHANGED 2009-08-15: added docstrings - Michael Arntzenius
(mac rfnwith (name withses . body)
  " Convenient wrapper for applying an rfn using with-like syntax.
    `withses' is a list of argument names and their initial values.
    Best explained by example:

      arc> (rfnwith sum (x (range 1 3))
             (iflet (a . r) x (+ a (sum r)) 0))
      6

    The above example macroexpands to:

      ((rfn sum (x) (iflet (a . r) x (+ a (sum r)) 0))
       (range 1 3))

    See also [[afnwith]] [[w/rfn]] [[rfn]] "
  (let w (pair withses)
    `((rfn ,name ,(map car w) ,@body) ,@(map cadr w))))

(mac afnwith (withses . body)
  " Convenient wrapper for applying an afn using with-like syntax.
    `withses' is a list of argument names and their initial values.
    Best explained by example:

      arc> (afnwith (x (range 1 3))
             (iflet (a . r) x (+ a (self r)) 0))
      6

    See also [[rfnwith]] [[w/afn]] [[afn]] "
  `(rfnwith self ,withses ,@body))

; ripoff: w/afn, by absz
; http://arclanguage.org/item?id=10125
; CHANGED 2009-08-15: added docstrings - Michael Arntzenius

(mac w/rfn (name withses . body)
  " Convenient wrapper for applying an rfn using preexisting variables
    in `withses' as arguments. Best explained by example:

      arc> (let x (range 1 3) 
             (w/rfn sum (x)
               (iflet (a . r) x (+ a (sum r)) 0)))
      6

    The above example (w/rfn sum ...) macroexpands to:

      ((rfn sum (x) (iflet (a . r) x (+ a (sum r)) 0))
       x)

    See also [[w/afn]] [[rfnwith]] [[rfn]] "
  `(rfnwith ,name ,(mappend [list _ _] mklist.withses) ,@body))

(mac w/afn (withses . body)
  " Convenient wrapper for applying an afn using the preexisting variables
    in `withses' as arguments. Best explained by example:

      arc> (let x (range 1 3)
             (w/afn (x)
               (iflet (a . r) x (+ a (self r)) 0)))
      6

    The above example (w/afn (x) ...) macroexpands to:

      ((afn (x) (iflet (a . r) x (+ a (sum r)) 0))
       x)

    See also [[w/rfn]] [[afnwith]] [[afn]] "
  `(w/rfn self ,withses ,@body))

; end ripoffs

(def butlast (x) 
  (cut x 0 (- (len x) 1)))

(def joinstr (lst (o glue " ")) 
  (let lst (keep [len> _ 0] lst)
    (if lst 
  (apply + (intersperse (string glue) lst))
  "")))
