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

; redefinition of mac so it is similar to clojure's autogensym
; you can just append '@' to a symbol and that symbol will be replaced with a uniq
; all of the symbols that are the same will be replaced with the same uniq
(= defmacro mac)
(= redefmacro remac)

(redefmacro mac (name args . body)
  (let uniqs (table)
    `(defmacro ,name ,args
       ,@(subst auto
                [or= uniqs._ uniq._]
                (tree:map maybe-ssexpand
                          tree.body)))))

(redefmacro mac! (name args . body)
  (let uniqs (table)
    `(redefmacro ,name ,args
       ,@(subst auto
                [or= uniqs._ uniq._]
                (tree:map maybe-ssexpand
                          tree.body)))))

(def auto (exp)
  "Tests whether an expression should be autogensymed"
  (and exp (isa exp 'sym) (endmatch "@" (string exp))))

(def maybe-ssexpand (s)
  (if (ssyntax s)
    (ssexpand s)
    s))

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

(def gc () ($.collect-garbage))

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

(def classify (classifier seq)
  " Groups the elements of `seq' by `classifier', returning back a table,
    whose keys are the results of `classifier' and whose values are lists
    of the corresponding elements of `seq'. For example:

      arc> (classify type '(1 \"foo\" a 2 (b)))
      #hash((cons . ((b))) (int . (2 1)) (string . (\"foo\")) (sym . (a)))

    See also [[partition]] [[keep]] [[rem]] "
  (w/table h
    (each e seq
      (push e (h classifier.e)))))

(def partition (test seq)
  " Equivalent to but more efficient than
   `(list (keep test seq) (rem test seq))'. See also [[keep]] [[rem]] "
  (let (passed failed) nil
    (each e seq
      (if test.e
        (push e passed)
        (push e failed)))
    (list rev.passed rev.failed)))

(def unzip (xs)
  " Precisely as `zip', except that zip's `ls' is unzip's `xs'; so it takes one
    list of lists rather than any number of lists as arguments. Can be thought
    of as performing the inverse operation.
    See also [[zip]] "
  (apply map list xs))

(def zip ls
  " Returns a list of lists; the n-th element of the result is a list of the
    n-th elements of the lists in `ls'. The length of the result is the length
    of the shortest list in `ls'; extra elements in other lists are discarded.
    See also [[unzip]] "
  (unzip ls))

(def mklist (x)
  " Wraps atoms in a list; does nothing if `x' is already a list.
    See also [[atom]] [[alist]] [[list]] "
  (check x alist list.x))

; 'many was precisely the same function as 'acons, hence has been removed (any
; cons has a length > 0, since nil is not a cons). Also, 'popfind has been
; renamed 'pull1, to fit with the newly-added 'rem1.

(def rem1 (test seq)
  " Returns a copy of `seq' with the first element that passes `test' removed.
    See also [[rem]] [[keep]] [[pull1]] [[pull]] "
  (zap testify test)
  (if alist.seq ((afn (s)
                   (if no.s nil
                       (f car.s) cdr.s
                       (cons car.s (self cdr.s))))
                 seq)
      (coerce (rem1 test (coerce seq 'cons)) 'string)))

(mac pull1 (test place)
  " Removes the first element that passes `test' from `place'.
    See also [[pull]] [[rem1]] [[rem]] [[keep]] "
  `(= ,place (rem1 ,test ,place)))

(= len= [is len._a _b])
(= len- [- len._a _b])
(= car< [< car._a car._b])

(def keepkey (key lst) (keep [_ key] lst))
(def mapkey (key lst) (map [_ key] lst))

(def rand-pos (lst) (if lst (rand:len lst)))

(mac pushend (elem lst)
  `(= ,lst (join ,lst (list ,elem))))

(mac popnth (lst n)
  (w/uniq g1
    `(let ,g1 (,lst ,n)
       (= ,lst (+ (cut ,lst 0 ,n) (cut ,lst (+ 1 ,n))))
       ,g1)))

(mac poprand (lst)
  (w/uniq g1
    `(if ,lst
         (let ,g1 (rand-pos ,lst)
           (popnth ,lst ,g1)))))

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

(mac mappendeach (var lst . body)
  " As 'mapeach, but using 'mappend instead of 'map.
    See also [[mapeach]] [[mappend]] [[each]] "
  `(mappend (fn (,var) ,@body) ,lst))

(mac ado body
  " Anaphoric do. Each expr in the body is available to the next.
    See also [[aif]] [[awhen]] [[aand]] [[ado1]]"
  (aif cdr.body `(let it ,car.body (ado ,@it))
       car.body))

(mac ado1 args
  " Anaphoric ado1. First expr is available to the rest and also returned.
    See also [[aif]] [[awhen]] [[aand]] [[ado]]"
  `(ret it ,car.args
     ,@cdr.args))

(mac assert (exp (o msg (+ "Assertion failed: "
                           (tostring:ppr-main exp (len "Assertion failed: ") t))))
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
    See also [[withr]] [[where]] "
  `(let ,(map1 car bindings) nil
     ,@(map [cons 'assign _] bindings)
     ,@body))

(mac withr (bindings . body)
  " Scheme's 'letrec, with the redundant inner parens removed.
    See also [[withf]] [[letf]] [[where]] [[withr/p]] "
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

; inspired by Haskell
(mac where (expr . parms)
  " Binds `parms' and evaluates `expr'. Examples:

      arc> (where (square x)
              square [* _ _]
              x 2)
      4

    Note that binding is recursive, but that actual assignment of values is done
    in the reverse of the order given, so any variables which are both bound and
    used in `parms' must be used in reverse dependency order:

      arc> (where x x (+ y y) y 0)    ; this works as expected
      y
      arc> (where x y 0 x (+ y y))    ; this doesn't
      nil

    Essentially, this is a reversed form of Scheme's 'letrec,
    with many fewer parentheses. Inspired by Haskell's \"where\".
    See also [[withr]] [[withr/p]] [[withf]] "
  `(withr/p ,(rev:pair parms) ,expr))

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

; start Andrew Wilcox (aw) code

; http://awwx.ws/span0.arc
(def span (tst lst)
  ((afn (a lst)
     (if (and lst (tst (car lst)))
          (self (cons (car lst) a) (cdr lst))
          (list (rev a) lst)))
   nil lst))

; http://awwx.ws/implicit2.arc
(mac implicit (name (o val))
  `(do (defvar ,name ($.make-parameter ,val))
       (mac ,(sym (string "w/" name)) (v . body)
         (w/uniq (param gv gf)
           `(with (,param (defvar-impl ,',name)
                   ,gv ,v
                   ,gf (fn () ,@body))
              ($ (parameterize ((,param ,gv)) (,gf))))))))

; http://awwx.ws/extend-readtable0.arc
(def extend-readtable (c parser)
  ($
   (current-readtable
    (make-readtable (current-readtable)
                    c
                    'non-terminating-macro
                    (lambda (ch port src line col pos)
                      (parser port))))))

; end aw code

; END RIPOFFS
