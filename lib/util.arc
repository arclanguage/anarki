;; lib/util.arc: A collection of useful but mostly unrelated stuff

;; Authors:
;; - Michael Arntzenius <daekharel@gmail.com>

;; License: Do what you want, but it's not my fault!

;; Feel free to make additions to this file and push them to anarki, as long as
;; you don't mind the licensing (above). Keep it to stuff that is widely rather
;; than specifically applicable. Don't change stuff other people added, though.
;; Add yourself to the authors list if you want.

(mac mappendeach (var lst . body)
  "Like 'mapeach, except it uses 'mappend instead of 'map."
  `(mappend (fn (,var) ,@body) ,lst))

(mac ado body
  "Anaphoric do."
  (aif cdr.body `(let it ,car.body (ado ,@it))
    car.body))

(def flip (f)
  "Flips the order of arguments of 'f. eg: ((flip cons) 1 2) -> (2 . 1)"
  (fn (a b) (f b a)))

(mac switchlet (var expr . cases)
  "Like caselet, except it (lazily) evals the expressions compared against.
   See also [[switch]] [[caselet]] [[case]]"
  `(let ,var ,expr
     ,((afn (args)
         (if (no cdr.args) car.args
           `(if (is ,var ,car.args) ,cadr.args
              ,(self cddr.args)))) cases)))

(mac switch (expr . cases)
  "switch is to switchlet as case is to caselet.
   See also [[switchlet]] [[case]] [[caselet]]"
  `(switchlet ,(uniq) ,exp ,@cases))

(mac dol (parms (test result) . body)
  "Like the standard lisp do loop, but with redundant inner parens removed."
  (w/uniq loop-name
    (let parms (tuples parms 3)
    `((rfn ,loop-name ,(map1 [_ 0] parms)
        (if ,test ,result
          (do ,@body (,loop-name ,@(map1 [_ 2] parms)))))
       ,@(map1 [_ 1] parms)))))

;; Searching
(def level-search (init test unfold (o fail nil))
  (catch
    ((afn (lvl)
       (aif (foldr
              (fn (elt unfolded)
                (if test.elt throw.elt
                  (join unfold.elt unfolded)))
              nil lvl)
         self.it
         throw.fail))
        (list init))))

(def depth-search (init test unfold (o fail nil))
  (catch
    ((afn (e)
       (if test.e throw.e
         (map self unfold.e))) init)
    fail))


;; List manipulation

;; 'reduce and 'rreduce have somewhat quirky behavior, well-suited to arithmetic
;; functions, but otherwise hard to reason about. I usually prefer 'foldl and
;; 'foldr.
(def foldl (f v l)
  (if l (foldl f (f v car.l) cdr.l) v))

(def foldr (f v l)
  (foldl flip.f v rev.l))

(def foot (l)
  "Gets the last cons in a proper list. (Fails on improper lists.)"
  (aif cdr.l foot.it l))

(def join/d/2 (a b)
  (if b (aif foot.a (do (scdr it b) a) b) a))

(def join/d ls
  " Destructive join.
    See also [[join]]."
  (foldr join/d/2 nil ls))

(def partition (test seq)
  "Equivalent to but more efficient than (list (keep test seq) (rem test seq)).
    See also [[keep]] [[rem]] [[pull]]"
  (with (passed ()
         failed ())
    (each e seq
      (if test.e
        (push e passed)
        (push e failed)))
    (list rev.passed rev.failed)))

(def zip ls
  (apply map list ls))


;; Binding forms

;; helper function - generates a list of temporaries for each element in 'lst,
;; with an appropriate base-name if the corresponding element is a symbol
(def uniqs (lst)
  (map1 [if (isa _ 'sym) uniq._ (uniq)] lst))

;; scheme/lisp-like let - ie: with the parens added back in. While I agree with
;; pg that the less-parens version is better in code, the more-parens version is
;; easier to generate in macros.
(mac with/p (vars-vals . body)
  `(with ,(apply join vars-vals) ,@body))

(mac withs/p (vars-vals . body)
  `(withs ,(apply join vars-vals) ,@body))

;; with works for defining recursive functions
(mac withr/p (bindings . body) 
  `(with/p ,(mapeach (name _) bindings (list name nil))
     ,@(mapeach b bindings `(set ,@b))
     ,@body))

(mac withr (bindings . body)
  `(withr/p ,pair.bindings ,@body))

;; mutually recursive local functions
(mac withf (fns . body)
  `(withr/p ,(mapeach (name . rest) fns `(,name (fn ,@rest)))
     ,@body))

(mac letf (fun . body)
  `(withf (,fun) ,@body))

;; 'iflet generalizations
(mac ifwith/p (vars-exps then . rest)
  (withs (vars (map1 car vars-exps)
          exps (map1 cadr vars-exps)
          tmps (uniqs vars))
    `(with/p ,(zip tmps exps)
       (if (and ,@tmps)
         (with/p ,(zip vars tmps)
           ,then)
         (do ,@rest)))))

(mac ifwiths/p (vars-exps then . rest)
  (w/uniq exit
    `(point ,exit
       (let ,exit (fn () (,exit (do ,@rest)))
         (withs/p ,(mapeach (v e) vars-exps `(,v (or ,e (,exit))))
           ,then)))))

(mac ifwith (bindings then . rest)
  "ifwith is to iflet as with is to let
   See also [[iflet]] [[with]] [[let]] [[if]]"
  `(ifwith/p ,pair.bindings ,then ,@rest))

(mac ifwiths (bindings then . rest)
  `(ifwiths/p ,pair.bindings ,then ,@rest))

(mac whenwith (bindings . body)
  `(iflet ,bindings (do ,@body)))

(mac whenwiths (bindings . body)
  `(ifwiths ,bindings (do ,@body)))
