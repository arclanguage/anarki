;; Examples of using "treeparse.arc"
(require "lib/defpat.arc")
(require "lib/treeparse.arc")
(mac w/nils (vars . body)
  `(with ,(intersperse nil vars) ,@body))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 1: Traditional Lisp cond using if and do.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Demonstrates the use of filters to simultaneously parse a tree
;; and transform it. 

(w/nils (s clause forms)
  (= forms (filt [list:cons 'do _] (many anything)))
  (= clause (filt car (list (seq anything forms))))
  (= s (filt [cons 'if _] (many clause)))
  (mac cond clauses (parse-all s clauses)))

;; This is how the cond parser looks without the filters.
;; (= forms  (many anything))
;; (= clause (list (seq anything forms)))
;; (= s      (many clause))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 2: Pattern matching with guard clauses.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Using filters to construct a more advanced macro, this time
;; supplementing the `defpat' matching with guard clauses and a
;; different syntax.

;; This may help, if you can read EBNF.
;; form      ->  <anything but =, /, or ||>
;; arglist   ->  [stuff*]
;; guard     ->  stuff*
;; body      ->  <anything>
;; last-body ->  <anything>*
;; guard-clause ->  /  guard = body
;; guard-clause ->  || guard = body
;; a ->  = body
;; a ->  = guard-clause*
;; s ->  [arglist a]* [last-body]
(w/nils (stuff arglist guard body last-body guard-clause a s)
  (= form (seq (cant-see (alt '/ '|| '=)) anything))
  (= arglist (filt list (many form)))
  (= guard (alt (filt list (many2 form)) form))
  (= body anything)
  (= last-body (many1 anything))
  (= guard-clause (filt [list (_ 1) (_ 3)] 
                        (seq (alt '|| '/) guard '= body)))
  (= a (alt (filt cdr (seq '= body))
            (filt [list (cons 'if _)] (many1 guard-clause))))
  (= s (seq (many (seq arglist a))
            (maybe last-body)))
  (mac hcase (xs . cases)
    "Haskell style pattern matching."
    `(apply (p-m:fn ,@(parse-all s cases)) ,xs)))

;; An example of the `hcase' pattern matching.
;; Gaurd clauses can be denoted either with / or ||.
;; Note that the outter parens in the guard conditions are optional.
(def union (< xs ys)
  "Merge two sorted lists, discarding duplicates."
  (hcase `(,xs ,ys)
    xs () = xs
    () ys = ys
    (x . xt) (y . yt)
    || < x y = (cons x (union < xt ys))
    || < y x = (cons y (union < xs yt))
    || t     = (cons x (union < xt yt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 3: Brackets.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(= brack-parser 
   (alt (filt list:cadr:car `[,(many anything)]) anything))

(def transform-brackets (pats)
  "Converts [bracket] lists to normal lists.
  (transform-brackets '([[[a] ([b]) c]]))  =>  ((((a) ((b)) c)))"
  (if (no (acons pats)) pats
      (let parsed (parse-all (many brack-parser) pats)
        (map transform-brackets parsed))))

;; This does roughly the same thing as `tranform-brackets', as long as
;; '[] still evaluates to (make-br-fn nil).
(def transform-brackets2 (a)
  "(transform-brackets2 '([] [x]))  =>  (nil (x))"
  (if (and (acons a) (~dotted a))
      (if (is 'make-br-fn (car a))
          (map transform-brackets2 (cadr a))
          (map transform-brackets2 a))
      a))