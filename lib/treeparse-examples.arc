;; Examples of using "treeparse.arc"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 1: Pattern matching with guard clauses.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require "lib/defpat.arc")
(require "lib/treeparse.arc")

(mac w/nils (vars . body)
  `(with ,(join (intersperse nil vars) '(nil))
     ,@body))

;; This may help, if you can read EBNF.
;; stuff     ->  <anything but =, /, or ||>
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
  (= stuff (seq (cant-see (alt '/ '|| '=))
                anything))
  (= arglist (filt list (many stuff)))
  (= guard (alt (filt list (many2 stuff)) stuff))
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
    `(apply (p-m:fn ,@(car (s cases))) ,xs)))

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
;; Example 2: Brackets.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(= brack-parser 
   (alt (filt list:cadr:car `[,(many anything)]) anything))

(def transform-brackets (pats)
  "Converts [bracket] lists to normal lists.
  (transform-brackets '([[[a] ([b]) c]]))  =>  ((((a) ((b)) c)))"
  (if (no (acons pats)) pats
      (let parsed (car ((many brack-parser) pats))
        (map transform-brackets parsed))))

;; `brack-parser' and `tranform-brackets' do the same thing as this,
;; as long as '[] still evaluates to (make-br-fn nil).
(def transform-brackets2 (a)
  "(transform-brackets2 '([] [x]))  =>  (nil (x))"
  (if (and (acons a) (~dotted a))
      (if (is 'make-br-fn (car a))
          (map transform-brackets2 (cadr a))
          (map transform-brackets2 a))
      a))