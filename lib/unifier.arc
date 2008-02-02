; the unifier part is more or less from PAIP
; the rest was written with lots of beer, check for bugs...
; (the mcond part isn't really... meh whatever... cheers

(mac var? (x) (w/uniq  g 
  `(and (isa ,x 'sym)
       (let ,g (coerce ,x 'string)
         (and (isnt ,g "") (is (,g 0) #\?))))))
(mac lookup (var bindings) `(,bindings ,var));  `(alref ,bindings ,var))
(mac extend (var x bindings) `(do (= (,bindings ,var) ,x) ,bindings))
(def unify (x y (o bindings (table)))
  (if
   (is bindings 'fail) 'fail
   (iso x y) bindings
   (var? x) (unify-var x y bindings)
   (var? y) (unify-var y x bindings)
   (and (isa x 'cons) (isa y 'cons))
       (unify (cdr x) (cdr y)
              (unify (car x) (car y) bindings))
       'fail))

(def unify-var (var x bindings)
  (let b (lookup var bindings)
    (if b (unify b x bindings)
        (and (var? x) (lookup x bindings))
          (unify var (lookup x bindings) bindings)
        (occurs-check var x bindings) 
          'fail 
          (extend var x bindings))))

(def occurs-check (var x bindings)
  (if (is var x) t
      (and (var? x) (lookup x bindings))
        (occurs-check var (lookup x bindings) bindings)
      (isa x 'cons)
        (or (occurs-check var (car x) bindings)
            (occurs-check var (cdr x) bindings))
         nil))

;(unify '(?x ?y ?x) '(?y ?x ?y))

(def toalist (t)
  (map (fn (k) (cons k (t k))) (keys t)))

(mac match (expr pat . body) ;(prn "IN MATCH: " expr "; " pat "; " body)
  (w/uniq u
    (let u (unify expr pat)
       (if (is 'fail u) 
           ''fail
;           `(with ,(reduce + (map (fn ((x . y)) (pr `(,x ',y))) (toalist u)))
           ` (with ,(reduce + (map (fn ((x . y)) `(,x ',y)) (toalist u)))
                 ,@body)))))



(mac mcond (val . pats) ;(prn "in mc: " val " | " pats " |"); match-case
   (w/uniq x
      (if (car pats)
        `(let ,x (match ,val ,(caar pats) ,(cadr:car pats))
           (if (is 'fail ,x)
               (mcond ,val ,@(cdr pats))
               ,x))
      ())))

;(def x () (match (a c) (a ?x) (prn ?x)))
;(match ?x (1 2 3) (prn ?x))
;(match (1 2 3) (?x ?y ?z) (prn ?x ?y ?z))
;(match (?x ?y) ((1 2 3) uga) (prn ?x))
;(match (1) (?x) ?x)
;(mcond (foo bar) 
;       ((a b) 4) 
;       ((?x) (cons 'haha ?x))
;       (?z (pr (cons 'lala ?z))))
