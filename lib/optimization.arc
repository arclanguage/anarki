; Simulated Annealing: 
; 
; It takes a "vector" (an alist) of numbers that give ranges for the variables to optimize
; The costf function should take a list of numbers and return the cost.
;
; It's more or less a direct port from python from "Collective Intelligence"


(mac rand-ft (from to)
  (w/uniq r
     `(let ,r (rand)
       (round (/ (+ (* ,from (- 1 ,r)) 
                    (* ,to ,r)) 2)))))
(def annealing (domain costf T cool step (o sol (map (fn ((x y)) (rand-ft x y)) domain)))
  (if (> T 0.1)
      (withs (i (rand-ft 0 (+ (len domain) 1))
             dir (rand-ft (- step) step)
             oldval (sol i)
             dom (domain i)
             minv (car dom)
             maxv (cadr dom)
             sol_ (+ (cut sol 0 i) 
                     (list (min maxv (max minv (+ (sol i) dir))))
                     (cut sol (+ i 1) (len sol)))
             curc (costf sol)
             newc (costf sol_)
             p (expt 2.71 (/ (- (- newc) curc) T)))
             (annealing domain 
                        costf 
                        (* T cool) 
                        cool 
                        step 
                        (if (or (< newc curc)  (< (rand) p)) sol_ sol)))
      sol))

;(annealing '((0 100) (20 50)(-30 45)) 
;           (fn (x) (let t (apply + (map (compose abs [- _ 40]) x)) (* t t)))  
;           10000 
;           0.95 
;           10)

