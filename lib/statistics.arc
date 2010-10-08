(require "lib/math.arc")
(require "lib/extend.arc")

; statistics. 06 Oct 2010.

; Ali Moeeny

(def diffSquared (x y) 
     		 (apply + (map [* (- _ y)(- _ y)] x)))

(def sumSquared (x y) 
     	   (apply + (map * x y)))

(def sumDiffMultiplied (x y) 
       (with (mx (avg x) my (avg y))
       (reduce + 
       	       (map * 
	       	    (map [- _ mx] x)
		    (map [- _ my] y)))))



(def pearsonsCorrelation (x y) 
     (/ (sumDiffMultiplied x y) 
        (sqrt 
	      (* 
	      	 (diffSquared 
		 	 x 
			 (avg x)) 
	         (diffSquared y 
		 	 (avg y)))))
)

; Mark Huetsch

; Some simple matrix stuff I wrote. Would probably be better if matrix were its own type, like a hash table, so that things like + and - could
; be used in lieu of +m and -m, and ij could be eliminated, but this was just thrown together when I needed a nearest neighbors alg. 

(def pm (m)
  (each r m (prn r)))

(def +v vs
  (map [reduce + _] (apply zip vs)))

(def -v vs
  (map [reduce - _] (apply zip vs)))

(def *v (a b)
  (if (isa a 'int)
      (map [* a _] b)
      (map [* b _] a)))

(def lenv (v)
  (sqrt (reduce + (map square v))))

(def dot (a b)
  (reduce + (map [apply * _] (zip a b))))

(def cosv (a b)
  (/ (dot a b) (* (lenv a) (lenv b))))

(def cosm (m)
  (mapeach row1 m
	   (map [cosv _ row1] m)))

(def pearson (x y)
  (with (xbar (avg x) ybar (avg y))
    (/ (reduce + (map (fn (xi yi) (* (- xi xbar) (- yi ybar))) x y))
       (sqrt (* (reduce + (map [square (- _ xbar)] x))
		(reduce + (map [square (- _ ybar)] y)))))))

(def pearsonm (m)
  (mapeach row1 m
	   (map [pearson _ row1] m)))

(def rows (m) m)

(def cols (m)
  (apply zip m))

(def tm (m)
  (cols m))

(def +m ms
  (map [apply +v _] (apply zip ms)))

(def -m ms
  (map [apply -v _] (apply zip ms)))

(def ij (m i j)
  ((m i) j))

(mac sij (m i j v)
  (w/uniq modified-row
    `(= ,m
	(let ,modified-row (,m ,i)
	  (+ (cut ,m 0 ,i)
	     (list (+ (cut ,modified-row 0 ,j) (list ,v) (cut ,modified-row (+ 1 ,j))))
	     (cut ,m (+ 1 ,i)))))))

(def bestnpos (n f seq)
  (with (vals (bestn n f seq) indices nil last-val nil last-pos 0)
    (each val vals
      (let found-pos (pos val seq (if (is val last-val) (+ 1 last-pos) 0))
	(push found-pos indices)
	(= last-val val)
	(= last-pos found-pos)))
    (rev indices)))

(def weighted-nearest-neighbors (v m (o n 10))
  (let pearm (pearsonm (+ m (list v)))
    (mapeach vec-pos (bestnpos n > (butlast (last pearm)))
	     (list (m vec-pos) ((butlast (last pearm)) vec-pos)))))

; maybe base this off of a weighted-nearest-neighbors? and allow to choose similarity function
(def nearest-neighbors (v m (o n 10))
  (let pearm (pearsonm (+ m (list v)))
    (mapeach vec-pos (bestnpos n > (butlast (last pearm)))
	     (m vec-pos))))

(def vec lst
  (annotate 'vector lst))

(extend + args (all [isa _ 'vector] args)
        (apply +v (map rep args)))

(extend - args (all [isa _ 'vector] args)
        (apply -v (map rep args)))
