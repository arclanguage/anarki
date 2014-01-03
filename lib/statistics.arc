; statistics. 10 Oct 2010.
; Ali Moeeny

; experimenting with matrices
; quite possibly these turn out to be redundant but I need to do it until I
; find out what the best way to do it is.

; gives a 1D or 2D matrix/list of random numbers

(def rand-mat dims
  (= dims (flat dims))
  (if
   (no (cdr dims))
        (n-of (car dims) (rand))
   (cdr dims)
        (n-of (car dims) (rand-mat (cdr dims))))
)

(def diff-squared (x y)
  (apply + (map [* (- _ y)(- _ y)] x)))

(def sum-squared (x y)
  (apply + (map * x y)))

(def sum-diff-multiplied (x y)
  (with (mx (avg x) my (avg y))
    (reduce +
            (map *
                 (map [- _ mx] x)
                 (map [- _ my] y)))))

(def pearsons-correlation (x y)
  (/ (sum-diff-multiplied x y)
     (sqrt (* (diff-squared x (avg x))
              (diff-squared y (avg y))))))

; Mark Huetsch

; Some simple matrix stuff I wrote. Would probably be better if matrix were
; its own type, like a hash table, so that things like + and - could be used
; in lieu of +m and -m, and ij could be eliminated, but this was just thrown
; together when I needed a nearest neighbors alg.

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

(defextend + args (all [isa _ 'vector] args)
  (apply +v (map rep args)))

;(defextend + (v i) (and (isa v 'vector) (mem type.i '(num int)))
;  (map [+ _ i] rep.v))

;(defextend - (v i) (and (isa v 'vector) (mem type.i '(num int)))
;  (map [- _ i] rep.v))

(defextend - args (all [isa _ 'vector] args)
  (apply -v (map rep args)))

(defcall vector (vec i)
  rep.vec.i)

; This manner of setting inspired by lush. I like it because it saves a lot of typing if you're constantly modifying vectors.
 (defcall vector (vec i val)
    (= rep.vec.i val))

(def norm (x))

(defextend cos args (all [isa _ 'vector] args)
  (cosv (rep:car args) (rep:cadr args)))

(defextend norm (arg) (isa arg 'vector)
  ; todo, should take optional argument and make euclidean default, many types of norms
  (lenv rep.arg))
