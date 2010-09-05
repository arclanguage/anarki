(def zeros (x)
  (n-of x 0))

(def ones (x)
  (n-of x 1))


;calculus fns

(def deriv (f)
  "provides differential of a function of a single vairable"
  (fn (x)
    (let dx (max 1d-9 (abs:* x 1d-9))
       (/ (- (f (+ x dx))
	     (f x))
          dx))))

(mac partial-diff (f n arity)
  "returns deriv function of f (w/ num args ARITY) wrt Nth variable(from 0) "
  (withs (arglis (n-of arity (uniq))
	  a-lis (firstn n arglis)
	  x arglis.n
	  b-lis (nthcdr (+ n 1) arglis))
    `(fn ,arglis
      (let dx (max 1d-9 (abs:* ,x 1d-9)
	(/ (- (,f ,@a-lis (+ ,x dx) ,@b-lis)
	      (,f ,@a-lis    ,x     ,@b-lis))
	   dx)))))

(mac partial-diff-vec (f n arity)
  "returns vector with each element differentiated with respect to Nth argument"
  (withs (arglis (n-of arity (uniq))
	  a-lis (firstn n arglis)
	  x arglis.n
	  b-lis (nthcdr (+ n 1) arglis))
    `(fn ,arglis
	(let dx (max 1d-9 (abs:* ,x 1d-9)
	   (vec-scale (vec- (,f ,@a-lis	(+ ,x dx) ,@b-lis)
			    (,f ,@a-lis    ,x     ,@b-lis))
		      (/ 1 dx))))))

(def grad (f)
  "gradient of 3D scalar field given by F"
  (fn (x y z)
    (list ((partial-diff f 0 3) x y z)
	  ((partial-diff f 1 3) x y z)
	  ((partial-diff f 2 3) x y z))))

(def div (f)
  "divergence of 3D vector field given by F"
  (fn (x y z)
      (+ (((partial-diff-vec f 0 3) x y z) 0)
	 (((partial-diff-vec f 1 3) x y z) 1)
	 (((partial-diff-vec f 2 3) x y z) 2))))

(def curl (f)
  "curl of 3D vector field given by F"
  (with (d/dx (partial-diff-vec f 0 3)
	 d/dy (partial-diff-vec f 1 3)
	 d/dz (partial-diff-vec f 2 3))
  (fn (x y z)
    (list (- ((d/dy x y z) 2)
	     ((d/dz x y z) 1))
	  (- ((d/dz x y z) 0)
	     ((d/dx x y z) 2))
	  (- ((d/dx x y z) 1)
	     ((d/dy x y z) 0))))))

(def integral (f)
  "returns the integral of a single argument function"
  (fn (lower upper (o its 10000))
    (withs (dx      (/ (- upper lower) its)
	    x       lower
	    current (f x)
	    next    (f (+ x dx))
	    accum   0) 
      (while (<= x (- upper dx))
	     (++ accum (* (/ (+ current next) 2) dx))
	     (++ x dx)
	     (= current next)
	     (= next (f:+ x dx)))
      accum)))



; vector fns

(def vec-dot (v1 v2)
  (apply + (map (fn (x y) (* x y)) v1 v2)))

(def vec-cross (v1 v2 . args)
  (if (car args)
      (vec-cross (vec-cross v1 v2) (car args) (cdr args))
      (list (- (* v1.1 v2.2) (* v2.1 v1.2))
	    (- (* v1.2 v2.0) (* v2.2 v1.0))
	    (- (* v1.0 v2.1) (* v2.0 v1.1)))))

(with (v+ (fn (v1 v2)
	     (map (fn (x y) (+ x y)) v1 v2))
       v- (fn (v1 v2)
	     (map (fn (x y) (- x y)) v1 v2)))
  
  (def vec+ (v1 . args)
    (if no.args v1
	(reduce v+ (cons v1 args))))
  
  (def vec- (v1 . args)
    (if no.args (map [- _] v1)
	(v- v1 (apply vec+ args))))

)


(def vec-scale (vec . scalars)
  (let c (apply * scalars)
    (map [* _ c] vec)))

(def quad-add args
  ((afn (tot xs)
     (if no.xs (sqrt tot)
	 (self (+ tot (expt (car xs) 2)) (cdr xs))))
   0 args))
    
(def vec-norm (vec)
  (vec-scale vec (/ (apply quad-add vec))))


;others

(def fact (num)
  "factorial of num (num must be a fixnum)"
  (if (or (< num 0)(no:isa num 'int)) (err "num must be a positive integer")
      ((afn (n x)
	 (if (<= n 1)
	     x
	     (self (- n 1) (* n x))))
       num 1)))

(def n-bessel (n (o terms 100))
  "gives a fn for the nth bessel function of the first kind evaluated at x"
  (fn (x)
    (with (i 0 
	   tot 0)
      (while (< i terms)
	     (++ tot (/ (* (expt -1 i) (expt (/ x 2) (+ n i i)))
			(* (fact i) (fact (+ n i)))))
	     (++ i 1))
      tot)))

(def mean lis
  ((afn (tot i x . xs)
     (if no.xs (/ (+ tot x) (+ i 1))
	 (self (+ tot x) (+ i 1) car.xs cdr.xs)))
   0 0 car.lis cdr.lis))

(def std-dev lis
  (let m mean.lis
    (sqrt:/ (apply + (map [expt (- _ m) 2] lis))
	    len.lis)))

(def choose (n x)
  "number of ways of picking n elements from x (ie no. of ways of mixing 2 different sets of identical objects of size n and (- x n))"
  (if (> n x)
      (do (prn "")(pr n)(pr " is greater than ")(prn x))
      (/ (fact x) (* (fact n) (fact (- x n))))))

(def gauss-random (sigma (o middle 0))
  "aproximation to a gausian distributed random with width sigma around middle, max possible deviation +/- 1000sigma"
  (let tot 0
    (for i 0 999
      (++ tot  (- (* (rand) 2.0) 1)))
    (+ (* tot (/ sigma 25)) middle)))

(def quad-roots (a b c)
  "returns roots of the equation ax²+bx+c=0"
  (let sqroot (sqrt (- (* b b) (* 4 a c)))
    (rem-dups
     (list (/ (- sqroot b) 2 a)
	   (/ (- 0 sqroot b) 2 a)))))
