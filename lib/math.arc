;matrix fns

(def init-matrix(dims initval)
  (zap flat dims)
  (if cdr.dims
    (n-of car.dims (init-matrix cdr.dims initval))
    (n-of car.dims initval)))

(def matrix-rank (mat)
  (if atom.mat 0
      (let rec (afn (M r)
		 (if (atom car.M) r
		     (self car.M (+ r 1))))
	(rec mat 1))))

(def zeros dims
  (init-matrix dims 0))

(def ones dims
  (init-matrix dims 1))

(def mat-to-table (mat)
  "coerces a list-of-lists representation of a matrix into a hash table one where the key is a list of indices (0 referenced)"
  (let ans (table)
       (let rec (afn (X pre)
		  (if (atom:car X)
		      (for i 0 (- len.X 1)
			 (= (ans (cons i pre)) X.i))
		      (for i 0 (- len.X 1)
			 (self X.i (cons i pre)))))
	    (rec mat (list)))
       ans))


(mac elt (mat pos)
  "access the element of the matix given by co-ords listed in pos"
  (if (cdr pos) 
      `(elt (,mat ,(car pos)) ,(cdr pos))
      `(,mat ,(car pos))))

(def matrix-multiply (a b)
  "multiplies the matrix a by the matrix b  (N.B. Rank one row vectors need to be in the form ((a b c d ...)) /NOT/ (a b c d), i.e. initiated with dims=(1 n) not (n))"
  (with (col (fn (mat i) (map [_ i] mat))
	 result-matrix (list))
    (for rw 0 (- len.a 1)
          (push (let result-row (list)
		  (for cl 0 (- (len b.0) 1)
		       (push (apply + (map * a.rw (col b cl))) result-row))
		  rev.result-row)
		result-matrix))
    rev.result-matrix));could be generalised as a recursive macro to work on arbitrary rank tensors? 

(def gauss-elim (mat rhs)
  "solves the linear equations:

mat_00*x_0 + mat_01*x_1 ... mat_0n*x_n = rhs_0
mat_10*x_0 + mat_11*x_1 ... mat_1n*x_n = rhs_1
...
mat_n0*x_0 + mat_n1*x_1 ... mat_nn*x_n = rhs_n

using gaussian elimination and returns a list of x's (N.B. not efficient for large sparce matrices)"
  (zap flat rhs)
  (withs (N (if (no (is len.mat (len car.mat))) (err "mat must be a square matrix, use co-effs of 0 for equations which dont have a certain variable in them")
	     len.mat)
	  tmp (list)
	  MAX 0
	  i 0 j 0 k 0
	  X (zeros N))
    (for i 0 (- N 1) (push (join mat.i (cons rhs.i ())) tmp))
    (let M rev.tmp
      ;elimination step
      (loop (= i 0) (<= i (- N 1)) (++ i) 
       (= MAX i)
       (loop (= j (+ i 1)) (<= j (- N 1)) (++ j)
        (if (> (abs:elt M (j i)) (abs:elt M (MAX i))) (= MAX j)))
       (if (isnt MAX i) (swap M.i M.MAX))
       (loop (= j (+ i 1)) (<= j (- N 1)) (++ j)
	  (loop (= k N) (>= k i) (-- k)
	   (-- (elt M (j k)) (/ (* (elt M (i k)) (elt M (j i))) (elt M (i i))))))) ;;end of elimination step
      ;;substitution
      (loop (= j (- N 1)) (>= j 0) (-- j) 
	    (= tmp 0.0)
	    (loop (= k (+ j 1)) (<= k(- N 1)) (++ k) (++ tmp (* (elt M (j k)) X.k)))
	    (= X.j (/ (- (elt M (j N)) tmp) (elt M (j j))))))
    X))


;calculus fns

(def deriv (f)
  "provides differential of a function of a single vairable"
  (fn (x)
    (let dx (if (is x 0) 1d-9 (abs:* x 1d-9))
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
      (let dx (if (is ,x 0) 1d-9 (abs:* ,x 1d-9))
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
	(let dx (if (is ,x 0) 1d-9 (abs:* ,x 1d-9))
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
  "returns the integral of a single argument function using Simpson's method"
  (fn (lower upper (o its 100))
    (withs (dx      (/ (- upper lower) its)
	    x       lower
	    current (f x)
	    next    (f (+ x dx))
	    accum   0) 
      (while (<= x (- upper dx))
	     (++ accum (/ (+ (* (+ current next) 1/2 dx)
			     (* 2 dx (f (+ x (/ dx 2)))))
			  3))
	     (++ x dx)
	     (= current next)
	     (= next (f:+ x dx)))
      accum)))


; vector fns

(def vec-dot (v1 v2)
  (apply + (map (fn (x y) (* x y)) v1 v2)))

(def vec-cross (v1 v2 . args)
  (if (car args)(vec-cross (vec-cross v1 v2) (car args) (cdr args))
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

(def square (x) (* x x))
(def cube (x) (* x x x))
(def floor (n) (trunc n))
(def ceiling (n) (int ($.ceiling n)))
(def highbyte (n) (floor (/ n 256)))
(def lowbyte (n) (mod n 256))

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
  "returns roots of the equation ax^2+bx+c=0"
  (let sqroot (sqrt (- (* b b) (* 4 a c)))
    (if (is sqroot 0) (list (/ (- b) 2 a))
	(list (/ (- sqroot b) 2 a)
	      (/ (- 0 sqroot b) 2 a)))))
