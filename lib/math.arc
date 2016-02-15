(= e           2.71828182845904523536
   pi          3.14159265358979323846
   phi         (/ (+ 1 (sqrt 5)) 2)
   euler-gamma 0.577215664901532860606)

;;should this be in util.arc?
(mac iterators (its . body)
  "its is a list of (var max) tuples which are iterated over eg (iterators ((x 2)(y 4)) (pr x)(prn y)) ->
00
01
02
03
10
11
12
13"
  (if car.its
    (if (> cadar.its 0)
      `(up ,(caar its) 0 ,(cadar its)
         (iterators ,(cdr its) ,@body))
      `(up ,(caar its) ,(cadar its) 0
         (iterators ,(cdr its) ,@body)))
    `(do ,@body)))


;matrix fns

(def init-matrix (dims initval)
  (zap flat dims)
  (if cdr.dims
    (n-of car.dims (init-matrix cdr.dims initval))
    (n-of car.dims initval)))

(def matrix-order (mat)
  (if atom.mat 0
    (let rec (afn (M r)
               (if (atom car.M)
                 r
                 (self car.M (+ r 1))))
      (rec mat 1))))

(def zeros dims
  (init-matrix dims 0))

(def ones dims
  (init-matrix dims 1))

(mac elt (mat pos)
  "access the element of the matix given by co-ords listed in pos"
  (if (cdr pos)
    `(elt (,mat ,(last pos)) ,(butlast pos))
    `(,mat ,(car pos))))

(def mat-to-table (mat)
  "coerces a list-of-lists representation of a matrix into a hash table one where the key is a list of indices (0 referenced)"
  (with (ans (table) keys (list))
    (let rec (afn (X pre)
               (if (atom:car X)
                 (up i 0 len.X
                   (= (ans (cons i pre)) X.i)
                   (push (cons i pre) keys))
                 (up i 0 len.X
                   (self X.i (cons i pre)))))
      (rec mat (list)))
    (= ans!dims (map [+ _ 1] (car:sort (fn (x y) (> (apply + x) (apply + y))) keys)))
    ans))

(def table-to-mat (tab)
  "coerces a table representation of a matrix into a list-of-lists one"
  (let key-val-lis (list)
    (maptable (fn (key val) (if (isnt key 'dims) (push (cons key val) key-val-lis))) tab)
    (let rec (afn (lis)
               (if (is (len caar.lis) 1)  (map cdr (sort (fn (x y) (< caar.x caar.y)) lis)) ;if there is only one index sort by it
                 (map (fn (tp) (self:map [cons (butlast car._) cdr._] tp))            ;
                        (tuples (sort (fn (x y) (< (last car.x) (last car.y))) lis) ; if there is more than one index: collect into tuples of the rightmost index and list the answers of rec on each tuple
                                (len:keep [is (last car._) 0] lis)))))              ;
      (rec key-val-lis))))

(def matrix-minor (mat indices (o table? t))
  "creates the minor the the element of mat with the indices specified, i.e. if the indices are (i j k ...) then it is the matrix which excludes elements in the ith row or jth column or kth depth etc"
  (if (acons mat) (zap mat-to-table mat))
  (= indices (firstn (len mat!dims) indices))
  (let ans (mat-to-table:init-matrix (map [- _ 1] mat!dims) 0)
    (maptable (fn (key val)
                (if (~acons key)  nil
                    (~all t (map (fn (k i) (isnt k i)) key indices))  nil
                    :else  (= (ans (map (fn (k i) (if (> k i) (- k 1) k)) key indices)) val)))
              mat)
    (if table? ans
      (table-to-mat ans))))

(def det (mat)
  "calculates the determinant of a matrix by cramer's rule: det(A)=sum_over_i(A(i,j) * det(A_minor(i,j)) * -1^(i+j)); det(scalar)=scalar"
  (if (acons mat) (zap mat-to-table mat))
  (if (nand (is (len mat!dims) 2) (is (car mat!dims) (cadr mat!dims)))
    (err "can only calculate the determinant for a square matrix of order 2"))
  (if (and (is (car  mat!dims) 1)
           (is (cadr mat!dims) 1))
    (mat '(0 0))
    (ret ans 0
      (up i 0 (car mat!dims)
        (++ ans (* (mat (list 0 i))
                   (expt -1 i)
                   (det:matrix-minor mat (list 0 i))))))))

(def ident-matrix (order size (o table? nil))
  "creates an identity maxtrix of number of dimensions order and of size size"
  (let M (mat-to-table (zeros (n-of order size)))
   (up i 0 size
      (= (M (n-of size i)) 1))
    (if table? M
      (table-to-mat M))))

(def matrix-multiply (a b)
  "multiplies the matrix a by the matrix b (N.B. Order one row vectors need to be in the form ((a b c d ...)) /NOT/ (a b c d), i.e. initiated with dims=(1 n) not (n))"
  (if (isa a 'table) (zap table-to-mat a))
  (if (isa b 'table) (zap table-to-mat b))
  (with (col  (fn (mat i) (map [_ i] mat))
         result-matrix  (list))
    (up rw 0 len.a
      (push (let result-row (list)
              (up cl 0 (len b.0)
                (push (apply + (map * a.rw (col b cl))) result-row))
              rev.result-row)
            result-matrix))
    rev.result-matrix))  ; could be generalised as a recursive macro to work on arbitrary order tensors?

(def gauss-elim (mat rhs)
  "solves the linear equations:

mat_00*x_0 + mat_01*x_1 ... mat_0n*x_n = rhs_0
mat_10*x_0 + mat_11*x_1 ... mat_1n*x_n = rhs_1
...
mat_n0*x_0 + mat_n1*x_1 ... mat_nn*x_n = rhs_n

using gaussian elimination and returns a list of x's (N.B. not efficient for large sparce matrices)"
  (zap flat rhs)
  (if (acons mat) (zap mat-to-table mat)) ;assumes if using list-of-lists representation of matrices you arent worried about efficiency and so wont mind inline conversion
  (withs (MAX  0
          tmp  0
          X  (if (is (car mat!dims) (cadr mat!dims) len.rhs) (zeros (car mat!dims))
               (do (pr "car.dims:")(pr (car mat!dims))(pr " ")(pr " cadr.dims:")(pr (cadr mat!dims))(pr " ")(pr "rhs:")(prn rhs)(err "mat must be a square matrix of the same size as rhs, use 0 elements for equations which dont feature a variable")))
          N (car mat!dims))
    (let M (copy mat)
      (up i 0 N
        (= (M (list N i)) rhs.i))
      ;;elimination step - manipulates the matrix so all elements below the diagonal are zero while maintaining the relation between variables and co-efficients
      (up i 0 N
        (= MAX i)
        (up j (+ i 1) N
          (if (> (abs:M (list i j)) (abs:M (list i MAX))) (= MAX j)))
        (up k i (+ N 1)
          (= tmp (M (list k i)))
             (M (list k i)) (M (list k MAX))
             (M (list k MAX)) tmp)
        (up j (+ i 1) N
          (down k N (- i 1)
            (-- (M (list k j)) (/ (* (M (list k i)) (M (list i j)))
                                  (M (list i i)))))))
      ;;sub step - starting from bottom element which just has one co-efficient and can therefore be easily calculated sub in the value of the know variables and solve
      (down j (- N 1) -1
        (= tmp 0)
        (up k (+ j 1) N
          (++ tmp (* X.k (M (list k j)))))
        (= X.j (/ (- (M (list N j)) tmp)
                  (M (list j j)))))
      X)))


;calculus fns

(def deriv (f)
  "provides differential of a function of a single vairable"
  (fn (x)
    (let dx (if (is x 0) 1d-9 (abs:* x 1d-9))
      (/ (- (f (+ x dx))
            (f (- x dx)))
         2 dx))))

(mac partial-diff (f n arity)
  "returns deriv function of f (w/ num args ARITY) wrt Nth variable(from 0)"
  (withs (arglis  (n-of arity (uniq))
          a-lis  (firstn n arglis)
          x  arglis.n
          b-lis  (nthcdr (+ n 1) arglis))
    `(fn ,arglis
       (let dx (if (is ,x 0) 1d-9 (abs:* ,x 1d-9))
         (/ (- (,f ,@a-lis (+ ,x dx) ,@b-lis)
               (,f ,@a-lis (- ,x dx) ,@b-lis))
            2 dx)))))

(mac partial-diff-vec (f n arity)
  "returns vector with each element differentiated with respect to Nth argument"
  (withs (arglis  (n-of arity (uniq))
          a-lis  (firstn n arglis)
          x  arglis.n
          b-lis  (nthcdr (+ n 1) arglis))
    `(fn ,arglis
       (let dx (if (is ,x 0)  1d-9  (abs:* ,x 1d-9))
         (vec-scale (vec- (,f ,@a-lis (+ ,x dx) ,@b-lis)
                          (,f ,@a-lis (- ,x dx) ,@b-lis))
                    (/ 1 2 dx))))))

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
    (if (> lower upper) (err "upper must be > lower"))
    (withs (dx      (/ (- upper lower) its)
            dx/2    (/ dx 2)
            x       lower
            current (f x)
            next    (f (+ x dx))
            accum   0)
      (while (<= x (- upper dx))
        (++ accum (/ (+ (* (+ current next) dx/2)
                        (* 2 dx (f (+ x dx/2))))
                     3))
        (++ x dx)
        (= current next)
        (= next (f:+ x dx)))
      accum)))

(def adaptive-integral (f)
  (let inte (memo:integral f)
    (afn (lower upper (o tol 0.001))
      (with (a (inte lower upper 2)
             b (inte lower upper 8))
        (if (and (isnt b 0) (< (abs:/ (- a b) b) tol))
          b
          (let half (+ lower (/ (- upper lower) 2))
            (+ (self lower half tol) (self half upper tol))))))))



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
    (if no.args
      v1
      (reduce v+ (cons v1 args))))

  (def vec- (v1 . args)
    (if no.args
      (map - v1)
      (v- v1 (apply vec+ args))))
)


(def vec-scale (vec . scalars)
  (let c (apply * scalars)
    (map [* _ c] vec)))

(def quad-add args
  ((afn (tot xs)
     (if no.xs
       sqrt.tot
       (self (+ tot (expt car.xs 2)) cdr.xs)))
   0 args))

(def vec-norm (vec)
  (vec-scale vec (/:apply quad-add vec)))


;others

(def square (x)  (* x x))
(def cube (x)  (* x x x))
(def e^ (x)  (expt e x))
(def floor (n)  (trunc n))
(def ceiling (n)  (int ($.ceiling n)))
(def highbyte (n)  (floor (/ n 256)))
(def lowbyte (n)  (mod n 256))


(def mean lis
  ((afn (tot i x xs)
     (if no.xs
       (/ (+ tot x) (+ i 1))
       (self (+ tot x) (+ i 1) car.xs cdr.xs)))
   0 0 car.lis cdr.lis))

(def geo-mean lis
  ((afn (tot i x xs)
     (if no.xs
       (expt (* tot x) (/:+ i 1))
       (self (* tot x) (+ i 1) car.xs cdr.xs)))
   1 0 car.lis cdr.lis))

(def std-dev lis
  (let m (apply mean lis)
    (sqrt:/ (apply + (map [square:- _ m] lis))
            len.lis)))


(def gauss-random (sigma (o mu 0))
  "gausian distributed random with width sigma around mu"
  (withs (u (rand)
          v (* 1.7156 (- (rand) 0.5))
          x (- u 0.449871)
          y (+ abs.v 0.386595)
          q (+ (* x x) (* y (- (* 0.196 y) (* 0.25472 x)))))
    (while (and (> q 0.27597)
                (or (> q 0.27846) (> (* v v) (* -4 log.u u u))))
      (= u (rand)
         v (* 1.7156 (- (rand) 0.5))
         x (- u 0.449871)
         y (+ abs.v 0.386595)
         q (+ (* x x) (* y (- (* 0.196 y) (* 0.25472 x))))))
    (+ mu (/ (* sigma v) u))))


(def quad-roots (a b c)
  "returns roots of the equation ax^2+bx+c=0"
  (let sqroot (sqrt (- (* b b) (* 4 a c)))
    (if (is sqroot 0)
      (list (/ (- b) 2 a))
      (list (/ (- sqroot b) 2 a)
            (/ (- 0 sqroot b) 2 a)))))


;special functions

(def ln-gamma (x)
  "calculates the natural log of gamma, lots of magic numbers from numerical recipes: third edition"
  (let consts '(57.1562356658629235 -59.5979603554754912 14.1360979747417471 -0.491913816097620199 .339946499848118887e-4 .465236289270485756e-4 -.983744753048795646e-4 .158088703224912494e-3 -.210264441724104883e-3 .217439618115212643e-3 -.164318106536763890e-3 .844182239838527433e-4 -.261908384015814087e-4 .368991826595316234e-5)
    (with (y   x
           tmp (+ x 671/128)
           ser 0.999999999999997092)
      (= tmp (- (* log.tmp (+ x 0.5)) tmp))
      (map [++ ser (/ _ (++ y))] consts)
      (+ tmp (log:/ (* 2.5066282746310005 ser) x)))))

(def gamma (x)
  (e^:ln-gamma x))

(let rec (memo:afn (n x)
           (if (<= n 1)
             x
             (self (- n 1) (* n x))))
 (def fact (num)
   "factorial of num (num must be a fixnum)"
   (if (or (< num 0)(no:isa num 'int))
     (err "num must be a positive integer")
     (rec num 1))))

(defmemo ln-fact (num)
  "natural log of num!"
  (if (or (< num 0)(no:isa num 'int))
    (err "num must be a positive integer")
    (ln-gamma (+ num 1))))

(def bin-coef (n k)
  "number of ways of picking n elements from k (ie no. of ways of mixing 2 different sets of identical objects of size n and (- k n))"
  (if (< n k)   (err "n > k in bin-coef")
      (< n 171)  (/ (fact n) (* (fact k) (fact (- n k)))) ; 170! is largest factorial which is represented exactly as a double-float
      :else  (floor:+ 0.5 (e^:- ln-fact.n ln-fact.k (ln-fact:- n k)))))

(def beta (z w)
  "returns the value of the beta function B(z,w)"
  (e^ (- (+ ln-gamma.z ln-gamma.w)
         (ln-gamma (+ z w)))))


(def Jn-bessel (n (o terms 100))
  "gives a fn for the nth bessel function of the first kind evaluated at x"
  (fn (x)
    (with (i 0
           tot 0)
      (while (< i terms)
        (++ tot (/ (* (expt -1 i) (expt (/ x 2) (+ n i i)))
                   (* fact.i (e^:ln-gamma (+ n i 1)))))
        (++ i 1))
      tot)))

(def Yn-bessel (n (o terms 100))
  "gives a fn for the nth bessel function of the second kind evaluated at x"
  (let J (Jn-bessel n terms)
    (fn (x)
      (with (JNx  (J x terms)
             Cnpi (cos (* n pi)))
        (/ (- (* JNx Cnpi) (* (expt -1 n) JNx))
           Cnpi)))))

(def In-bessel (n (o terms 100))
  "gives a function for the nth modified bessel function of the first kind"
  (let J(Jn-bessel n terms)
    (fn (x)
      (* (expt -i n)
         (J:* i x) terms))))

(def Kn-bessel (n (o terms 100))
  "gives a function for the nth modified bessel function of the second kind"
  (with (J (Jn-bessel n terms)
         Y (Yn-bessel n terms))
    (fn (x)
      (* (/ pi 2) (expt +i (+ n 1))
         (+ (J (* +i x)) (* +i (Y (* +i x))))))))

(def jn-spherical-bessel (n)
  "creates a function for the nth spherical bessel function of the first kind"
  (let J (Jn-bessel (+ n 1/2))
    (fn (x)
      (* (sqrt:/ pi 2 x)
         J.x))))

(def yn-spherical-bessel (n)
  "creates a function for the nth spherical bessel function of the first kind"
  (let Y (Yn-bessel (+ n 1/2))
     (fn (x)
       (* (sqrt:/ pi 2 x)
          Y.x))))

(def P-legendre (l m)
  "creates a function for the Legendre polynomial P_m,l(x) with an optional second argument for re-normalising by the spherical harmonics co-efficients (from Numerical recipes 3rd edition section 6.7)"
  (fn (x (o renorm? nil))
    (let pmm 1.0
      (if (> m 0)
        (with (omx2 (* (- 1 x) (+ 1 x))
               fac 1.0)
          (up i 0 m
            (= pmm (/ (* pmm omx2 fac) (+ 1 fac)))
            (++ fac 2))))
      (= pmm (sqrt:/ (* pmm 2 (+ m 1)) (* pi 4)))
      (if (isnt m 0) (= pmm (* pmm -1)))
      (if (is l m)
        (if remorm? pmm (* pmm (sqrt:/ (* 4 pi (fact (+ l m))) (* (+ l l 1) (fact (- l m))))))
        (with (pmmp1 (* x pmm (sqrt:+ (* 2 m) 3))
               ll 0)
         (if (is l (+ m 1))
           (if remorm? pmmp1 (* pmmp1 (sqrt:/ (* 4 pi (fact (+ l m))) (* (+ l l 1) (fact (- l m))))))
           (with (oldfac (sqrt:+ 3 (* 2 m))
                  fac 0 pll 0)
             (up ll (+ m 2) 2  ; todo: is 2 the right upper bound? see git history
               (= fac (sqrt:/ (- (* 4 ll ll) 1) (- (* ll ll) (* m m)))
                  pll (* fac (/ (- (* x pmmp1) pmm) oldfac))
                  oldfac fac
                  pmm pmmp1
                  pmmp1 pll))
             (if remorm? pll (* pll (sqrt:/ (* 4 pi (fact (+ l m))) (* (+ l l 1) (fact (- l m)))))))))))))

(def sphere-harm (l m)
  "creates a function for the spherical harmonic Y_l,m(theta phi)"
  (let P_lm (P-legendre l m)
    (fn (theta phi)
      (* (P_lm (cos theta) t) (e^:* i m phi)))))

(def sine-integral (x (o terms 100))
  "returns the value of the sine integral Si(x)=integral from 0 to x sin(t)/t dt"
  (let X abs.x
    (for i 3  (<= ((- i 1) 2) terms)  (++ i 2)
      (++ X (/ (* (expt X i) (expt -1 (mod i 2))) i fact.i)))
    (if (< x 0) (- X) X)))

(def cos-integral (x (o terms 100))
  "returns the value of the cosine integral Ci(x)=euler_gamma+ln(x)+integral from 0 to x (cos(t)-1)/t dt"
  (with (X (abs x) tot 0)
    (for i 2  (< (/ i 2) terms)  (++ i 2)
      (++ tot (/ (* (expt -1 (mod (/ i 2) 2)) (expt X i)) i fact.i)))
    (++ tot (+ log.x euler-gamma))
    (if (< x 0)  (- X (* +i pi))  X)))


;probability distributions

(def poisson-dist (lambda)
  "returns a function for the probability of k discrete, uncorrelated events occuring in a time where the mean expected events is lambda"
  (fn (k)
    (if (or (< k 0) (no:isa k 'int))
      (err "k in poisson dist must be a non-negative integer"))
    (* (/ (expt lambda k) fact.k) (e^ (- lambda)))))

(def beta-dist (a b)
  "beta probability distribution defined over the unit interval 0<x<1, a&b > 0"
  (if (or (< b 0) (< a 0)) (err "alpha and beta must both be non-negative"))
  (fn (x)
    (if (no:< 0 x 1)
      (err "x must be between 0 and one"))
    (* (/:beta a b) (expt x (- a 1)) (expt (- 1 x) (- b 1)))))

(def binomial-dist (n p)
  "returns a function for the binomal distribution, gives the probability of k events with probability p out of n occuring"
  (if (no:<= 0 p 1)
    (err "p is a probability and muxt be between 0 and 1"))
  (fn (k)
    (* (bin-coef n k) (expt p k) (expt (- 1 p) (- n k)))))


;data-fitting

(def least-squares-linear (data)
  "data is expected in the form ((x1 y1)(x2 y2)...) returns list of co-efficients for powers of x in acsending order"
  (if (< len.data 2) (err "cannot fit to less than 2 points"))
  (withs (xs (map car  data)
          ys (map cadr data)
          fs (list (map (fn (x) 1) xs)
                   (map (fn (x) x) xs))
          A (mat-to-table (init-matrix '(2 2) 0))
          B (map [vec-dot _ ys] fs))
    (up i 0 2
      (up j 0 2
        (= (A (list i j)) (vec-dot fs.i fs.j))))
    (gauss-elim A B)))

(def least-squares-quadratic (data)
  "data is expected in the form ((x1 y1)(x2 y2)...) returns list of co-efficients for powers of x in acsending order"
  (if (< len.data 3) (err "cannot fit to less than 3 points"))
  (withs (xs (map car  data)
          ys (map cadr data)
          fs (list (map (fn (x) 1) xs)
                   (map (fn (x) x) xs)
                   (map (fn (x) square.x) xs))
          A (mat-to-table (init-matrix '(3 3) 0))
          B (map [vec-dot _ ys] fs))
    (up i 0 3
      (up j 0 3
        (= (A (list i j)) (vec-dot fs.i fs.j))))
    (gauss-elim A B)))

(def least-squares-custom (data . fns)
  "data is expected in the form ((x1 y1)(x2 y2)...), fns must each accept 1 argument, returns list of co-efficients for powers of x in acsending order"
  (if (< len.data len.fns) (err "cannot fit to less points than component functions"))
  (withs (xs (map car  data)
          ys (map cadr data)
          fs (map [map _ xs] fns)
          A (mat-to-table (init-matrix (list len.fns len.fns) 0))
          B (do (prn fs) (prn ys) (map [vec-dot _ ys] fs)))
    (map prn (list xs ys fns fs A B))
    (up i 0 len.fns
      (up j 0 len.fns
        (= (A (list i j)) (vec-dot fs.i fs.j))))
    (gauss-elim A B)))
