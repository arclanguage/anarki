;;;Math Library

;;Int (predicate)
(def int (n)
  "Returns true if n is an integer; i.e. has no decimal part."
  (is (trunc n) n)) ; return true if the number is the same as itself without the decimal part

(def floor (x)
  "Returns the largest integer less than or equal to x."
  (if (int x)
      x
    (< x 0)
      (- (trunc x) 1)
      (trunc x)))

(def ceil (x)
  "Returns the smallest integer greater than or equal to x."
  (if (int x)
      x
    (< x 0)
      (trunc x)
      (+ (trunc x) (if (int x) 0 1))))

(defmemo fac (n)
  "Returns n! = n * (n - 1) * (n - 2) * ... * 3 * 2 * 1."
  ((afn (n a)
     (if (> n 1)
         (self (- n 1) (* a n))
         a))
   n 1))

(def sin (x)
  "Returns the sine of x in radians."
  ($ (sin ,x)))

(def cos (x)
  "Returns the cosine of x in radians."
  ($ (cos ,x)))

(def tan (x)
  "Returns the tangent of x in radians."
  ($ (tan ,x)))

(def asin (x)
  "Returns the arcsine of x in radians."
  ($ (asin ,x)))

(def acos (x)
  "Returns the arccosine of x in radians."
  ($ (acos ,x)))

(def atan (x)
  "Returns the arctangent of x in radians."
  ($ (atan ,x)))

(def log (x)
  "Returns the natural log of x."
  ($ (log ,x)))

(def log10 (x)
  "Returns the log base 10 of x."
  (/ (log x) (log 10)))

;;Greatest Common Denominator
(def gcd l
  "returns the greatest common denominator, or divisor, of a list of numbers. Numbers should be integers,
   or the result will default to 0."
  (with (a (car l) c (cdr l))
    (if (len> c 1) 
          (gcd a (gcd (car c) (cdr c))) ; handle lists of more than two numbers
        (no a) 0
        (no c) (abs a)
        (let b (car (flat c))
          (if (or (~int a) (~int b)) 0 ; skip non-integers
              (is a b) a ; return common divisor
              (if (> a b)
                    (gcd (- a b) b)
                  (gcd (- b a) a)))))))
