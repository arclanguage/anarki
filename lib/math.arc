;;;Math Library

;;Int (predicate)
(def int (n)
  "Returns true if n is an integer; i.e. has no decimal part."
  (is (trunc n) n)) ; return true if the number is the same as itself without the decimal part

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
