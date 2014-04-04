;;;; For information on streams see chapter 3.5 of SICP

;;;; May want to use defextend to modify langauge
;;;; so that streams are accessed in the same way lists are
;;;; car/cdr would work on streams and then extend other operations
;;;; such as map, rem, keep, and find
;;;; would also need to tag all of the streams which may
;;;; cause some efficency issues

(mac delay (exp)
  "delays the evaluations of exp"
  `(memo (fn () ,exp)))

(def force (exp)
  "evaluates the delayed expression exp"
  (exp))

;;; change names of scar and scdr so they can be used for streams
(when (~bound 'set-car) (= set-car scar))
(when (~bound 'set-car) (= set-cdr scdr))

(= snil '())

(mac scons (x y)
  "conses x onto the stream (lazy seq) y"
  `(cons ,x (delay ,y)))

(def scar (xs)
  "gets the first element of the stream (lazy seq) xs
   same name as scar in the standard language which sets the car
   original scar is moved to set-car"
  (car xs))

(def scdr (xs)
  "gets the rest of the stream (lazy seq) xs
   same name as scdr in the standard language which sets the cdr
   original scdr is moved to set-cdr"
  (force:cdr xs))

(def sno (xs)
  "checks whether xs is the empty stream"
  (no xs))

(def sprn (xs (o n -1))
  "prints the stream xs
   if n is specified prints the first n elements
   otherwise just prints the whole stream
   WARNING: have to be careful because possibly infinite streams
   does not print streams of streams correctly"
  (if (or (sno xs) (is n 0))
      (do (prn "()") snil)
      (do (pr "(" (scar xs))
	(afnwith (xs (scdr xs) n (- n 1))
	  (if (or (is n 0) (sno xs))
	      (prn ")")
	      (do (pr " " (scar xs))
		  (self (scdr xs) (- n 1)))))))
  xs)

(def snth (xs n)
  "gets the nth element of the stream xs"
  (if (sno xs)
        snil
      (is n 0)
        (scar xs)
      'else
        (snth (scdr xs) (- n 1))))

(def srange (a b)
  "generates a stream of the range [a,b]"
  (if (> a b)
      snil
      (scons a (srange (inc a) b))))

(def integers-starting-from (n)
  "creates the infinite stream of integers starting from n"
  ;; might want to change this so it doens't use memoization
  (scons n (integers-starting-from:inc n)))

(def smap (f xs)
  "maps f over the stream xs"
  (if (sno xs)
      snil
      (scons (f:scar xs) (smap f (scdr xs)))))

(def srem (test xs)
  "removes all of the elements from xs that satisfy test"
  (let f (testify test)
    (afnwith (xs xs)
      (if (sno xs)
	    snil
	  (f:scar xs)
	    (self:scdr xs)
	  'else
	    (scons (scar xs) (self:scdr xs))))))

(def skeep (test xs)
  "keeps all of the elements that satisfy test"
  (srem (complement (testify test)) xs))

(def sfind (test xs)
  "find the first element of the stream xs that satisfies test"
  (let f (testify test)
    (afnwith (xs xs)
      (if (sno xs)
	    nil
	  (f:scar xs)
	    (scar xs)
	  'else
	    (self:scdr xs)))))
