;; SICP 3.5
;; lazy streams are ideal for short-circuiting pipelines, using infinite
;; lists, generators, etc.

;; Create lazy streams using either lazy-cons or lazy-gen.

;; Default list operations should handle lazy streams, but will usually return
;; regular lists. Variants here with lazy- prefix will instead transform them into
;; other lazy streams.

(mac delay (expr)
  "delays the evaluation of expr"
  `(memo (fn () ,expr)))

(def force (delayed-fn)
  "evaluates a delayed expression"
  (delayed-fn))

(mac lazy-cons (first second-expr)
  "create a lazy stream that delays evaluation of second-expr"
  `(annotate 'lazy-stream (cons ,first (delay ,second-expr))))

(defextend car (xs) (isa xs 'lazy-stream)
  "gets the first element of the lazy stream xs"
  (car rep.xs))

(defextend cdr (xs) (isa xs 'lazy-stream)
  "gets the rest of the lazy stream xs"
  (force (cdr rep.xs)))

(defcoerce cons lazy-stream (s)
  "convert a lazy stream into a regular (eager) list"
  (accum acc
    (drain (do (acc car.s)
               (zap cdr s)))))

; comparing two lazy streams requires reifying them
(defextend iso (x y)  (or (isa x 'lazy-stream)
                          (isa y 'lazy-stream))
  (iso (as cons x)
       (as cons y)))

(def lazy-gen (f)
  "turn a generator function f into a lazy stream"
  (lazy-cons (f) (lazy-gen f)))

(defcoerce lazy-stream fn (f)
  "generates a lazy stream from repeated calls to a generator function f"
  (lazy-gen f))

;; helpers for lazy streams of numbers

(def lazy-range (a b)
  "generates a stream of the range [a,b]"
  (if (<= a b)
    (lazy-cons a
               (lazy-range inc.a b))))

(def integers-from (n)
  "creates the infinite stream of integers starting from n"
  ;; might want to change this so it doens't use memoization
  (lazy-cons n (integers-from inc.n)))

;; variants of common list operations that return lazy streams
;; often all you have to do is replace cons in a recursive definition with lazy-cons

(def lazy-map (f xs)
  "lazily maps f over the stream xs
   invariant: (as cons (lazy-map f lazy-stream)) == (map f lazy-stream)"
  (if xs
    (lazy-cons (f car.xs)
               (lazy-map f cdr.xs))))

(def lazy-rem (test xs)
  "lazily rem elements of xs satisfying test"
  (let f (testify test)
    ((afn (s)
       (if (no s)       nil
           (f car.s)    (self cdr.s)
                        (lazy-cons car.s (self cdr.s))))
      xs)))

(def lazy-keep (test xs)
  "lazily keep elements of xs satisfying test"
  (lazy-rem (complement testify.test) xs))

; no need for lazy-find because it short-circuits anyway
