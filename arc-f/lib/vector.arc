
; NOTE: Not necessarily needed for other implementations, i.e.
; other implementations may validly use built-in types.  We
; can't safely use vectors on the mzscheme implementations,
; because we do a lot of trickery on the mzscheme side and use
; scheme vectors for that trickery.
; Potentially this can implemented on the mzscheme side using
; the same trickery of course.
; A second rationale for this is that it's also a demonstration
; of the usage of the "scanner" concept in building the language.

(in-package vector)
(using <arc>v3)
(using <arc>v3-scanner)
(interface v1
  vector)
(interface v2 <vector>v1
  vector-of)

(= len-tag (uniq))

(defcall vector (v i)
  v.i)
(defm sref ((t v vector) val i)
  (let v (rep v)
    (if (> i v.len-tag)
        (= v.len-tag (+ i 1)))
    (= v.i val)
    val))

(def vector rest
  " Creates a vector.  A vector is a sequence with O(1) index lookup.
    See also [[vector-of]] "
  (unscan-vector rest))

(def vector-of (n (o objs))
  " Creates a vector of `objs' of length `n'.
    See also [[vector]] "
  (collect-on
    (fn (collect)
      (for i 0 (- n 1)
        (collect objs)))))

(defm scanner ((t v vector))
  (w/collect:for i 0 (- v.len-tag 1)
    (collect v.i)))
(defm unscan ((t v vector) s)
  (unscan-vector s))

(def unscan-vector (s)
  (collect-on
    (fn (collect)
      (each i s
        (collect i)))))

; optimization
(defm <base>each ((t v vector) skip bf)
  (withs (v (rep v)
          l v.len-tag)
    ((afn (i)
       (if (and (< i l) (bf:v i))
           (self (+ i 1))))
     skip)))
(defm <base>collect-on ((t v vector) bf)
  (collect-on bf))

(def collect-on (bf)
  (with (l  0
         rv (table))
    (bf (fn (i)
          (= rv.l i)
          ++.l
          i))
    (= rv.len-tag l)
    (annotate 'vector rv)))

(defm len ((t v vector))
  ((rep v) len-tag))


