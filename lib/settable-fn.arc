; settable-fn.arc
; by AmkG
; NOTE.  Experimental.  Use at your own risk.
; That said, I would like to know if anyone finds anything
; that might be a bug.

; NOTE: requires arc-wiki ($ ...) macro

(let (attached tagged) nil
  (= attached
     (fn (s)
       (and (($ vector?) s)
            (is (($ vector-ref) s 0) 'tagged)
            (> (($ vector-length) s) 3))))
  (= tagged
     (fn (s)
       (and (($ vector?) s)
            (is (($ vector-ref) s 0) 'tagged)
            (is (($ vector-length) s) 3))))
  (def get-attachment (k s)
    " Determines if an object is attached to the key `k' of the
      object `s'.
      Currently used for assignable collection-like objects, and
      may be used for other purposes besides.
      See also [[add-attachment]] [[type]] "
    (if (attached s)
        ((($ vector-ref) s 3) k)))
  (def add-attachment (k v s)
    " Attaches `v' to the key `k' of the object `s'.
      Currently used for assignable connection-like objects, and
      may be used for other purposes besides.
      Other references to the original object `s' may not be
      valid after executing this function.
      See also [[get-attachment]] [[annotate]] "
    (if (attached s)
          (do (= ((($ vector-ref) s 3) k) v)
            s)
        (tagged s)
          (($ vector) 'tagged (($ vector-ref) s 1) (($ vector-ref) s 2) (fill-table (table) (list k v)))
        ; else
          (($ vector) 'tagged (type s) s (fill-table (table) (list k v))))))

(let oldsref sref
  (= sref
    (fn (c v . rest)
      (aif (get-attachment '= c)
        (apply it v rest)
        (apply oldsref c v rest)))))

(= *test-settable-fn
  (let (x y) nil
    (add-attachment '=
      (fn (v s)
        (case s
          x (= x v)
          y (= y v)
            (err:string "Disallowed key: " s)))
      (fn (s)
        (case s
          x x
          y y)))))

