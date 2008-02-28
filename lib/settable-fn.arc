; settable-fn.arc
; by AmkG
; NOTE.  Experimental.  Use at your own risk.
; That said, I would like to know if anyone finds anything
; that might be a bug.

; NOTE: requires arc-wiki ($ ...) and redef macros

(let (attached tagged) nil
  (= attached
     (fn (s)
       (and (($ vector?) s)
            (is (($ vector-ref) s 0) 'tagged)
            (> (($ vector-length) s) 3))))
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
      Currently used for assignable collection-like objects, and
      may be used for other purposes besides.
      Other references to the original object `s' may not be
      valid after executing this function.
      See also [[add-attachments]] [[get-attachment]] [[annotate]] "
    (if (attached s)
          (do (= ((($ vector-ref) s 3) k) v)
            s)
        ; else
          (($ vector) 'tagged (type s) (rep s) (fill-table (table) (list k v))))))

(def add-attachments args
  (let s
       (let p args
         (while (cdr:cdr p)
           (= p (cdr p)))
         (do1 (cadr p)
           (= (cdr p) nil)))
    ((afn (args s)
       (if (no args)
         s
         (self (cdr:cdr args) (add-attachment (car args) (cadr args) s))))
     args s)))

(redef sref (c v . rest)
  (aif (get-attachment '= c)
       (apply it v rest)
       (apply old c v rest)))

(redef maptable (f tb)
  (aif (get-attachment 'keys tb)
    (do
      (each k (it)
        (f k (tb k)))
      tb)
    (old f tb)))

(redef keys (h)
  (aif (get-attachment 'keys h)
    (it)
    (old h)))

(redef len (x)
  (aif (get-attachment 'len x)
    (it)
    (old x)))

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

(= *test-fake-table
  (add-attachments
    'keys (fn () (list 'x 'y))
    '= (fn (v k) (err "Locked table, assignment not allowed!"))
    (annotate 'table
      (fn (k)
        (case k
          x "The x symbol"
          y "The y symbol")))))

(def *test-the-fake-table ()
  (each val *test-fake-table
    (prn val)))

