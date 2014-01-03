($:namespace-undefine-variable! '_walk)
(defgeneric walk (seq f)
  ((afn (l)
     (when acons.l
       (f car.l)
       (self cdr.l)))
   seq))

(defmethod walk (seq f) table
  (maptable (fn (k v)
              (f (list k v)))
            seq))

(defmethod walk (seq f) string
  (forlen i seq
    (f seq.i)))

(def tree (x)
  (annotate 'tree x))

(defmethod walk (seq f) tree
  (let x rep.seq
    (f x)
    (unless (atom x)
      (walk (tree car.x) f)
      (walk (tree cdr.x) f))))

(redef ontree (f x)
  (walk (tree x) f))
