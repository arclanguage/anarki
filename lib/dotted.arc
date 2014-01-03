(def dotted (x)
  (annotate 'dotted x))

(def map (f . seqs)  (isa car.seqs 'dotted)
  (let xs (map1 rep seqs)
    (if (no cdr.xs)
         (map1 f car.xs)
        (some atom xs)
         (apply f (map1 carif xs))
        (all idfn xs)
          (cons (apply f (map1 car xs))
                (apply map f (map1 dotted:cdr xs))))))

;(prn (map + (dotted '(1 2 3)) (dotted '(1 2 . 3))))
