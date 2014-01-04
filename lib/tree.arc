; different ways to navigate trees
(def tree (x)
  (annotate 'tree x))

(defextend walk (seq f) (isa seq 'tree)
  (let x rep.seq
    (f x)
    (unless (atom x)
      (walk (tree car.x) f)
      (walk (tree cdr.x) f))))

(defextend map (f seq) (isa seq 'tree)
  (withs (old rep.seq
          new f.old)
    (if (or atom.old (~is old new))
      new
      (cons (map f (tree car.old))
            (map f (tree cdr.old))))))

(def leaves (x)
  (annotate 'leaves x))

(defextend walk (seq f) (isa seq 'leaves)
  (let x rep.seq
    (if (atom x)
      (f x)
      (do (walk (leaves car.x) f)
          (walk (leaves cdr.x) f)))))

(defextend reduce (f base seq) (isa seq 'leaves)
  (let x rep.seq
    (if (atom x)
      base
      (f (reduce f base (leaves car.x))
         (reduce f base (leaves cdr.x))))))

(defextend counts (seq)  (isa seq 'leaves)
  (counts:flat rep.seq))
