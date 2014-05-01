(def queue ()
"A queue is like a list but with efficient insertion in one end and deletion
in the other."
  (annotate 'queue (list nil nil 0)))

(def enq (obj qq)
"Insert 'obj' into queue 'qq'."
  (let q rep.qq
    (atomic
      (++ q.2)
      (if (no q.0)
        (= q.1 (= q.0 list.obj))
        (= (cdr q.1)  list.obj
           q.1        (cdr q.1)))
      q.0)))

(def deq (qq)
"Delete last element from queue 'qq' and return it."
  (let q rep.qq
    (atomic (unless (is 0 q.2) (-- q.2))
            (pop q.0))))

(defextend len (q) (isa q 'queue)
  rep.q.2)

(defcoerce cons queue (q)
  rep.q.0)

(def enq-limit (val q (o limit 1000))
"Like [[enq]], but never let the queue 'q' grow larger than 'limit'."
  (atomic
     (unless (< len.q limit)
       (deq q))
     (enq val q)))
