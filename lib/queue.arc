(def queue ()
  (annotate 'queue (list nil nil 0)))

(def enq (obj qq)
  (let q rep.qq
    (atomic
      (++ q.2)
      (if (no q.0)
        (= q.1 (= q.0 list.obj))
        (= (cdr q.1)  list.obj
           q.1        (cdr q.1)))
      q.0)))

(def deq (qq)
  (let q rep.qq
    (atomic (unless (is 0 q.2) (-- q.2))
            (pop q.0))))

(def qlen (q) (rep.q 2))

(defmethod len(x) queue
  (qlen x))

(def qlist (q) (car rep.q))

(def enq-limit (val q (o limit 1000))
  (atomic
     (unless (< (qlen q) limit)
       (deq q))
     (enq val q)))
