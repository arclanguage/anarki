(test "iso works on queues"
  (with (q1 (queue)
         q2 (queue))
     (enq 1 q1)
     (enq 1 q2)
     (enq 2 q1)
     (enq 2 q2)
     (enq 3 q1)
     (enq 3 q2)
     (iso q1 q2)))

(test-iso "len works on queues"
  3
  (let q (queue)
    (enq 1 q)
    (enq 2 q)
    (enq 3 q)
    len.q))
