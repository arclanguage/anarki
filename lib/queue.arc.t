(suite queues
       (setup q1 (ret q (queue)
                   (enq 1 q)
                   (enq 2 q)
                   (enq 3 q))
              q2 (ret q (queue)
                   (enq 1 q)
                   (enq 2 q)
                   (enq 3 q)))
       (test iso (assert-t (iso q1 q2)))
       (test len (assert-same 3 (len q1))))

