(suite queues
       (setup q1 (queue 1 2 3)
              q2 (queue 1 2 3)
              empty-queue (queue))
       (test iso (assert-t (iso q1 q2)))
       (test len (assert-same 3 (len q1)))
       (test empty-queue (assert-same 0 (len empty-queue))))
