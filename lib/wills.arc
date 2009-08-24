; Simplified interface to plt scheme's will executors. Ignores the fiddly bits
; about "will executors" and wills not being automatically executed, by using
; one global will executor and creating a thread to handle wills. Also avoids
; the problem that if multiple wills are registered for a given value, only one
; can be run per value per gc cycle, by adding another layer of indirection.

(unless (and bound!will-executor* ($:will-executor? ,will-executor*))
  (= will-executor* ($.make-will-executor)))

(unless (and bound!will-table* (isa will-table* 'table))
  (= will-table* ($.make-hash-table 'weak)))

(unless (and bound!will-executor-thread*
             (isa will-executor-thread* 'thread)
             (~dead will-executor-thread*))
  (= will-executor-thread*
     (thread (while t ($.will-execute will-executor*)))))

(def generic-will (val)
  (each f will-table*.val (f x)))

(def will-register (val will)
  ; if val is not yet registered with a generic will, do so
  ($.hash-table-get will-table* val
                    (fn () ($.will-register will-executor* val generic-will)))
  (push will will-table*.val)
  val)

(mac will (val . body)
  (w/uniq ign
    `(will-register ,val (fn (,ign) ,@body))))
