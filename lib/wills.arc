; simplified interface to plt scheme's will executors. ignores the fiddly bits
; about "will executors" and wills not being automatically executed, by using
; one global will executor and creating a thread to handle wills.

(unless (and bound!will-executor* ($:will-executor? ,will-executor*))
  (= will-executor* ($.make-will-executor)))

(unless (and bound!will-executor-thread*
             (isa will-executor-thread* 'thread)
             (~dead will-executor-thread*))
  (= will-executor-thread*
     (thread (while t ($.will-execute will-executor*)))))

(def will-register (val will)
  ($.will-register will-executor* val will)
  val)

(mac will (val . body)
  (w/uniq ign
    `(will-register ,val (fn (,ign) ,@body))))
