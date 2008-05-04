; cached-table.arc
; by AmkG
; a cached table


(require "lib/settable-fn.arc")

(let (arguments) nil
  (= arguments
     (fn (args . profile)
       (zap pair args)
       (let r nil
         (each (var defval) (pair profile)
           (push
             (aif (alref args var)
               it
               defval)
             r))
         (rev r))))
  (def cached-table args
    (with (
           ; access time table
           mt (table)
           ; actual table
           ct (table)
           ; cleanup function
           cleanup nil
           (cachetime) (arguments args 'cachetime 7200))
      (= cleanup
         (fn ()
           (let tm (seconds)
             ; avoid using ontable, because
             ; we're mutating the table
             (each k (keys mt)
               (when (> (- tm (mt k)) cachetime)
                 (= (mt k) nil (ct k) nil))))))
      (add-attachments
        '= (fn (v s)
             (atomic
               (= (mt s) (seconds))
               (cleanup))
             (= (ct s) v))
        'keys (fn ()
                  (atomic (cleanup)) (keys ct))
        (annotate 'table
          (fn (s)
            (atomic
              (cleanup)
              (aif (ct s)
                   (atomic
                     (= (mt s) (seconds))
                     it)))))))))

