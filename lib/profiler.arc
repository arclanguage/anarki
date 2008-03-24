; profiler.arc -- born on 2008/03/23
; Ray Myers <ray.myers@gmail.com>

(= *profiled-functions nil)
(= *profiler-old-definitions (table))
(= *profiler-stack nil)
(= *profiler-times (table))
(= *profiler-child-time 0)

(def profiler-reset ()
  (= *profiler-times (table))
  (= *profiler-stack nil)
  (each name *profiled-functions
    (= (*profiler-times name) 0))
  (= *profiler-child-time 0))

(def profiler-report ()
  (prn (sort (fn (a b) (> (a 1) (b 1)))
             (tablist *profiler-times)))
  nil)

(mac profile names
  (join '(do) (map [list 'profile-function _] names) '(nil)))

(mac unprofile names
  (join '(do) (map [list 'unprofile-function _] names) '(nil)))

;; --- The rest are for internal use. ---

(mac profile-function (name)
  (let old (uniq)
    `(unless (find ',name *profiled-functions)
       (= (*profiler-times ',name) 0)
       (let ,old ,name
            (= (*profiler-old-definitions ',name) ,old)
            (push ',name *profiled-functions)
            (def ,name args
              (profiler-enter ',name)
              (let ret (apply ,old args)
                (profiler-exit)
                ret))))))

(mac unprofile-function (name)
  `(when (find ',name *profiled-functions)
     (= ,name (*profiler-old-definitions ',name))
     (pull ',name *profiled-functions)))

(def profiler-enter (name)
  (push (list name (msec)) *profiler-stack))

(def profiler-exit ()
  (let (name ms) (pop *profiler-stack)
    (let total (- (msec) ms)
      (++ (*profiler-times name)
          (- total *profiler-child-time))
      (= *profiler-child-time 
         (if *profiler-stack total 0)))))
