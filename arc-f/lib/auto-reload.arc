
(in-package auto-reload)
(using <arc>v3)
(interface v1
  auto-reload
  reload-interval*)

(= reloads* nil
   reload-mtimes* (table)
   reload-interval* 1
   reload-thread* nil)

(def loadq (path)
  (tostring:load path)
  nil)

(def auto-reload paths
  (atomic
    (each p paths
          (loadq p)
          (push p reloads*)
          (= (reload-mtimes* p) (mtime p)))
    (if (no reload-thread*) (= reload-thread* (thread (reload-check))))))

(def reload-check ()
  (sleep reload-interval*)
  (each p reloads*
    (when (isnt (reload-mtimes* p) (mtime p))
      (= (reload-mtimes* p) (mtime p))
      (prn "*** [" (cut (tostring:system "date +%H:%M:%S") 0 -1)
           "] reloading " p)
      (loadq p)))
  (reload-check))


