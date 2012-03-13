; from http://bitbucket.org/fallintothis/trace/src/tip/trace.arc

; function & macro tracing

(= trace-level* 0)
(= trace-indent* nil)
(= traced* (table))

(def traced (f (o name (fn-name f)))
  (check-traceable f)
  (let orig f
    (annotate (type orig)
      (fn args
        (++ trace-level*)
        (trace-enter name args)
        (after (trace-exit name (apply (rep orig) args))
               (-- trace-level*))))))

(mac trace fs
  `(do1 nil ,@(map (fn (f)
                     `(if (no (traced* ',f))
                          (do (prn "*** tracing " ',f)
                              (make-traced ,f))
                          (prn "*** already traced " ',f)))
                   fs)))

(mac make-traced (name)
  (check-traceable-name name)
  (w/uniq f
    `(let ,f (traced ,name ',name)
       (= (traced* ',name) ,name
          ,name ,f))))

(mac untrace fs
  `(do1 nil ,@(map (fn (f)
                     `(if (traced* ',f)
                          (do (prn "*** untracing " ',f)
                              (make-untraced ,f))
                          (prn "*** already untraced " ',f)))
                   (or fs (keys traced*)))))

(mac make-untraced (name)
  `(= ,name (traced* ',name)
      (traced* ',name) nil))

(def indent () (sp (indent-amount)))

(def indent-amount ()
  (* (or trace-indent* 0) (- trace-level* 1)))

(def trace-enter (name args)
  (indent)
  (pr trace-level* ". Trace: ")
  (pprn-elastic (cons name args)))

(def trace-exit (name result)
  (indent)
  (pr trace-level* ". Trace: " name " ==> ")
  (pprn-elastic result)
  result)

(def pprn-elastic (expr)
  (let broke (w/stdout (outstring) (ppr expr))
    (if broke (prn))
    (ppr expr (if broke (indent-amount) 0)) ; xxx gross to redo ppr
    (prn)))

(def check-traceable-name (name)
  (if (~isa name 'sym)
      (err "Not a function or macro name:" name)
      (~bound name)
      (err "Can't trace unbound symbol:" name)))

(def check-traceable (f)
  (unless (in (type f) 'fn 'mac)
    (err "Can only trace functions and macros:" f)))

; gross, but more-or-less effective

(def fn-name (f)
  (let repr (tostring:disp (case (type f) fn f mac (rep f)))
    (if (headmatch "#<procedure:" repr)
        (sym:trim (cut repr 12 -1))
        f))) ; ppr uses write, not disp, so (sym:tostring:disp f) has bars
