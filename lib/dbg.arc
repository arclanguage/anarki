(= *debug nil
   *env   (obj))

(def *envwipe ()
  (each k (keys *env)
    (wipe (*env k))))

(def tlread (prompt)
  (w/stdout original-stdout*
    (w/stdin original-stdin*
      (disp prompt)
      (flushout)
      (read))))

(def tlerr (c)
  (prn (details c))
  c)

(def dbg-stats ()
  (pp (sorted (map [list _.1 _.0] (tablist *fncounts)))))

(def dbg-wipestats ()
  (each k (keys *fncounts)
    (= (*fncounts k) nil)))

(def dbg-exnenv (env c)
  (catch
    (each x (stacktrace c)
      (if (env x)
          (throw (env x))))
    nil))

(assign exn* nil)
(assign dbgexpr* nil)

(def dbg-restore (tbl)
  (when tbl
    (*envwipe)
    (maptable (fn (k v) (sref *env v k)) tbl)
    *env))

(def dbg-copy (tbl)
  (let new (table)
    (maptable (fn (k v) (sref new v k)) tbl)
    new))

(def dbg-copyenv ()
  (dbg-copy *env))

(def call-w/dbg (f)
  (if *debug
       (f)
       (let prev (dbg-copyenv)
         (*envwipe)
         (assign *debug t)
         (after
           ;(f)
           (on-err (fn (c) (let name (*fn)
                             (let e (dbg-copyenv)
                               (w/pushnew c exn* 
                                 (dbg-restore e)
                                 (debugger c)))))
                               ;(if (~posmatch "cannot reference undefined identifier" (details c))
                               ;  (debugger c)
                               ;  (tlerr c)))))
                   f)
           (do
             (dbg-restore prev)
             (assign *debug nil))))))

(def pprint (x)
  (seval!pretty-print x)
  x)

(mac w/dbg body
  `(call-w/dbg (fn () ,@body)))

(def dbg-pps xs
  (apply string
    (intersperse #\space
      (map [trim:tostring:pprint _] xs))))

(def dbg-slot (slot)
  (when (>= len.slot 2)
    (list slot.0 (if (is (type slot.1) 'fn)
                     (slot.1)
                     slot.1))))

(def dbg-locals (lenv)
  (map dbg-slot lenv))

(def dbg-prn (lenv retexpr)
  (when (> (len exn*) 0)
    (tlerr (car exn*)))
  ;(iflet stack (stacktrace) ;(at (stacktrace) 0 (pos 'dbg-prn (stacktrace)))
  ;       (prn "debugging at: " (dbg-pps (nthcdr (+ (pos 'debugger stack) 1) stack))))
  (whenlet trace (assoc '*stacktrace lenv)
    (prn "debugging at: " (dbg-pps (dbg-slot trace))))
  (pr "locals:")
  (each (name val) (dbg-locals lenv)
    (prn)
    (prblue name)
    (pr " " (dbg-pps val)))
  (prn)
  (prn "type 'h to see this printout")
  (prn "type 'c to continue")
  (when retexpr
    (pr  "type 'v to see value of ") (prnblue (dbg-pps retexpr))))

(assign dbgenv* nil)

(def dbg-eval (e expr lenv)
  (let prev (dbg-copy e)
    (assign dbgenv* (dbg-copyenv))
    (dbg-restore e)
    (eval expr lenv)))

(def dbg-prexpr (e lenv expr (o printer) (o o (stdout)) (o i (stdin)))
  (let result (w/stdout o (w/stdin i (dbg-eval e expr lenv)))
    (if printer
      (printer expr result)
      (prnred (dbg-pps result)))
    (= thatexpr expr)
    (= that result)
    result))

;(def dbgerr (c)
;  (prn:details c)
;  (map pp stacktrace.c)
;  (map pp fulltrace.c)
;  c)

(assign dbgerr debugger:tlerr)

(def debugger (lenv (o retexpr) (o o (stdout)) (o i (stdin)))
  (w/stdin original-stdin*
    (w/stdout original-stdout*
      (let e (dbg-copy (or dbgenv* *env))
        (dbg-restore e)
        (assign dbgenv* nil)
        (assign *debug nil)
        (when (is (type lenv) 'exception)
          ;(= lenv (lexenv lenv)))
          (= lenv (dbg-exnenv e lenv)))
        (when (is (type lenv) 'sym)
          (= lenv (e lenv)))
        (when (is (type lenv) 'fn)
          (= lenv (lenv)))
        (dbg-prn lenv retexpr)
        (let done nil
          ((afn ()
             (on-err (fn (c)
                         ;(dbgerr c)
                         (if *debug
                             (w/pushnew c exn*
                               (debugger c))
                             (do
                               (tlerr c)
                               (if done
                                   nil
                                   (self)))))
                     (fn ()
             (let expr (tlread "> ")
               (assign dbgexpr* expr)
               (if (iso expr ''c)
                   (do (= done t)
                       (dbg-prexpr e lenv retexpr
                                   (fn (expr result)
                                     (when expr
                                       (prnblue (dbg-pps expr))
                                       (pr "  returned ")
                                       (prnred (dbg-pps result))))
                                   o i))
                   (iso expr ''v)
                    (do (dbg-prexpr e lenv retexpr)
                        (self))
                   (iso expr ''h)
                     (do (dbg-prn lenv retexpr)
                         (self))
                   (do (prnred (dbg-pps (dbg-eval e expr lenv)))
                       (if (iso dbgexpr* ''c)
                           (do
                             (assign dbgexpr* nil)
                             (dbg-prn lenv retexpr)))
                       (self)))))))))))))



