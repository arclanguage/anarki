(def thread-cell ((o init-val))
  " Creates a thread-local storage cell with initial value `init-val'.
    Mutating a cell in one thread does not affect its value in others.
    See also [[thread-cell-get]] [[thread-cell-put]] "
  ($.make-thread-cell init-val))

(def thread-cell-get (cell)
  " Retrieves `cell's value in the current thread.
    See also [[thread-cell-put]] [[thread-cell]] "
  ($.thread-cell-ref cell))

(def thread-cell-put (cell val)
  " Changes `cell's value in the current thread (and only the current thread).
    See also [[thread-cell-get]] [[thread-cell]] "
  (($ thread-cell-set!) cell val)
  val)

(defcall thread-cell (cell)
  (thread-cell-get cell))

(extend sref thread-cell
  (fn (ob val . _) (isa ob 'thread-cell))
  (fn (ob val . _) (thread-cell-put ob val)))
