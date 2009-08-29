(require "lib/util.arc")

; simpler than point and robust against changes in it, given that 'point
; indicates that the form it produces must only be used as an escape procedure.
(mac lcc (var . body)
  " Binds the current contination to `var' within `body'.
    See also [[point]] "
  `(ccc (fn (,var) ,@body)))

(def yielder (update consume)
  " Returns a fn that, when called, captures and calls `update' on its
    continuation, then applies `consume' to its arguments. A building block
    for coroutines in arc.
    See also [[cowrap]] "
  (fn args
    (lcc cont
      (update cont)
      (apply consume args))))

(def cowrap (func (o end nilfn))
  " `func' is called with a fn `yield' to produce a fn `inner'. 'cowrap returns
    a fn `wrapped' that, when called with input, gives it to `inner', then
    returns what `inner' subsequently calls `yield' with. If `inner' does not
    call `yield', `wrapped' returns the result of calling `end' on the input
    instead.
    See also [[yielder]] [[ccc]] "
  (let (enter exit done) nil
    (withs (yield (yielder (fn (k) (assign enter k))
                           (fn args (apply exit args)))
            outer (yielder (fn (k) (assign exit k))
                           (fn input
                             (assign done (fn () (apply end input)))
                             (apply enter input)
                             ; inner did not call yield.
                             ; can't just use compose without wrapping in fn
                             ; because values of 'exit and 'done can change.
                             ((= enter (fn _ (exit:done)))))))
      (= enter (func yield))
      outer)))

(mac cofn (name args . body)
  " Creates a coroutine which yields via `name', takes `args' as initial input,
    and executes `body'.
    See also [[yfn]] [[cowrap]] [[cofngen]] "
  `(cowrap (fn (,name) (fn ,args ,@body))))

(mac yfn (args . body)
  " Creates a coroutine which yields via 'yield, takes `args' as initial input,
    and executes `body'.
    See also [[cofn]] [[cowrap]] [[yfngen]] "
  `(cofn yield ,args ,@body))

(mac cofngen (name args . body)
  " Creates a coroutine generator which takes `args' and returns a coroutine
    which yields via `name', takes no initial input, and executes `body'.
    See also [[cofn]] [[yfngen]] "
  `(fn ,args (cofn ,name () ,@body)))

(mac yfngen (args . body)
  " Creaes a coroutine generator which takes `args' and returns a coroutine
    which yields via 'yield, takes no initial input, and executes `body'.
    See also [[yfn]] [[cofngen]] "
  `(cofngen yield ,args ,@body))

(mac coro (name args . body)
  " Defines a coroutine-generator `name' which takes `args' and returns a
    coroutine that yields via 'yield, takes no initial input, and executes
    `body'.
    See also [[yfn]] [[yfngen]] [[cowrap]] "
  `(def ,name ,args
     (yfn () ,@body)))
