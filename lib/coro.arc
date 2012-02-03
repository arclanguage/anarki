; I haven't thought deeply enough about the proper semantics here, so please
; don't rely on this library. - rntz <daekharel@gmail.com> (2010-01-01)

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


; generators - coroutines which take no input and can be iterated over
(def genwrap (func (o default-sym))
  " Returns a generator `gen'. When called, `gen' calls `func' with a `yield' fn
    and returns whatever `func' calls `yield' with. Successive calls continue
    `func' and return the results of `func''s successive calls to `yield'. `gen'
    may also be passed an argument; if `func' terminates, this argument is
    returned instead of nil.

    Generators can be iterated over with 'walk and 'each.

    See also [[cowrap]] [[cofngen]] "
  (annotate 'generator
            (cowrap (fn (yield) (fn _ (func yield)))
                    (fn ((o endsym default-sym)) endsym))))

(set-coercer 'fn 'generator rep)

(extend walk generator
  (fn (seq . _) (isa seq 'generator))
  (fn (gtor func) (w/uniq done
                    (afnwith ()
                      (let x (gtor done)
                        (unless (is x done)
                          (func x)
                          (self)))))))

(mac ngen (name . body)
  " Creates a generator executing `body' which uses `name' to yield.
    See also [[gen]] [[genwrap]] "
  `(genwrap (fn (,name) ,@body)))

(mac gen body
  " Creates a generator executing `body' which uses 'yield to yield.
    See also [[ngen]] [[genwrap]]"
  `(ngen yield ,@body))

(mac defgen (name args . body)
  " Defines a function `name' which returns a generator executing `body' that
    uses 'yield to yield.
    See also [[gen]] [[genwrap]] [[coro]] "
  `(def ,name ,args
     (gen ,@body)))

(defgen genwalk (l)
  " Turns anything that can be `walk'ed into a generator.
    See also [[walk]] [[gen]] [[genwrap]] "
  (walk l yield))
