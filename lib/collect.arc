; List comprehensions -- Kartik Agaram -- http://arclanguage.org/item?id=20381
;
; Idea suggested by musk_fan (http://arclanguage.org/item?id=20375).
; This implementation inspired by Common Lisp's 'iterate macro (https://common-lisp.net/project/iterate)
; as described by Michael Malis (http://malisper.me/loops-in-lisp-part-3-iterate).

(mac collect (expr . guards)
"Creates a list of data based on a sort of 'set builder' notation.
https://en.wikipedia.org/wiki/List_comprehension"
  ; TODO: Rename `gacc-from-collect` back to `gacc` and remove the
  ; debug printing once the Travis CI tests pass.
  (w/uniq gacc-from-collect
    ( [do (prn "blah collect expansion result:") (write _) (prn) _]
      `(accum ,gacc-from-collect
          ,(apply ingest
                  (join (map collect-transform guards)
                        (list `(,gacc-from-collect ,expr))))))))

(examples collect
  ; TODO: Remove the debug printing once the Travis CI tests pass.
  (do
    (prn "blah compiled collect example 1, now executing it")
    (do (pr "declarations* = ") (write declarations*) (prn))
    (collect i (for i from 1 to 3)))
  (1 2 3)
  (do
    (prn "blah compiled collect example 2, now executing it")
    (do (pr "declarations* = ") (write declarations*) (prn))
    (collect (* i 2) (for i from 1 to 3)))
  (2 4 6)
  ; TODO: See if we should remove this example again once the
  ; Travis CI tests pass.
  (do
    (prn "blah compiled collect example 3 (created for debugging), now executing it")
    (do (pr "declarations* = ") (write declarations*) (prn))
    (collect (* i 2) (for i from 1 to 3) (if (< i 3))))
  (2 4)
  (do
    (prn "blah compiled collect example 4 (originally 3), now executing it")
    (do (pr "declarations* = ") (write declarations*) (prn))
    (collect (list i j)
            (for i from 1 to 3)
            (for j from 1 to 3)
            (if (< i j))))
  ((1 2) (1 3) (2 3)))

(def ingest lists
  (when lists
    (ret result car.lists
      (when cdr.lists
        (nappend result (apply ingest cdr.lists))))))

(def collect-transform (guard)
  (if (is 'for guard.0)
        (or (case len.guard
              5
                ; regular syntax: (for _var _init _term _update)
                guard
              4
                ; (for _ in _)
                (if (is 'in guard.2)
                  `(each ,guard.1 ,guard.3))
              6
                ; (for _ from _ to _)
                (if (and (is 'from guard.2) (is 'to guard.4))
                  `(up ,guard.1 ,guard.3 ,guard.5))
              7
                ; (for _ from _ down to _)
                (if (and (is 'from guard.2) (is 'down guard.4) (is 'to guard.5))
                  `(down ,guard.1 ,guard.3 ,guard.6)))
            (err "don't understand how to handle " guard " in collect macro"))
      ; insert other syntax here
      'else
        guard))
