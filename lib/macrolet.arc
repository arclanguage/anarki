; macrolet.arc
; by AmkG
; scoped macros implementation
;  (macrolet
;    ( (foo) nil
;      (bar x y) `(= ,x ,y))
;    (bar z (foo)))

(mac macrolet (macroforms . body)
  (withs
     (
      macros
       (fill-table (table)
         (mappend
           (fn (((macroname . parms) expr))
             (list macroname (eval `(fn ,parms ,expr))))
           (pair macroforms)))
      scanner
        (afn (x)
          (if (and (acons x) (isnt (car x) 'quote))
               (aif (macros (car x))
                 (self (apply it (cdr x)))
                 (map self (macex x)))
              x)))
   `(do ,@(map scanner body))))

(def *macrolet-test1 ()
  (macrolet
     (
       (myassign x y) `(= ,x ,y))
     (let z 1
       (prn "z before: " z)
       (myassign z 42)
       (prn "z after: " z)
       z)))

