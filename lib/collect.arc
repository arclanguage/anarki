; List comprehensions -- Kartik Agaram -- http://arclanguage.org/item?id=20381
;
; Idea suggested by musk_fan (http://arclanguage.org/item?id=20375).
; This implementation inspired by Common Lisp's 'iterate macro (https://common-lisp.net/project/iterate)
; as described by Michael Malis (http://malisper.me/loops-in-lisp-part-3-iterate).

(mac collect (expr . guards)
"Creates a list of data based on a sort of 'set builder' notation.
https://en.wikipedia.org/wiki/List_comprehension"
  (w/uniq gacc
    `(accum ,gacc
        ,(apply ingest
                (join (map collect-transform guards)
                      (list `(,gacc ,expr)))))))

(examples collect
  (collect i (for i from 1 to 3))
  (1 2 3)
  (collect (* i 2) (for i from 1 to 3))
  (2 4 6)
  (collect (list i j)
           (for i from 1 to 3)
           (for j from 1 to 3)
           (if (< i j)))
  ((1 2) (1 3) (2 3)))

(def ingest lists
  (when lists
    (ret x car.lists
      (when cdr.lists
        (let lastx lastcons.x
          (= cdr.lastx
             (list (apply ingest cdr.lists))))))))

(def collect-transform (guard)
  (if (is 'for guard.0)
        (if (is 'from guard.2)
          (if (is 'down guard.4)
                ; (for _ from _ down to _)
                `(down ,guard.1 ,guard.3 ,guard.6)
              'else
                ; (for _ from _ to _)
                `(up ,guard.1 ,guard.3 ,guard.5))
          'else
            guard)  ; regular for
      ; insert other syntax here
      'else
        guard))
