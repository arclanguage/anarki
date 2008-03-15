; settable-fn.arc
; by AmkG
; adds the concept of "attachments", which are key-value
; pairs attached to an object.
; Some keys are then made to work with a few redefined arc
; "builtins"

; An ordinary fn can be used as if it were a readable
; table:
;   (= tbl
;    (let ....
;     (fn (k) ...)))
;   tbl!foo
; However, normally assignments cannot be made:
;   (= tbl!foo 42) ; error!
; By using attachments and this library, you can attach
; any arbitrary function as the setter for that function:
;   (= tbl
;     (let ...
;       (= writer (fn (value k) ...))
;       (= reader (fn (k) ...))
;       (add-attachment
;         '= writer
;         reader)))
; Then the form:
;   (= tbl!foo 42)
; will call the writer:
;   (writer 42 'foo)
#|
Predefined attachments:

  '= (fn (v . args) ...)
A function which acts as the setter for the main object.
The arguments to the function are passed via args, while
the value to be assigned is passed as the first argument.
The valuse is passed as the first argument in order to
support setterfunctions with variable number of arguments.

 'keys (fn () ...)
A function which returns a list of valid keys for the main
object.  Must return a list.  Note that this function takes
zero arguments, on the assumption that you've already bound
or curried the main object in this function.  If you want
to create a table-like object, you will generally want to
attach this.

 'len (fn () ...)
A function which returns a number specifying the number of
elements in the main object.  Must return a number.  Note
that again this function takes zero arguments, on the
assumption that you've already bound or curried the main
object in this function.  If you want to create a sequence
object, you will generally want to attach this.


In spite of its name, settable-fn is not limited to
functions.  You can attach anything to any object, although
be warned that most of the basic Arc functions don't
actually work with tagged objects yet.

|#
;
; NOTE.  Experimental.  Use at your own risk.
; That said, I would like to know if anyone finds anything
; that might be a bug.
;
; Please report bugs you don't want to fix yourself to:
;  almkglor@gmail.com
; or throw it on the arclanguage forum

; NOTE: requires arc-wiki ($ ...) and redef macros

(let (attached tagged) nil
  (= attached
     (fn (s)
       (and (($ vector?) s)
            (is (($ vector-ref) s 0) 'tagged)
            (> (($ vector-length) s) 3))))
  (def get-attachment (k s)
    " Determines if an object is attached to the key `k' of the
      object `s'.
      Currently used for assignable collection-like objects, and
      may be used for other purposes besides.
      See also [[add-attachment]] [[type]] "
    (if
      (attached s)
        ((($ vector-ref) s 3) k)
      (isnt s (rep s))
        (get-attachment k (rep s))))
  (def add-attachment (k v s)
    " Attaches `v' to the key `k' of the object `s'.
      Currently used for assignable collection-like objects, and
      may be used for other purposes besides.
      Other references to the original object `s' may not be
      valid after executing this function.
      See also [[add-attachments]] [[get-attachment]] [[annotate]] "
    (if (attached s)
          (do (= ((($ vector-ref) s 3) k) v)
            s)
        ; else
          (($ vector) 'tagged (type s) (rep s) (fill-table (table) (list k v))))))

(def add-attachments args
  (let s
       (let p args
         (while (cdr:cdr p)
           (= p (cdr p)))
         (do1 (cadr p)
           (= (cdr p) nil)))
    ((afn (args s)
       (if (no args)
         s
         (self (cdr:cdr args) (add-attachment (car args) (cadr args) s))))
     args s)))

(redef ref (c . ind)
  (if (isnt (type c) (type (rep c))) (apply ref (rep c) ind)
      (isa c 'fn) (apply c ind)
      (old c (car ind))))

; Have to redefine these so they get the new `ref'
(= (call* 'cons) ref)
(= (call* 'cons) ref)
(= (call* 'table) ref)
(= (call* 'vec) ref)
(= (call* 'fn) ref)

(redef sref (c v . rest)
  (aif (get-attachment '= c)
       (apply it v rest)
       (apply old c v rest)))

(redef maptable (f tb)
  (aif (get-attachment 'keys tb)
    (do
      (each k (it)
        (f k (tb k)))
      tb)
    (old f tb)))

(redef keys (h)
  (aif (get-attachment 'keys h)
    (it)
    (old h)))

(redef len (x)
  (aif (get-attachment 'len x)
    (it)
    (old x)))

#|to allow crazy stuff like this:
(= f (file-table "/some/path"))
(= b f:urlencode)
(= (b "~/*stuff?") "nah-uh!")
|#
(let old (rep compose)
  (= compose
     (annotate 'mac
       (fn args
         (w/uniq (ff sub) ;first function, subsequent functions
           `(with (,ff  ,(car args)
                   ,sub ,(apply old (cdr args)))
              (add-attachments
                '= (fn (v . s) (= (,ff (apply ,sub s)) v))
                'keys (fn () (keys ,ff))
                (annotate (type ,ff)
                  (fn args (,ff (apply ,sub args))) ))))))))

(= *test-settable-fn
  (let (x y) nil
    (add-attachment '=
      (fn (v s)
        (case s
          x (= x v)
          y (= y v)
            (err:string "Disallowed key: " s)))
      (fn (s)
        (case s
          x x
          y y)))))

(= *test-fake-table
  (add-attachments
    'keys (fn () (list 'x 'y))
    '= (fn (v k) (err "Locked table, assignment not allowed!"))
    (annotate 'table
      (fn (k)
        (case k
          x "The x symbol"
          y "The y symbol")))))

(def *test-the-fake-table ()
  (each val *test-fake-table
    (prn val)))

