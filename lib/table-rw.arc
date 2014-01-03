; taken from http://awwx.ws/table-rw3.arc with slight modifications
; - Mark Huetsch

(require "lib/scheme.arc")
(require "lib/skipwhite.arc")

(def parse-table-items (port (o acc (table)))
  (scheme.skip-whitespace port)
  (if (is (peekc port) #\})
       (do (readc port) acc)
       (with (k (read port)
              v (read port))
         (= (acc k) v)
         (parse-table-items port acc))))

(extend-readtable #\{ parse-table-items)

; need the errsafe on type tests because (type x) croaks on
; non-Arc types

(defextend ac-literal (x) (errsafe:isa x 'table)
  scheme-t)

(def print-table (f x s)
  (scheme.display "{" s)
  (between (k v) x (scheme.display " " s)
    (write k s)
    (scheme.display " " s)
    (write v s))
  (scheme.display "}" s))

(def print-cdr (f x s)
  (if (no x)
       (scheme.display ")" s)
      (errsafe:acons x)
       (do (scheme.display " " s)
           (print f (car x) s)
         (print-cdr f (cdr x) s))
       (do (scheme.display " . " s)
           (print f x s)
         (scheme.display ")" s))))

(def print (f x s)
  (if (errsafe:acons x)
       (do (scheme.display "(" s)
           (print f (car x) s)
           (print-cdr f (cdr x) s))
      (errsafe:isa x 'table)
       (print-table f x s)
       (f x s))
  (unless (and (bound 'explicit-flush) explicit-flush) (scheme.flush-output s)))

(def disp (x (o s (stdout)))
  (print scheme.display x s))

(def write (x (o s (stdout)))
  (print scheme.write x s))
