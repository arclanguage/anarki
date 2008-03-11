; scanner.arc
; by AmkG

#|
A scanner is a value that happens to be a
perfect subset of a cons.  It supports only
two basic operations, car and cdr.  The result
of a 'car can be anything, but the result of
a 'cdr must always be either another scanner
or nil.

Generally it is can be used to treat certain
objects as lists without making them into
actual lists, for example, strings (as list
of characters) or input files (as list of
characters, again).

In C++ STL terms, a scanner is equivalent to
a forward const_iterator, with unary* being
car and prefix ++ being cdr.
|#

(require "lib/defm.arc")

(def make-scanner args
  " Creates a scanner, whose car and cdr operations
    are functions given using the parameters 'car
    and 'cdr.
      (make-scanner 'car (fn () ...)
                    'cdr (fn () ...))
    Note that you are responsible for properly
    memoizing the results returned by 'car and 'cdr
    See also [[scanner]] "
  (let (a d) nil
    ( (afn ((opt val . args))
        (if
          (is opt 'car)
            (= a val)
          (is opt 'cdr)
            (= d val)
            (err:tostring:pr "Unknown option - " opt))
        (if args (self args)))
     args)
    (annotate 'scanner
      (cons a d))))

(redef isa (x y)
  (if (is (type x) 'scanner)
    (in y 'scanner 'cons)
    (old x y)))
(defm acons ((t s scanner)) t)
(defm alist ((t s scanner)) t)

(defm car ((t s scanner))
  ((old (rep s))))

(defm cdr ((t s scanner))
  ((old (rep s))))

(defm scar ((t s scanner) v)
  (err "scar: attempt to write to scanner"))

(defm scdr ((t s scanner) v)
  (err "scdr: attempt to write to scanner"))

; don't invoke unless you
; really, really, really have to.
; (scanner 'cdr operations are not
; guaranteed to be quick!)
(defm len ((t s scanner))
  (let n 0
    (while s
      (++ n)
      (zap cdr s))
    n))

; can't safely redefine using 'defm, since
; sigs aren't exactly the same.
(with (unspecified (uniq)
       helper
       (afn (seq end)
         (if (and seq (> end 0))
             (make-scanner
               'car (fn () (car seq))
               'cdr (fn () (self (cdr seq) (- end 1)))))))
  ; why not (o end (len seq))?  Because
  ; 'len on a scanner might be a long operation,
  ; e.g. on an input port the scanner would read
  ; a new character at each 'cdr.
  (redef cut (seq start (o end unspecified))
    (if (is end unspecified)
        (if (isa seq 'scanner)
            (do
              ; do this here, because the old version
              ; of cut might have a different interpretation
              ; for negative args
              (if (< start 0) (zap + start (len seq)))
              (nthcdr start seq))
            (old seq start)))
        (do
          (if (isa seq 'scanner)
              (do
                (if (< start 0) (zap + start (len seq)))
                (if (< end 0) (zap + end (len seq)))
                (if (< start end)
                    (helper (nthcdr start seq) (- end start))))
              (old seq start end)))))

(def scanner-string (s (o start 0) (o end (len s)))
  " Creates a scanner for a string `s'.  You may also
    specify optional `start' and `end' locations using
    negative arguments similar to 'cut.
    See also [[scanner-input]] [[cut]] [[make-scanner]] "
  (if (< start 0) (zap + start (len s)))
  (if (< end 0)   (zap + end (len s)))
  (if (and (< start end) (<= end (len s)))
    (let (a d d-valid) nil
      (make-scanner
        'car (fn () (or a (= a (s start))))
        'cdr (fn ()
               (if d-valid
                   d
                   (= d-valid t
                      d (scanner-string s (+ 1 start) end))))))))

(def scanner-input (s (o autoclose nil))
  " Creates a scanner for an input stream `s'.
    If the optional `autoclose' parameter is specified
    and true, the stream is closed when the scanner
    reaches end of file.
    See also [[scanner-string]] [[make-scanner]] "
  (let a (readc s)
    (if a
        (let (d d-valid) nil
          (make-scanner
            'car (fn () a)
            'cdr (fn ()
                   (if d-valid
                       d
                       (= d-valid t
                          d (scanner-input s autoclose))))))
        (if autoclose
            (do (close s) nil)))))

