
(in-package lazy-scanner)
(using <arc>v3)
(using <arc>v3-scanner)
(interface v1
  lazy-scanner lazy-destruct
  generate)

(def delay (v)
  (fn () v))

(defm car ((t x lazy-scanner-type))
  ((car (rep x))))
(defm cdr ((t x lazy-scanner-type))
  ((cdr (rep x))))

(defm scanner ((t x lazy-scanner-type))
  x)
(defm unscan ((t orig lazy-scanner-type) x)
  x)

(mac lazy-scanner (a d)
  " Creates a scanner which evaluates the
    given expressions only when 'car and
    'cdr are used on it.
    See also [[lazy-destruct]] [[generate]] "
  `(symeval!lazy-scanner-f (fn () ,a) (fn () ,d)))

(def lazy-scanner-f (af df)
  ; warning!  Not *completely* lazy: it is
  ; possible, in a multithread environment,
  ; for two different threads to execute
  ; a lazy expression simultaneously
  ; we can use semaphore locks of some
  ; sort, but it *might* get bashed by
  ; <base>collect-on (depending on whether
  ; we use dynamic-wind, potentially, but
  ; I'll have to figure out the continuation
  ; guards first)
  (let rv (cons nil nil)
    (= (car rv)
       (fn ()
         (let it (af)
           (= (car rv) (delay it))
           it)))
    (= (cdr rv)
       (fn ()
         (let it (df)
           (= (cdr rv) (delay it))
           it)))
    (annotate 'lazy-scanner-type
      rv)))

(mac lazy-destruct (c)
  " Creates a scanner from a single expression
    that returns a cons cell or other scanner.
    The expression is evaluated only when
    'car or 'cdr is applied to this scanner.
    See also [[lazy-scanner]] [[generate]] "
  `(symeval!lazy-destruct-f (fn () ,c)))

(def lazy-destruct-f (cf)
  (withs (rv (cons nil nil)
          destruct
          (fn ()
            (withs (v (cf)
                    a (car v)
                    d (cdr v))
              (= (car rv) (delay a))
              (= (cdr rv) (delay d)))))
    (= (car rv)
       (fn ()
         (destruct)
         ((car rv))))
    (= (cdr rv)
       (fn ()
         (destruct)
         ((cdr rv))))
    (annotate 'lazy-scanner-type
      rv)))

; ensure constructing a new lazy-scanner from
; another is truly lazy
; this gets to affect pretty much every sequence-building
; thing in arc.arc
(defm <base>collect-on ((t seq lazy-scanner-type) bf)
  (point return
    (bf
      (fn (i)
        (point body-return
          (return
            (lazy-scanner
             i
             (point new-return
               (= return new-return)
               (body-return i)))))))
    ; return might have been reassigned
    (return nil)))

(def generate (f i)
  " Creates an infinite lazy sequence from the
    continued application of `f' on `i'.
    See also [[lazy-scanner]] [[lazy-destruct]] "
  (lazy-scanner
    i
    (generate f (f i))))

