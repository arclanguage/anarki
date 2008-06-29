
(mac def-all-ss rest
  (let pairs (pair rest (fn (s e) `(,(string s) ,e)))
    `(do (a-ssyntax-clear)
         (a-ssyntax-top ',pairs))))

(mac add-ssyntax-top body
  `(a-ssyntax-top ',(pair body [list (string _1) _2])))

(mac add-ssyntax-bottom body
  `(a-ssyntax-bottom ',(pair body [list (string _1) _2])))

(let (has split-string expander postfix prefix expand
      expansion-type
      ssyntaxes pre-ss post-ss in-ss exact-ss) nil
  ; determines if the expression e ever
  ; has the symbol v
  (= has
     (fn (v e)
       (if
         (acons e)
           (or (has v (car e))
               (has v (cdr e)))
         (is e v)
           t
           nil)))
  (= split-string
     (fn (at what)
       (let (hd tl) nil
         (with (collect (fn (o)
                          (if hd (= tl (= (cdr tl) (cons o nil)))
                                 (= hd (= tl       (cons o nil)))))
                j 0
                len-at (len at))
           ; start scanning from 1, not 0, and stop
           ; just before the line end: we don't want
           ; to trigger from a prefix or postfix, since
           ; this splitterfunction is supposed to support
           ; infix notations only
           (for i 1 (- (len what) len-at 1)
             (when (and (is at!0 what.i)
                        (all [is at._ (what (+ i _))] (if (isnt len-at 0) (range 1 (- len-at 1)))))
                 (collect (cut what j i))
                 (= j (+ i len-at))))
           (collect (cut what j)))
         hd)))
  ; creates a function that performs generic expansion
  (= expander
     (fn (e)
       (fn (l r)
         ((afn (e)
            (if (in e 'l 'L) l
                (in e 'r 'R) r
                (acons e)    (cons (self (car e))
                                   (self (cdr e)))
                             e))
          e))))
  ; performs expansion for postfix
  (= postfix
     (fn (s ssyn e next)
       (if (is ssyn (cut s (- (len ssyn))))
           (let sub (cut s 0 (- (len ssyn)))
             (if (empty sub)
               (next s)
               ((expander e) (next sub) "")))
           (next s))))
  ; performs expansion for prefix
  (= prefix
     (fn (s ssyn e next)
       (if (is ssyn (cut s 0 (min (len ssyn) (len s))))
           (let sub (cut s (len ssyn))
             (if (empty sub)
               (next s)
               ((expander e) "" (next sub))))
           (next s))))
  ; determine type of expansion
  (= expansion-type
     (fn (e)
       (if
         (has 'l e)
           (if (or (has 'R e) (has 'r e))
               'rightassoc
               'postfix)
         (has 'L e)
           (if (or (has 'R e) (has 'r e))
               'leftassoc
               'postfix)
         (or (has 'R e) (has 'r e))
           'prefix
         (has '... e)
           'variadic
           'none)))
  (= expand
     (fn (s ssyntaxes)
       (if (no ssyntaxes)
           (read s)
           (withs (((ssyn e) . rest) ssyntaxes
                   next [expand _ rest])
             ; determine the type of expansion first
             (case (expansion-type e)
               rightassoc
                 (let split (map next (split-string ssyn s))
                   (if (> (len split) 1)
                       (rreduce (expander e next) split)
                       (next s)))
               leftassoc
                 (let split (map next (split-string ssyn s))
                   (if (> (len split) 1)
                       (reduce (expander e) split)
                       (next s)))
               postfix
                 (postfix s ssyn e next)
               prefix
                 (prefix s ssyn e next)
               variadic
                 (let split (map next (split-string ssyn s))
                   (if (> (len split) 1)
                       ((afn (e)
                          (if (acons e)
                              (if (is (car e) '...)
                                  (join split (self (cdr e)))
                                  (cons (self (car e))
                                        (self (cdr e))))
                              e))
                        e)
                       (next s)))
               none
                 (if (is ssyn s)
                     e
                     (next s)))))))
  (=
     ; key: initial character of ssyntax
     ; value: set of ssyntaxes starting with
     ;   that character
     pre-ss (table)
     ; key: number of characters in ssyntax
     ; value: table
     ;   key: first character of ssyntax
     ;   value: set of ssyntaxes of the correct
     ;     length starting with that character
     post-ss (table)
     ; key: initial character of ssyntax
     ; value: set of ssyntaxes starting with
     ;   that character
     in-ss (table)
     ; key: string of ssyntax
     ; value: t
     exact-ss (table))
  (def a-ssyntax-clear ()
    (= ssyntaxes nil)
    (ontable k v pre-ss   (= pre-ss.k nil))
    (ontable k v post-ss  (= post-ss.k nil))
    (ontable k v in-ss    (= in-ss.k nil))
    (ontable k v exact-ss (= exact-ss.k nil)))
  (def a-ssyntax-add (pairs on-top)
    (zap (if on-top [join pairs _] [join _ pairs]) ssyntaxes)
    (let to-in
         (fn (s)
           (push s (in-ss s.0)))
      (each (s e) pairs
        (case (expansion-type e)
          variadic   (to-in s)
          leftassoc  (to-in s)
          rightassoc (to-in s)
          none       (= exact-ss.s t)
          postfix
            (withs (l  (len s)
                    tb (or post-ss.l (= post-ss.l (table))))
              (push s (tb s.0)))
          prefix
            (push s (pre-ss s.0)))))
    t)
  (def a-ssyntax-top    (pairs) (a-ssyntax-add pairs t))
  (def a-ssyntax-bottom (pairs) (a-ssyntax-add pairs nil))
  (def a-ssyntax (s)
    (and (isa s 'sym)
         (withs (s (string s)
                 ls (len s))
           (or (exact-ss s)
               (awhen (pre-ss s.0)
                 (aand (mem [headmatch _ s] it)
                       (> ls (len:car it))))
               ; scan through
               (when (> ls 1)
                 ; start at 1
                 ((afn (i l)
                    (when (> l 0)
                      (if
                        (aif
                          (in-ss s.i)
                            (some [and (> l (len _))
                                       (headmatch _ s i)] it)
                          (aand (post-ss l)
                                (it s.i))
                            (some [headmatch _ s i] it))
                          t
                          (self (+ i 1) (- l 1)))))
                  1 (- ls 1)))))))
  (def a-ssexpand (s)
    (if (isa s 'sym)
        (expand (string s) ssyntaxes)
        s)))

(def-all-ss
  ; earlier listed stuff has lower precedence
  ".." (range ...)
  //   (orf ...)
  &&   (andf ...)
  #\:  (compose ...)
  ; prefix
  ~    (complement R)
  ; standalone
  ~    no
  #\.  (...)
  !    (L 'r)
  ?    [isa _ 'L])

(= ssyntax a-ssyntax
   ssexpand a-ssexpand)

