
(mac def-all-ss rest
  (let pairs (map (fn ((s e)) `(,(string s) ,e))
                  (pair rest))
    `(= ssyntaxes*
        ',pairs
        ssyntax-strings*
        ',(map car pairs))))

(def a-ssyntax (s)
  (and (isa s 'sym)
       (let ss (string s)
         (some [posmatch _ ss] ssyntax-strings*))))

(let (has split-string expander postfix prefix expand) nil
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
                        (all [is at._ (what (+ i _))] (range 1 (- len-at 1))))
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
       (if (is ssyn (cut s (- (len s))))
           (let sub (cut s 0 (- (len s)))
             (if (empty sub)
               (next s)
               ((expander e) (next sub) "")))
           (next s))))
  ; performs expansion for prefix
  (= prefix
     (fn (s ssyn e next)
       (if (is ssyn (cut s 0 (len ssyn)))
           (let sub (cut s (len ssyn))
             (if (empty sub)
               (next s)
               ((expander e) "" (next sub))))
           (next s))))
  (= expand
     (fn (s ssyntaxes)
       (if (no ssyntaxes)
           (read s)
           (withs (((ssyn e) . rest) ssyntaxes
                   next [expand _ rest])
             ; determine the type of expansion first
             (if
               (has 'l e)
                 (if
                   ; right-associative
                   (or (has 'R e) (has 'r e))
                     (let split (map next (split-string ssyn s))
                       (if (> (len split) 1)
                           (rreduce (expander e next) split)
                           (next s)))
                     ; postfix
                     (postfix s ssyn e next))
               (has 'L e)
                 (if
                   ; left associative
                   (or (has 'R e) (has 'r e))
                     (let split (map next (split-string ssyn s))
                       (if (> (len split) 1)
                           (reduce (expander e) split)
                           (next s)))
                     ; postfix
                     (postfix s ssyn e next))
               (or (has 'r e) (has 'R e))
                 ; prefix
                 (prefix s ssyn e next)
               (has '... e)
                 ; variadic
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
               ; no fixity: expand only if
               ; ssyntax is exact
               (is ssyn s)
                 e
                 (next s))))))
  (def a-ssexpand (s)
    (if (isa s 'sym)
        (expand (string s) ssyntaxes*)
        s)))

(def-all-ss
  ; earlier listed stuff has lower precedence
  #\:  (compose ...)
  ; prefix
  ~    (complement R)
  ; standalone
  ~    no
  #\.  (...)
  !    (L 'r))


