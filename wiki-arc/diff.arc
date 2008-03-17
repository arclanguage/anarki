
(require "lib/defpat.arc")

(def *wiki-diff (a b (o is is))
  ; data structure:
  ;  (a-seq b-seq diff N)
  (breakable:withs
        (
         build list
         simplify
         (fn (seq (o pt seq) (o mpt (cdr seq)) (o num 0))
           (while mpt
             (if (is (car pt) (car mpt))
                 (do
                   (= num 2 mpt (cdr mpt))
                   (while (is (car pt) (car mpt))
                     (++ num)
                     (zap cdr mpt))
                   (= (car pt) (list num (car pt))
                      (cdr pt) mpt)))
             (= pt mpt mpt (cdr mpt)))
           seq)
         ; dt here is the data structure above
         snake
         (fn (dt)
           (let (a-seq b-seq diff N) dt
             (if
               (is (car a-seq) (car b-seq))
                 (do
                   (while (and a-seq b-seq (is (car a-seq) (car b-seq)))
                     (= diff (cons 'skip diff)
                        N (- N 2)
                        a-seq (cdr a-seq)
                        b-seq (cdr b-seq)))
                    (if (is N 0) (break:simplify:rev diff))
                   (build a-seq b-seq diff N))
               (is N 0)
                 (break:simplify:rev diff)
               ; else
                 dt)))
         downsnake
         (fn (dt)
           (let (a-seq b-seq diff N) dt
             (if b-seq
               (snake (build
                        a-seq
                        (cdr b-seq)
                        (cons `(insert ,(car b-seq)) diff)
                        (- N 1))))))
         rightsnake
         (fn (dt)
           (let (a-seq b-seq diff N) dt
             (if a-seq
               (snake (build
                        (cdr a-seq)
                        b-seq
                        (cons 'delete diff)
                        (- N 1))))))
         N-of [_ 3]
         minN
         (fn (dt1 dt2)
           (if (no dt1) dt2
               (no dt2) dt1
                        (with (N1 (N-of dt1) N2 (N-of dt2))
                          (if (< N1 N2) dt1 dt2))))
         next-line
         (p-m:afn
           ((x y . zs))  (cons (minN (rightsnake x) (downsnake y)) (self (cons y zs)))
           ((x))         (list (rightsnake x))))
  ((afn (line)
      (self
        (cons
          (downsnake (car line))
          (next-line line))))
    ; start the triangle with a single line
    (list (snake (build a b nil (+ (len a) (len b))))))))

; use our own copy and +, since the built-ins
; don't work on scanners.
(with (copy
       (p-m:afn
         (())       ()
         ((a . as)) (cons a (self as)))
       +
       (p-m:afn
         (() b)        b
         ((a . as) b)  (cons a (self as b))))
  (p-m:def *wiki-undiff
    ( as        ()                 )
        (copy as)
    ( (_ . as)  ('delete     . es) )
        (*wiki-undiff as es)
    ( as        ((n 'delete) . es) )
        (*wiki-undiff (nthcdr n as) es)
    ( (a . as)  ('skip       . es) )
        (cons a (*wiki-undiff as es))
    ( as        ((n 'skip)   . es) )
        (+ (cut as 0 n) (*wiki-undiff (nthcdr n as) es))
    ( as        (('insert e) . es) )
        (cons e (*wiki-undiff as es))))
