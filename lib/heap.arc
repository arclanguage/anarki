
; purely functional leftist heap

(mac rank (h)
     (w/uniq q
     `(let ,q ,h
        (if ,q
            (car ,q)
            0))))

(def mkt (x a b)
       (if (>= (rank a) (rank b))
           (list (+ (rank b) 1) x a b)
           (list (+ (rank a) 1) x b a)))


(def hmerge (h1 h2 (o <= <=))
  (if (no h2) h1
      (no h1) h2
      (with ((r1 x a1 b1) h1
             (r2 y a2 b2) h2)
        (if (<= x y)
            (mkt x a1 (hmerge b1 h2 <=))
            (mkt y a2 (hmerge h1 b2 <=))))))

(mac insert (x h (o <= '<=))
     `(hmerge (list 1 ,x (mptyh) (mptyh)) ,h ,<=))

(mac mptyh ())

; return min elem
(def hmin (h)
  (when (isa h 'cons) (cadr h)))

; del min elem
(def delmin (h (o <= <=))
  (when (isa h 'cons)
    (let (r x a b) h
      (hmerge a b <=))))

; (insert 5 (mptyh))
; (= h (insert 5 (insert 3 (insert 7 (insert 4 (mptyh))))))
; (do (prn:hmin h) 
;     (prn:hmin:delmin:delmin h)
;     (prn:hmin:delmin:delmin:delmin h)
;     (prn:hmin:delmin:delmin:delmin:delmin h))
