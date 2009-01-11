; iterator utils
; by ben stoker <benrkts@yahoo.com>

(mac iter xs
  (with (us (map [uniq] xs)
         f  (uniq))
    `(withs ,(mappend list us xs)
       (fn (,f) ,@(map [list f _] us) nil))))

(mac ieach (var i . body) `(do (,i (fn (,var) ,@body)) nil))

; transformations

(def imap (f xs) [ieach x xs (_ f.x)])

(def ikeep (f xs) [ieach x xs (if f.x _.x)])

(def iconc (xss) [ieach xs xss xs._])

(def imappend (f xs) [ieach x xs (f.x _)])

(def itake (n xs)
  [ccc (fn (ret)
         (let i 0
           (ieach x xs (if (>= i n) ret.nil) _.x (++ i))))])

(def itakewhile (f xs) [ccc (fn (ret) (ieach x xs (if f.x _.x ret.nil)))])

; builders

(def irange (a b) [for i a b _.i])

(def ibelow (n) (irange 0 (- n 1)))

(def iupfrom (n) [let i n (while t _.i (++ i))])

(def irepeat (x) [while t _.x])

(def icumulate (f q xs) [let q q (ieach x xs (= q f.q.x) _.q)])

; lookups/tests

(def ifirst (xs) ccc.xs)

(def inth (n xs) (ccc [let i 0 (ieach x xs (if is.i.n _.x (++ i)))]))

(def ifind (f xs) (ccc [ieach x xs (if f.x _.x)]))

(def isome (f xs) (ccc [ieach x xs (if f.x _.t)]))

(def iall (f xs) (ccc [do (ieach x xs (if (~f x) _.nil)) t]))

; reductions

(def ifoldl (f q xs) (ieach x xs (= q f.q.x)) q)

(def ibest (gt xs) (ifoldl (fn (y x) (if (or no.y gt.x.y) x y)) nil xs))

; properties

(def ilen (xs) (let i 0 (ieach x xs (++ i)) i))

(def iempty (xs) (ccc (fn (ret) (ieach x xs ret.nil) t)))

(def iornil (xs) (if (~iempty xs) xs))

; combining iters

(def iproduct (xs ys) [ieach x xs (ieach y ys (_ list.x.y))])

(= iendgen* (uniq))

; slow :-(
(def igenerate (xs)
  (with (n nil r nil)
    (fn ()
      (ccc [(= r _)
            (if n
              n.nil
              (do (ieach x xs (ccc [do (= n _) r.x]))
                  (ccc [= n _])
                  r.iendgen*))]))))

(def izip (xs ys)
  [withs (gx igenerate.xs x (gx))
    (ccc (fn (ret)
           (ieach y ys
             (if is.x.iendgen* ret.nil)
             (_ list.x.y)
             (= x (gx)))))])

(def iare (xs ys)
  (withs (gx igenerate.xs x (gx))
    (ccc (fn (ret)
           (ieach y ys
             (if isnt.x.y ret.nil)
             (= x (gx)))
           is.x.iendgen*))))

; common zips; should be faster than using izip

(def imkzip (init stop f next xs)
  [ccc (fn (ret)
         (let z init
           (ieach x xs
             (if stop.z ret.nil)
             (_ (list f.z x))
             (= z next.z))))])

(def izip-range (a b xs) (imkzip a [> _ b] idfn [+ _ 1] xs))

(def izip-below (n xs) (izip-range 0 (- n 1) xs))

(def izip-i (xs) (imkzip 0 [do nil] idfn [+ _ 1] xs))

(def izip-list (lst xs) (imkzip lst no car cdr xs))

(def izip-string (s xs) (imkzip 0 [is _ len.s] [s _] [+ _ 1] xs))

; conversions

(def listiter (xs) [let xs xs (while xs (_ car.xs) (= xs cdr.xs))])

(def stringiter (s) [for i 0 (- len.s 1) (_ s.i)])

(def tabiter (h) [do (ontable k v h (_ list.k.v)) nil])

(def toiter (x)
  (if alist.x      listiter.x
      isa.x!string stringiter.x
      isa.x!table  tabiter.x
                   iter.x))

(def iterlist (xs)
  (with (hd nil tl nil)
    (ieach x xs
      (if hd
        (= tl (= cdr.tl cons.x.nil))
        (= tl (= hd cons.x.nil))))
    hd))

(def itertab (xs)
  (let h (table)
    (ieach (k v) xs (= h.k v))
    h))

; misc

(= ifreeze listiter:iterlist)
