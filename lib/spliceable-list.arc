;; append elems to it, check n-elem suffix for a match, then splice the suffix out

(def spliceable-list (n (o init))
  ++.n
  (annotate 'spliceable-list (obj contents init
                                  last lastcdr.init
                                  suffix-len n
                                  suffix (suffix n init))))

(defmethod len(l) spliceable-list
  (len rep.l!contents))

(defmethod empty(l) spliceable-list
  (empty rep.l!contents))

(defgeneric append(a b)
  (= (cdr lastcdr.a) b))

(defmethod append(l tail) spliceable-list
  (if empty.l
    (= rep.l!contents tail
       rep.l!last rep.tail)
    (= (cdr rep.l!last) tail))
  (zap lastcdr rep.l!last)
  (if rep.l!suffix
    (zap [nthcdr len.tail _] rep.l!suffix)
    (= rep.l!suffix
       (suffix rep.l!suffix-len
               rep.l!contents))))

(defcoerce cons spliceable-list (l)
  rep.l!contents)

(defcoerce spliceable-list cons (l)
  (spliceable-list l))

(defcall spliceable-list (l i)
  rep.l!contents.i)

(extend sref (l v i) (isa l 'spliceable-list)
  (sref rep.l!contents v i))

; returns all but the suffix; corrupts the suffix list
(def splice(l)
  (if
    rep.l!suffix
      (do
        (wipe (cdr rep.l!suffix))
        rep.l!contents)
    (is rep.l!suffix-len (len rep.l!contents))
      (do
        (wipe (cdr rep.l!contents))
        rep.l!contents)))

(defgeneric suffix (n l)
  (let max len.l
    (if (> max n)
      (nthcdr (- max n) l))))

(defmethod suffix (l) spliceable-list
  (aif rep.l!suffix
    cdr.it
    (if (iso rep.l!suffix-len (len rep.l!contents))
      (cdr rep.l!contents)
      rep.l!contents)))
