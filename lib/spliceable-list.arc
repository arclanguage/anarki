;; append elems to it, check n-elem suffix for a match, then splice the suffix out

(def spliceable-list (n (o init))
  ++.n
  (annotate 'spliceable-list (obj contents init
                                  last lastcons.init
                                  suffix-len n
                                  suffix (suffix n init))))

(defmethod len(l) spliceable-list
  (len rep.l!contents))

(defmethod empty(l) spliceable-list
  (empty rep.l!contents))

(defgeneric append(a b)
  (= (cdr lastcons.a) b))

(defmethod append(l tail) spliceable-list
  (if empty.l
    (= rep.l!contents tail
       rep.l!last rep.tail)
    (= (cdr rep.l!last) tail))
  (zap lastcons rep.l!last)
  (if rep.l!suffix
    (zap [nthcdr len.tail _] rep.l!suffix)
    ; no suffix yet; do we have enough elems to start?
    (if (is rep.l!suffix-len (len rep.l!contents))
      (= rep.l!suffix rep.l!contents))))

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
  (when rep.l!suffix
    (wipe (cdr rep.l!suffix))
    rep.l!contents))

; return last n elems of l -- as long as there are at least that many
(defgeneric suffix (n l)
  (let max len.l
    (if (>= max n)
      (nthcdr (- max n) l))))

(defmethod suffix (l) spliceable-list
  (aif
    rep.l!suffix
      cdr.it
    (iso (len rep.l!contents) (- rep.l!suffix-len 1))
      rep.l!contents))
