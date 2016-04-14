;; append elems to it, check n-elem suffix for a match, then splice the suffix out

(def spliceable-list (n (o init))
"Create a spliceable list with a constant suffix length of n. A spliceable
list is a special data structure that efficiently supports the following
operations:

1. Appending items using [[append]].
2. Returning the last n items appended using [[suffix]] if there are at least
n items to return.
3. Dropping the last n items appended, and returning everything else. [[splice]]"
  ++.n
  (annotate 'spliceable-list (obj contents init
                                  last lastcons.init
                                  suffix-len n
                                  suffix (suffix n init))))

(defextend len (l) (isa l 'spliceable-list)
  (len rep.l!contents))

(defextend empty (l) (isa l 'spliceable-list)
  (empty rep.l!contents))

(def append (a b)
  (= (cdr lastcons.a) b))

(defextend append (l tail) (isa l 'spliceable-list)
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

(defextend sref (l v i) (isa l 'spliceable-list)
  (sref rep.l!contents v i))

(def splice (l)
"Clears up to the last n items of a [[spliceable-list]] defined with a suffix
length of n, and returns everything else."
  (when rep.l!suffix
    (wipe (cdr rep.l!suffix))
    rep.l!contents))

(examples splice
  (splice (spliceable-list 3))
  nil
  (splice (spliceable-list 3 '(1)))
  nil
  (splice (spliceable-list 3 '(1 2)))
  nil
  (splice (spliceable-list 3 '(1 2 3)))
  nil
  (splice (spliceable-list 3 '(1 2 3 4)))
  (1))

(def suffix (n l)
"Computes the last n elements of l -- as long as there are at least that many."
  (let max len.l
    (if (>= max n)
      (nthcdr (- max n) l))))

(examples suffix
  (suffix 3 '(1))
  nil
  (suffix 3 '(1 2))
  nil
  (suffix 3 '(1 2 3))
  (1 2 3)
  (suffix 3 '(1 2 3 4))
  (2 3 4))

(defextend suffix (l) (isa l 'spliceable-list)
  (aif
    rep.l!suffix
      cdr.it
    (iso (len rep.l!contents) (- rep.l!suffix-len 1))
      rep.l!contents))
