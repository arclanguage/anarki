; Code analysis. Spun off 21 Dec 07.

; Ought to do more of this in Arc.  One of the biggest advantages
; of Lisp is messing with code.

(def codelines (file)
  (w/infile in file
    (summing test
      (whilet line (readline in)
        (test (aand (find nonwhite line) (isnt it #\;)))))))

(def codeflat (file)
  (len (flat (readall (infile file)))))

(def codetree (file)
  (treewise + (fn (x) 1) (readall (infile file))))

(def code-density (file)
  (/ (codetree file) (codelines file))) 

(def tokcount (files)
  (let counts (table)
    (each f files
      (each token (flat (readall (infile f)))
        (++ (counts token 0))))
    counts))

(def common-tokens (files)
  (let counts (tokcount files)
    (let ranking nil
      (maptable (fn (k v)
                  (unless (nonop k)
                    (insort (compare > cadr) (list k v) ranking)))
                counts)
      ranking)))

(def nonop (x)
  (in x 'quote 'unquote 'quasiquote 'unquote-splicing))

(def common-operators (files)
  (keep [and (isa (car _) 'sym) (bound (car _))] (common-tokens files)))

(def top40 (xs)
  (map prn (firstn 40 xs))
  t)

(def space-eaters (files)
  (let counts (tokcount files)
    (let ranking nil
      (maptable (fn (k v)
                  (when (and (isa k 'sym) (bound k))
                    (insort (compare > [* (len (string (car _)))
                                          (cadr _)])
                            (list k v (* (len (string k)) v))
                            ranking)))
                counts)
    ranking)))

;(top40 (space-eaters allfiles*))

(mac flatlen args `(len (flat ',args)))
