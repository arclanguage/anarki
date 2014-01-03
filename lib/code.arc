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
  (reduce + 1 (leaves (readall infile.file))))

(def code-density (file)
  (/ (codetree file) (codelines file)))

(def tokcount (files)
  (counts:mappend flat:readall:infile files))

(def common-tokens (files)
  (sort (compare > cadr)
        (rem nonop:car (tablist:tokcount files))))

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

(def code (x)
  (annotate 'code x))

(defextend walk (seq f) (isa seq 'code)
  (let x rep.seq
    (f x)
    (if (acons x)
      (each elem x
        (walk code.elem f)))))
