(def re-split (delim str)
  (($ regexp-split) delim str))

(def re-replace (pat text replacement)
  (($ regexp-replace*) (($ pregexp) pat) text replacement))

(def matchsplit (pat str)
  (let pos (posmatch pat str)
    (if pos
	(list (cut str 0 pos) (cut str (+ 1 pos)))
	(list str))))
