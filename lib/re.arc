(def re-split (delim str)
  (($ regexp-split) delim str))

(def re-replace (pat text replacement)
  (($ regexp-replace*) (($ pregexp) pat) text replacement))
