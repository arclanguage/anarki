($:require
  (only-in racket
    regexp-match*
    regexp-split
))

(= re $.regexp)
(document builtin re (s)
  "Compiles 's' to a regular expression. See also [[pre]].")

(= pre $.pregexp)
(document builtin pre (s)
  "Compiles 's' to a regular expression using a Perl-like syntax. See also [[re]].")

(= re-match ac-niltree:$.regexp-match)
(document builtin re-match (pat i)
  "Returns the first match of 'pat' in 'i'.")

(= re-match* ac-niltree:$.regexp-match*)
(document builtin re-match* (pat i)
  "Returns each match of 'pat' in 'i'.")

(def re-pos (pat i) (errsafe:car (ac-niltree:$.regexp-match-positions pat i)))
(document builtin re-pos (pat i)
  "Returns the start and end position of the first match of 'pat' in 'i'.")

(= re-subst ac-niltree:$.regexp-replace)
(document builtin re-subst (pat i sub)
  "Returns 'i' where first match of 'pat' has been replaced with 'sub'.")
