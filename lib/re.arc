; by Mark Huetsch
; some code lifted (with slight modifications) from http://awwx.ws/re2.arc

(def re-match-pat (pat str)
  (ac-niltree ($.regexp-match ($.pregexp pat) str)))

(def re-match (pat str)
  (only.cdr (re-match-pat pat str)))

(def re-split (delim str)
  ($.regexp-split delim str))

(def re-replace (pat text replacement)
  ($.regexp-replace* ($.pregexp pat) text replacement))

; TODO why did I want this in addition to re-split again?
(def matchsplit (pat str)
  (let pos (posmatch pat str)
    (if pos
	    (list (cut str 0 pos) (cut str (+ 1 pos)))
	    (list str))))
