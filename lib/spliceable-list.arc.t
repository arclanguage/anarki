(test-iso "spliceable-list initializes without a list"
  (obj contents nil last nil suffix-len 3 suffix nil)
  (rep:spliceable-list 2))

(test-iso "spliceable-list initializes with a list"
  (obj contents list.1 last list.1 suffix-len 3 suffix nil)
  (rep:spliceable-list 2 '(1)))

(= l (spliceable-list 2 '(1)))
(test-iso "suffix returns nothing if list is too short"
  nil
  suffix.l)

(append l list.2)
(test-iso "suffix returns list if just long enough"
  '(1 2)
  suffix.l)
(test-iso "appending to spliceable-list works"
  (obj contents '(1 2)  last list.2   suffix nil  suffix-len 3)
  rep.l)

(append l list.3)
(test-iso "suffix 3" '(2 3) suffix.l)
(test-iso "splicing a list without a suffix works"
  '(1)
  splice.l)

(= l (spliceable-list 2 '(1 2 3)))
(append l list.4)
(test-iso "suffix 4" '(3 4) suffix.l)
(test-iso "appending to spliceable-list updates suffix"
  (obj contents '(1 2 3 4)  last list.4   suffix '(2 3 4)   suffix-len 3)
  rep.l)

(test-iso "splicing a list with suffix works"
  '(1 2)
  splice.l)
