; Print a simple list, without parentheses 
(def prsimplelist (arg)
  (pprint (car arg))
  (let cdr-part (cdr arg)
    (if cdr-part
      (if (is 'cons (type cdr-part))
        (do (pr " ") (prsimplelist cdr-part))
        (do (pr " . ") (pprint cdr-part))))))

; Print a list, possibly quoted
(def prlist (arg)
  (let car-part (car arg)
    (if (is 'quote car-part) (do (pr "'") (pprint (cadr arg)))
        (is 'quasiquote car-part) (do (pr "`") (pprint (cadr arg)))
        (is 'unquote car-part) (do (pr ",") (pprint (cadr arg)))
        (is 'unquote-splicing car-part) (do (pr ",@") (pprint (cadr arg)))
	(do (pr "(") (prsimplelist arg) (pr ")") ))))

; Print an arbitrary thing
(def pprint (arg) 
  (let ty (type arg)
    (if (is 'cons ty) (prlist arg)
        (write arg)))
  nil)
