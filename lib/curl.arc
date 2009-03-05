(def curl (args)
  "Fetches a webpage using arguments, returning result"
  (= parms (map (fn (l) (string "--" (car l) (if (cadr l) (string " \"" (cadr l) "\" ")))) args))
  (system (apply string (cons "curl " parms))))
  
