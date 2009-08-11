; mzscheme -m -f as.scm
; (tl)
; (asv)
; http://localhost:8080

(require mzscheme) ; promise we won't redefine mzscheme bindings

(require "ac.scm") 
(require "brackets.scm")
(use-bracket-readtable)

(parameterize ((current-directory (current-load-relative-directory)))
  (aload "arc.arc")
  (aload "libs.arc"))

(tl)

