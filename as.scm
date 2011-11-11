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

(let ((args (vector->list (current-command-line-arguments))))
  (if (null? args)
    (tl)
    ; command-line arguments are script filenames to execute
    (for-each (lambda (f) (aload f)) args)))

