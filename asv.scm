;; Like as.scm, but interprets just the first arg as a file to execute.
;; Good for arc scripts that need to take their own commandline args.

; prelude is like as.scm
(require mzscheme)
(require "ac.scm")
(require "brackets.scm")
(use-bracket-readtable)
(parameterize ((current-directory (current-load-relative-directory)))
  (aload "arc.arc")
  (aload "libs.arc"))

(aload (vector-ref (current-command-line-arguments) 0))
