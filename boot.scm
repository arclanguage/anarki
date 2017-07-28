(module boot mzscheme ; promise we won't redefine mzscheme bindings

(require (only racket path-only))
(require (only racket/runtime-path define-runtime-path))

(require "ac.scm")
(require (only "brackets.scm" use-bracket-readtable))
(use-bracket-readtable)

(define-runtime-path arc-arc-path "arc.arc")
(define-runtime-path libs-arc-path "libs.arc")

(parameterize ((current-directory (path-only arc-arc-path)))
  (aload arc-arc-path)
  (aload libs-arc-path))

(provide (all-from mzscheme))
(provide (all-from "ac.scm"))

)
