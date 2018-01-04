(module boot racket/base

(parameterize ((current-output-port (current-error-port)))
  (display "initializing arc.. (may take a minute)")(newline))

(require (for-syntax racket)
         (only-in racket/runtime-path define-runtime-path)
         (only-in racket path-only))

(require "ac.rkt")
(require (only-in "brackets.rkt" use-bracket-readtable))
(use-bracket-readtable)

(define-runtime-path arc-arc-path "arc.arc")
(define-runtime-path libs-arc-path "libs.arc")

(parameterize ((current-directory (path-only arc-arc-path)))
  (aload arc-arc-path)
  (aload libs-arc-path))

(provide (all-from-out racket/base))
(provide (all-from-out "ac.rkt"))

)
