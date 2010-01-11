(current-load-relative-directory (current-directory))

(require "brackets.scm")
(use-bracket-readtable)

(require "arcc/ar.scm")

(load "arc.arc.scm")
(load "arcc/ac.arc.scm")

(_load "libs.arc")
(_tl)
