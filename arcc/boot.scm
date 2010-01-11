(current-load-relative-directory (current-directory))

(require "ac.scm")
(require "brackets.scm")
(use-bracket-readtable)

(acompile "arc.arc")
(require "arcc/ar.scm")
(acompile "arcc/ac.arc")
