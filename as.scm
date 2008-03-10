; mzscheme -m -f as.scm
; (tl)
; (asv)
; http://localhost:8080

(require mzscheme) ; promise we won't redefine mzscheme bindings

(require "ac.scm") 
(require "brackets.scm")
(use-bracket-readtable)
(require "bitops.scm")
(load "ffi.scm")

(aload "arc.arc")
(aload "libs.arc")

(when (file-exists? "~/.arcshrc")
  (aload "~/.arcshrc"))

; If we have command-line arguments.
(if (> (vector-length argv) 0)
  ; If we have a command line argument that represents a file-name of
  ; a program.
  (begin
    (call-with-input-file
      (vector-ref argv 0)
      aload1)
    (exit))
  ; else
  (tl))
