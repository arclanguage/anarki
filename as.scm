; mzscheme -m -f as.scm
; (tl)
; (asv)
; http://localhost:8080

(require mzscheme) ; promise we won't redefine mzscheme bindings

(require "ac.scm") 
(require "brackets.scm")
(use-bracket-readtable)
(require "bitops.scm")

(aload "arc.arc")
(aload "libs.arc") 

(when (file-exists? "~/.arcshrc")
  (aload "~/.arcshrc"))

; If we have command-line arguments.
(if (> (vector-length argv) 0)
  ; This was copy-and-pasted-and-modified from aload1.
  (begin
    (call-with-input-file
      (vector-ref argv 0)
      (lambda (p)
        (let ((x (read p)))
          (if (eof-object? x)
            #t
            (begin
              (arc-eval x))))))
    (exit))
  ; else
  (tl))
