; mzscheme -m -f as.scm
; (tl)
; (asv)
; http://localhost:8080

(require mzscheme) ; promise we won't redefine mzscheme bindings

(define arc-path (getenv "arc_dir"))
(define temp-cwd (current-directory))
(if arc-path (current-directory arc-path))

(require "ac.scm") 
(require "brackets.scm")
(use-bracket-readtable)
(require "bitops.scm")
(load "ffi.scm")


; in future, need to resolve path properly to
; installation path
(if (and (file-exists? "arc.arc.scm") (< (file-or-directory-modify-seconds "arc.arc")
                                         (file-or-directory-modify-seconds "arc.arc.scm")))
    (load "arc.arc.scm")
    (begin
      (display "Compiling arc.arc...\n")
      (flush-output (current-output-port))
      (acompile "arc.arc")))
(aload "libs.arc")

(if arc-path (current-directory temp-cwd))

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
