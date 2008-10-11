; mzscheme -m -f as.scm
; (tl)
; (asv)
; http://localhost:8080

;(require  (lib "errortrace.ss" "errortrace"))
(require mzscheme) ; promise we won't redefine mzscheme bindings

;(profiling-enabled #t)
;(profiling-record-enabled #f)
; http://docs.plt-scheme.org/errortrace/using-errortrace.html

;find the arc directory
;NOTE!  Might not be very portable.
;In particular, it depends on the launching
;script to properly define this environment variable

(define arc-path (getenv "arc_dir"))

(define (from-arc s)
  (if arc-path
      (path->string (build-path arc-path s))
      s))

;require needs a string, and will reject expressions T.T
(define temp-cwd (current-directory))
(current-directory arc-path)

(require "ac.scm")
(require "brackets.scm")
(use-bracket-readtable)
(require "bitops.scm")
(load "ffi.scm")

(current-directory temp-cwd)

(define arc.arc.scm (from-arc "arc.arc.scm"))
(define arc.arc     (from-arc "arc.arc"))
(if (and (file-exists? arc.arc.scm) (< (file-or-directory-modify-seconds arc.arc)
                                       (file-or-directory-modify-seconds arc.arc.scm)))
    (load arc.arc.scm)
    (begin
      (display "Compiling arc.arc...\n")
      (flush-output (current-output-port))
      (acompile arc.arc)))

;; load the whole pack.arc for the moment
;; in future we should load on startup only the part relative to
;; library loading
(aload (from-arc "pack.arc"))

(define ~/.arcshrc
        (path->string (build-path (find-system-path 'home-dir) ".arcshrc")))
(when (file-exists? ~/.arcshrc)
  (aload ~/.arcshrc))

; If we have command-line arguments.
(if (> (vector-length argv) 0)
  ; If we have a command line argument that represents a file-name of
  ; a program.
  (begin
    (aload (vector-ref argv 0))
    (exit))
  ; else
  (tl))

