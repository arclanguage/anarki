; This file is the source for the standalone interpreter of Arc.
; To make : mzc --exe arc-exe arc-exe.scm
; It is basically as.scm wrapped in a module, with an additional
; require-namespace to make definitions from ac.scm available to arc.

(module arc-exe "arc-exe-init.scm"

(require "ac.scm")
(namespace-require "ac.scm")
(require "brackets.scm")
(use-bracket-readtable)
(require "bitops.scm")

(define (cload1 in out)
  (let ((x (read in)))
    (if (eof-object? x)
        #t
        (begin
          (arc-eval x)
          (print (compile (ac x '())) out)
          (newline out)
          (cload1 in out)))))

(define (cload filename)
  (let ((in (open-input-file filename))
        (out (open-output-file (string-append filename "c"))))
    (cload1 in out)
    (close-input-port in)
    (close-output-port out)))

(if (not (file-exists? "arc.arcc"))
  (cload "arc.arc")
  (load "arc.arcc"))

(if (not (file-exists? "libs.arcc"))
  (cload "libs.arc")
  (load "libs.arcc"))

(when (file-exists? "~/.arcshrc")
  (aload "~/.arcshrc"))

(let ((args (current-command-line-arguments)))
  (cond ((= 0 (vector-length args)) (tl)) ; No argument : REPL
        ((equal? (vector-ref args 0) "-cc") (cload (vector-ref args 1))) ; compile & run code
        ((equal? (vector-ref args 0) "-lc") (load (vector-ref args 1)))  ; load compiled code
        (#t (aload (vector-ref args 0))))) ; no switch : load regular arc code

)