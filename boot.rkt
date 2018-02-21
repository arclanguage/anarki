#lang racket/base

(require
  (only-in racket/cmdline command-line)
  (only-in "ac.rkt"
    aload-with-main-settings
    anarki-init-in-main-namespace-verbose
    tl-with-main-settings))

(provide
  (all-from-out racket/base)
  aload-with-main-settings
  anarki-init-in-main-namespace-verbose
  anarki-windows-cli
  tl-with-main-settings)


(define (anarki-windows-cli)

  (define repl 'maybe)

  (command-line
    #:program "arc"

    #:usage-help
    "If <file> is provided, execute it with the arguments <file_args>."
    "Otherwise, run an interactive session (REPL)."

    #:once-each
    [ ("-i")
      (
        "Always run an interactive session (REPL) even if <file> is"
        "provided. The file loads first.")
      (set! repl 'definitely)]

    #:ps
    ""
    "EXAMPLES"
    "    Start the Arc REPL:"
    "        arc"
    "    Run the file \"x.arc\", passing to it the argument '3':"
    "        arc x.arc 3"
    "    Run the file \"x.arc\", passing to it the argument '3' -- and then"
    "    start the Arc REPL:"
    "        arc -i x.arc 3"
    "    Run the file \"x.arc\", passing to it the arguments '-i' and '3':"
    "        arc x.arc -i 3"

    ; We pick these variable names in particular because they show up
    ; in the help message when the user runs "arc -h". We make `file`
    ; an optional argument.
    #:args ([file #f] . file-args)

    ; We reconstruct a full argument list out of `file` and
    ; `file-args`, and we modify the `current-command-line-arguments`
    ; parameter so it looks like the process started with those
    ; arguments. This approximates parity with arc.sh.
    ;
    (define args (if (eq? #f file) null (cons file file-args)))
    (current-command-line-arguments (list->vector args))

    (anarki-init-in-main-namespace-verbose)

    (unless (eq? #f file)

      ; A file has been given, so we execute it.
      (aload-with-main-settings file))

    (when
      (or
        (eq? 'definitely repl)
        (and (eq? 'maybe repl) (eq? #f file)))

      ; We start an interactive prompt.
      (tl-with-main-settings))))
