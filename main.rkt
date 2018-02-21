#lang racket/base

; This is the primary interface to Anarki as a Racket library. It can
; be accessed like so:
;
;   $ raco pkg install anarki
;   $ racket
;   > (require anarki)
;   > (anarki)
;   arc>
;
; When you run the REPL this way, you can return to Racket with a
; special ":a" command, like so:
;
;   arc> :a


; NOTE: When accessing the Anarki REPL this way on Linux, there are
; some cosmetic quirks in the interface. The first "arc>" prompt's
; input begins just below the prompt rather than on the same line, and
; subsequent prompts don't display the "arc>" prompt until after the
; input has been given. These are artifacts of the Racket REPL's
; Readline implementation, as implemented by Racket's XREPL module,
; which is implemented with Racket's Readline module, which relies on
; an external dependency on the user's choice of either the Editline
; library or the GNU Readline library for its implementation.
;
; An undocumented feature of XREPL makes it possible to disable
; Readline support altogether by setting the "TERM" envrionment
; variable to "dumb" when running Racket at the command line, like so:
;
;   $ TERM=dumb racket
;
; This does correct the quirky-looking "arc>" prompt, but since it
; disables all Readline support, it may be less convenient overall. If
; your goal is REPL use from the command line, running ./arc.sh may be
; a more reliable option:
;
;   $ ./arc.sh
;   arc>
;
; TODO: See if there's any way we can ameliorate these prompt quirks.
; Someday the XREPL or Readline modules may update to offer better
; customization hooks for purposes like ours.


(require anarki/boot)

(provide
  anarki-init
  anarki-init-in-main-namespace
  anarki-init-verbose
  anarki-init-in-main-namespace-verbose
  (rename-out
    [arc-arc-path anarki-path]
    [main-namespace anarki-main-namespace]
    [tl anarki-repl]
    [aload anarki-load]
    [arc-eval anarki-eval])
  anarki)


; launch anarki repl from the anarki package folder
(define (anarki)
  (anarki-init-in-main-namespace-verbose)
  (parameterize ([current-directory arc-arc-path]
                 [current-namespace (main-namespace)])
    (tl)))
