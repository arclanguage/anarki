;;; inferior-arc.el --- Arc process in a buffer. Adapted from cmuscheme.el

;; Copyright (C) 1988, 1994, 1997, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Olin Shivers <olin.shivers@cs.cmu.edu>
;; Keywords: processes, lisp, arc

;; Adapted for Arc by Sami Samhuri <sami.samhuri@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This code is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; inferior-arc.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with inferior-arc.el; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;    This is a customization of comint-mode (see comint.el)
;;
;; Written by Olin Shivers (olin.shivers@cs.cmu.edu). With bits and pieces
;; lifted from scheme.el, shell.el, clisp.el, newclisp.el, cobol.el, et al..
;; 8/88
;;
;; Please send me bug reports, bug fixes, and extensions, so that I can
;; merge them into the master source.
;;
;; The changelog is at the end of this file.
;;
;;; CHANGE LOG
;;; ===========================================================================
;;; 8/88 Olin
;;; Created.
;;;
;;; 2/15/89 Olin
;;; Removed -emacs flag from process invocation. It's only useful for
;;; cscheme, and makes cscheme assume it's running under xscheme.el,
;;; which messes things up royally. A bug.
;;;
;;; 5/22/90 Olin
;;; - Upgraded to use comint-send-string and comint-send-region.
;;; - run-scheme now offers to let you edit the command line if
;;;   you invoke it with a prefix-arg. M-x scheme is redundant, and
;;;   has been removed.
;;; - Explicit references to process "scheme" have been replaced with
;;;   (scheme-proc). This allows better handling of multiple process bufs.
;;; - Added scheme-send-last-sexp, bound to C-x C-e. A gnu convention.
;;; - Have not added process query facility a la cmulisp.el's lisp-show-arglist
;;;   and friends, but interested hackers might find a useful application
;;;   of this facility.
;;;
;;; 3/12/90 Olin
;;; - scheme-load-file and scheme-compile-file no longer switch-to-scheme.
;;;   Tale suggested this.
;;;
;;; 2/08/08 sjs
;;; - Adapted for Arc (basically s/scheme/arc/g)

;;; Code:

(require 'arc)
(require 'comint)


(defgroup arc nil
  "Run an Arc process in a buffer."
  :group 'arc)

;;; INFERIOR ARC MODE STUFF
;;;============================================================================

(defcustom inferior-arc-mode-hook nil
  "*Hook for customizing inferior-arc mode."
  :type 'hook
  :group 'arc)

(defvar inferior-arc-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "\M-\C-x" 'arc-send-definition) ;gnu convention
    (define-key m "\C-x\C-e" 'arc-send-last-sexp)
    (define-key m "\C-c\C-l" 'arc-load-file)
    m))

(defvar arc-program-name "arc --no-rl"
  "The name of the program used to run Arc.")

;; Install the process communication commands in the arc-mode keymap.
(define-key arc-mode-map "\M-\C-x" 'arc-send-definition);gnu convention
(define-key arc-mode-map "\C-x\C-e" 'arc-send-last-sexp);gnu convention
(define-key arc-mode-map "\C-c\C-e" 'arc-send-definition)
(define-key arc-mode-map "\C-c\M-e" 'arc-send-definition-and-go)
(define-key arc-mode-map "\C-c\C-r" 'arc-send-region)
(define-key arc-mode-map "\C-c\M-r" 'arc-send-region-and-go)
(define-key arc-mode-map "\C-c\C-x" 'arc-expand-current-form)
(define-key arc-mode-map "\C-c\C-z" 'switch-to-arc)
(define-key arc-mode-map "\C-c\C-l" 'arc-load-file)

(let ((map (lookup-key arc-mode-map [menu-bar arc])))
  (define-key map [separator-eval] '("--"))
  (define-key map [load-file]
    '("Load Arc File" . arc-load-file))
  (define-key map [switch]
    '("Switch to Arc" . switch-to-arc))
  (define-key map [exp-form]
    '("Expand current form" . arc-expand-current-form))
  (define-key map [send-def-go]
    '("Evaluate Last Definition & Go" . arc-send-definition-and-go))
  (define-key map [send-def]
    '("Evaluate Last Definition" . arc-send-definition))
  (define-key map [send-region-go]
    '("Evaluate Region & Go" . arc-send-region-and-go))
  (define-key map [send-region]
    '("Evaluate Region" . arc-send-region))
  (define-key map [send-sexp]
    '("Evaluate Last S-expression" . arc-send-last-sexp))
  )

(defvar arc-buffer)

(define-derived-mode inferior-arc-mode comint-mode "Inferior Arc"
  "Major mode for interacting with an inferior Arc process.

The following commands are available:
\\{inferior-arc-mode-map}

An Arc process can be fired up with M-x run-arc.

Customization: Entry to this mode runs the hooks on comint-mode-hook and
inferior-arc-mode-hook (in that order).

You can send text to the inferior Arc process from other buffers containing
Arc source.
    switch-to-arc switches the current buffer to the Arc process buffer.
    arc-send-definition sends the current definition to the Arc process.
    arc-send-region sends the current region to the Arc process.

    arc-send-definition-and-go and arc-send-region-and-go
        switch to the Arc process buffer after sending their text.
For information on running multiple processes in multiple buffers, see
documentation for variable arc-buffer.

Commands:
Return after the end of the process' output sends the text from the
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
Tab indents for Arc; with argument, shifts rest
    of expression rigidly with the current line.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  Semicolons start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  ;; Customize in inferior-arc-mode-hook
  (arc-mode-variables)
  (set (make-local-variable 'comint-prompt-regexp) "^[^>\n]*>+ *")
  (set (make-local-variable 'comint-input-filter) (function arc-input-filter))
  (set (make-local-variable 'comint-get-old-input) (function arc-get-old-input))
  (setq mode-line-process '(":%s")))

(defcustom inferior-arc-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "*Input matching this regexp are not saved on the history list.
Defaults to a regexp ignoring all inputs of 0, 1, or 2 letters."
  :type 'regexp
  :group 'arc)

(defun arc-input-filter (str)
  "Don't save anything matching `inferior-arc-filter-regexp'."
  (not (string-match inferior-arc-filter-regexp str)))

(defun arc-get-old-input ()
  "Snarf the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

;;;###autoload
(defun run-arc (cmd)
  "Run an inferior Arc process, input and output via buffer `*arc*'.
If there is a process already running in `*arc*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `arc-program-name').
Runs the hook `inferior-arc-mode-hook' \(after the `comint-mode-hook'
is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"

  (interactive (list (if current-prefix-arg
                         (read-string "Run Arc: " arc-program-name)
                         arc-program-name)))
  (when (not (comint-check-proc "*arc*"))
    (let ((cmdlist (split-string cmd)))
      (set-buffer (apply 'make-comint "arc" (car cmdlist)
                         nil (cdr cmdlist)))
      (inferior-arc-mode)))
  (setq arc-program-name cmd)
  (setq arc-buffer "*arc*")
  (pop-to-buffer "*arc*"))
;;;###autoload (add-hook 'same-window-buffer-names "*arc*")

(defun arc-send-region (start end)
  "Send the current region to the inferior Arc process."
  (interactive "r")
  (comint-send-region (arc-proc) start end)
  (comint-send-string (arc-proc) "\n"))

(defun arc-send-definition ()
  "Send the current definition to the inferior Arc process."
  (interactive)
  (save-excursion
   (end-of-defun)
   (let ((end (point)))
     (beginning-of-defun)
     (arc-send-region (point) end))))

(defun arc-send-last-sexp ()
  "Send the previous sexp to the inferior Arc process."
  (interactive)
  (arc-send-region (save-excursion (backward-sexp) (point)) (point)))

(defun arc-expand-current-form ()
  "Macro-expand the form at point in the inferior Arc process."
  (interactive)
  (let ((current-form (arc-form-at-point)))
    (if current-form
        (progn
          (comint-send-string (arc-proc)
                              (format "(macex1 '%s)" current-form))
          (comint-send-string (arc-proc) "\n"))
      (error "Not at a form"))))

(defun arc-form-at-point ()
  (let ((next-sexp (thing-at-point 'sexp)))
    (if (and next-sexp (string-equal (substring next-sexp 0 1) "("))
        next-sexp
      (save-excursion
        (backward-up-list)
        (arc-form-at-point)))))

(defun switch-to-arc (eob-p)
  "Switch to the arc process buffer.
With argument, position cursor at end of buffer."
  (interactive "P")
  (if (or (and arc-buffer (get-buffer arc-buffer))
          (arc-interactively-start-process))
      (pop-to-buffer arc-buffer)
    (error "No current process buffer.  See variable `arc-buffer'"))
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

(defun arc-send-region-and-go (start end)
  "Send the current region to the inferior Arc process.
Then switch to the process buffer."
  (interactive "r")
  (arc-send-region start end)
  (switch-to-arc t))

(defun arc-send-definition-and-go ()
  "Send the current definition to the inferior Arc.
Then switch to the process buffer."
  (interactive)
  (arc-send-definition)
  (switch-to-arc t))

(defcustom arc-source-modes '(arc-mode)
  "*Used to determine if a buffer contains Arc source code.
If it's loaded into a buffer that is in one of these major modes,
it's considered a arc source file by `arc-load-file'.  Used by
these commands to determine defaults."
  :type '(repeat function)
  :group 'arc)

(defvar arc-prev-load-dir/file nil
  "Caches the last (directory . file) pair.
Caches the last pair used in the last `arc-load-file' command.
Used for determining the default in the next one.")

(defun arc-load-file (file-name)
  "Load a Arc file FILE-NAME into the inferior Arc process."
  (interactive (comint-get-source "Load Arc file: " arc-prev-load-dir/file
                                  arc-source-modes t)) ; t because `load'
                                                       ; needs an exact name
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq arc-prev-l/c-dir/file (cons (file-name-directory    file-name)
                                       (file-name-nondirectory file-name)))
  (comint-send-string (arc-proc) (concat "(load \""
                                            file-name
                                            "\"\)\n")))


(defvar arc-buffer nil "*The current arc process buffer.

MULTIPLE PROCESS SUPPORT
===========================================================================
inferior-arc.el supports, in a fairly simple fashion, running multiple Arc
processes.  To run multiple Arc processes, you start the first up with
\\[run-arc].  It will be in a buffer named *arc*.  Rename this buffer
with \\[rename-buffer].  You may now start up a new process with another
\\[run-arc].  It will be in a new buffer, named *arc*.  You can
switch between the different process buffers with \\[switch-to-buffer].

Commands that send text from source buffers to Arc processes -- like
`arc-send-definition' -- have to choose a process to send to, when you
have more than one Arc process around.  This is determined by the
global variable `arc-buffer'.  Suppose you have three inferior Arcs
running:
    Buffer      Process
    foo         arc
    bar         arc<2>
    *arc*    arc<3>
If you do a \\[arc-send-definition-and-go] command on some Arc source
code, what process do you send it to?

- If you're in a process buffer (foo, bar, or *arc*),
  you send it to that process.
- If you're in some other buffer (e.g., a source file), you
  send it to the process attached to buffer `arc-buffer'.
This process selection is performed by function `arc-proc'.

Whenever \\[run-arc] fires up a new process, it resets `arc-buffer'
to be the new process's buffer.  If you only run one process, this will
do the right thing.  If you run multiple processes, you can change
`arc-buffer' to another process buffer with \\[set-variable].

More sophisticated approaches are, of course, possible.  If you find yourself
needing to switch back and forth between multiple processes frequently,
you may wish to consider ilisp.el, a larger, more sophisticated package
for running inferior Lisp and Arc processes.  The approach taken here is
for a minimal, simple implementation.  Feel free to extend it.")

(defun arc-proc ()
  "Return the current Arc process, starting one if necessary.
See variable `arc-buffer'."
  (unless (and arc-buffer
               (get-buffer arc-buffer)
               (comint-check-proc arc-buffer))
    (arc-interactively-start-process))
  (or (arc-get-process)
      (error "No current process.  See variable `arc-buffer'")))

(defun arc-get-process ()
  "Return the current Arc process or nil if none is running."
  (get-buffer-process (if (eq major-mode 'inferior-arc-mode)
                          (current-buffer)
                        arc-buffer)))

(defun arc-interactively-start-process (&optional cmd)
  "Start an inferior Arc process.  Return the process started.
Since this command is run implicitly, always ask the user for the
command to run."
  (save-window-excursion
    (run-arc (read-string "Run Arc: " arc-program-name))))

;;; Do the user's customization...

(defcustom inferior-arc-load-hook nil
  "This hook is run when inferior-arc is loaded in.
This is a good place to put keybindings."
  :type 'hook
  :group 'arc)

(run-hooks 'inferior-arc-load-hook)

(provide 'inferior-arc)

;;; inferior-arc.el ends here
