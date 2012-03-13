; Copyright ©1995-2006 Matthew Flatt
;
; Permission is granted to copy, distribute and/or modify this document under
; the terms of the GNU Library General Public License, Version 2 published by
; the Free Software Foundation.
;
; http://download.plt-scheme.org/doc/352/html/mzscheme/mzscheme-Z-H-11.html#node_sec_11.2.8

($:define (skip-whitespace port)
  ;; Skips whitespace characters, sensitive to the current
  ;; readtable's definition of whitespace
  (let ((ch (peek-char port)))
    (unless (eof-object? ch)
      ;; Consult current readtable:
      (let-values (((like-ch/sym proc dispatch-proc)
                    (readtable-mapping (current-readtable) ch)))
        ;; If like-ch/sym is whitespace, then ch is whitespace
        (when (and (char? like-ch/sym)
                   (char-whitespace? like-ch/sym))
          (read-char port)
          (skip-whitespace port))))))
