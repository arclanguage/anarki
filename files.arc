; Filesystem access. Added 02 Feb 08.

(def mv (src dst)
   ($ (rename-file-or-directory ,src ,dst))
   nil)

(def cp (src dst)
   ($ (copy-file ,src ,dst))
   nil)

(def mtime (path)
   ($ (file-or-directory-modify-seconds ,path)))

(def file-perms (path)
   ($ (file-or-directory-permissions ,path)))

(def file-size (path)
   ($ (file-size ,path)))

(def pwd ()
   ($ (path->string (current-directory))))

(def mkdir (path)
   ($ (make-directory path))
   nil)

; This could be defined in pure Arc, but we don't really have a good way of
; testing whether or not we should use Windows path separators,
; so we'll leave it up to Scheme.
(def file-join parts
     ($ (path->string (build-path ,@parts))))
