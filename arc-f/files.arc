; Filesystem access. Added 02 Feb 08.

; adapted to arc-f by Stefano Dissegna 

(in-package fs)
(using <arc>v3)
(interface v1 mv cp mtime file-perms file-size cd pwd file-join qualified-path)

(def norm (x except)
  "unpackage x if it is a symbol"
  (if (and (no (mem x except)) (isa x 'sym)) (<arc>unpkg x) x))

(mac <fs>$ (except . body)
  "like <arc>$, but unpackage all symbols saving those in the 
   given exception list"
  `(<arc>$ ,@(treewise (fn (x y) (cons (norm x except) (norm y except)))
                       [norm _ except] body)))

(def mv (src dst)
   " Moves the file or directory `src' to `dst'. "
   ($ (src dst) (rename-file-or-directory ,src ,dst))
   nil)

(def cp (src dst)
   " Copies the file `src' to `dst'. "
   ($ (src dst) (copy-file ,src ,dst))
   nil)

(def mtime (path)
   " Returns the modification time of the file or directory `path' in
     seconds since the epoch. "
   ($ (path) (file-or-directory-modify-seconds ,path)))

(def file-perms (path)
   " Returns a list of the effective file permssions of `path'. "
   ($ (path) (file-or-directory-permissions ,path)))

(def file-size (path)
   " Returns the size, in bytes, of a file `path'. "
   ($ (path) (file-size ,path)))

(def cd (path)
   " Changes the current directory. "
   ($ (path) (current-directory ,path))
   (pwd))

(def pwd ()
   " Returns the current directory. "
   ($ () (path->string (current-directory))))

; This could be defined in pure Arc, but we don't really have a good way of
; testing whether or not we should use Windows path separators,
; so we'll leave it up to Scheme.
(def file-join parts
     " Joins `parts' into a path string. "
     ($ (parts) (path->string (build-path ,@parts))))

(def qualified-path (path)
     " Returns the fully-qualified path of a possibly relative `path'. "
     ($ (path) (path->string (simplify-path (build-path (simplify-path (path->complete-path ,path)) ".")))))
