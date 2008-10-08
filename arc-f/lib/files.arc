; Filesystem access. Added 02 Feb 08.

(in-package files)
(using <arc>v3)
(using <arc>v3-on-mzscheme)
(interface v1
  mv cp mtime file-perms file-size cd pwd ls
  file-join qualified-path)

(def mv (src dst)
   " Renames the file or directory `src' to `dst'. "
   ($.rename-file-or-directory src dst)
   nil)

(def cp (src dst)
   " Copies the file `src' to `dst'. "
   ($.copy-file src dst)
   nil)

(def mtime (path)
   " Returns the modification time of the file or directory `path' in
     seconds since the epoch. "
   ($.file-or-directory-modify-seconds path))

(def file-perms (path)
   " Returns a list of the effective file permssions of `path'. "
   ($.file-or-directory-permissions path))

(def file-size (path)
   " Returns the size, in bytes, of a file `path'. "
   ($.file-size path))

(def cd (path)
   " Changes the current directory. "
   ($.current-directory path)
   (pwd))

(def pwd ()
   " Returns the current directory. "
   ($.path->string ($.current-directory)))

(def ls ((o d "."))
   " Returns a list of files. "
   (dir d))

(= build-path
   $.build-path)
(= path->string
   $.path->string)
(= simplify-path
   $.simplify-path)
(= path->complete-path
   $.path->complete-path)

; This could be defined in pure Arc, but we don't really have a good way of
; testing whether or not we should use Windows path separators,
; so we'll leave it up to Scheme.
(= file-join
   path->string:build-path)
(docstring 'file-join 'fn 'parts
  " Joins `parts` into a path string ")

(def qualified-path (path)
     " Returns the fully-qualified path of a possibly relative `path'. "
     (path->string:simplify-path:build-path
        (simplify-path:path->complete-path path)
        "."))


