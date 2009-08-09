(def mv (src dst)
  " Moves the file or directory `src' to `dst'. "
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
  ($ (path->string (current-directory))))

; This could be defined in pure Arc, but we don't really have a good way of
; testing whether or not we should use Windows path separators,
; so we'll leave it up to Scheme.
(def file-join parts
  " Joins `parts' into a path string. "
  ($.path->string (apply $.build-path parts)))

(def qualified-path (path)
  " Returns the fully-qualified path of a possibly relative `path'. "
  ($ (path->string (simplify-path (path->complete-path ,path)))))
