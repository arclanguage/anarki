(defmemo path-separator ()
  (if (is ($.system-type) 'windows) #\\ #\/))

(defmemo canonical-path (path)
 " builds an absolute file path from the root anarki directory "
  ($.normalize-path path ))

(defmemo canonical-path-ts (path)
" builds a canonical path with a trailing separator "
  (string (canonical-path path) (path-separator)))

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
  " Returns a list of the effective file permissions of `path'. "
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

(def dir-tree (path)
  " Returns a directory tree from the given path. "
  (if (~dir-exists path) path
      (map dir-tree (map [$.path->string:$.build-path path _] (dir path)))))

(def textfile-write (path . lines)
   (w/outfile outf path 
    (w/stdout outf
      (each line lines 
        (prn line)))))
