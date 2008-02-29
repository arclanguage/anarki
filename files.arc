; Filesystem access. Added 02 Feb 08.

(def mv (src dst)
   " Moves the file or directory `src' to `dst'. "
   ($ (rename-file-or-directory ,src ,dst))
   nil)

(def cp (src dst)
   " Copies the file `src' to `dst'. "
   ($ (copy-file ,src ,dst))
   nil)

(def mtime (path)
   " Returns the modification time of the file or directory `path' in
     seconds since the epoch. "
   ($ (file-or-directory-modify-seconds ,path)))

(def file-perms (path)
   " Returns a list of the effective file permssions of `path'. "
   ($ (file-or-directory-permissions ,path)))

(def file-size (path)
   " Returns the size, in bytes, of a file `path'. "
   ($ (file-size ,path)))

(def cd (path)
   " Changes the current directory. "
   ($ (current-directory ,path))
   (pwd))

(def pwd ()
   " Returns the current directory. "
   ($ (path->string (current-directory))))

(def mkdir (path (o parents))
   " Creates a directory.
     If `parents' is non-nil, parent directories are created as needed."
   ((let os (which-os)
      (if
        ; If we're running Unix, MzScheme <371 has a bug
        ; where make-directory sets the sticky bit.
        ; Thus, we want to use system instead.
        (or (is os 'unix) (is os 'macosx))
         [system (string
                   "mkdir " (if parents "-p ") _)]
        ($ (begin (require (lib "file.ss"))
                  (if (null? ,parents) make-directory make-directory*)))))
    path)
   nil)

; This could be defined in pure Arc, but we don't really have a good way of
; testing whether or not we should use Windows path separators,
; so we'll leave it up to Scheme.
(def file-join parts
     " Joins `parts' into a path string. "
     ($ (path->string (build-path ,@parts))))

(def qualified-path (path)
     " Returns the fully-qualified path of a possibly relative `path'. "
     ($ (path->string (simplify-path (build-path (simplify-path (path->complete-path ,path)) ".")))))

(def ensure-dir (path)
  " Ensures that the specified directory exists, and creates it if not
    yet created. "
  (unless (dir-exists path)
          (mkdir path t)))
