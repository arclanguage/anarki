; file-table.arc
; by AmkG
; A persistent string-to-string table system for Arc

(require "lib/settable-fn.arc")

(let (file-contents file-write file-remove ctt mtt get-args) nil
  ; efficiency concern - not sure if buffering is done
  ; automatically in this case, and even so, there might
  ; be a better way of doing this.
  (= file-contents
     (fn (file)
       " Returns the contents of `file' as a string "
       (w/infile p file
         (tostring
           (whilet c (readc p)
             (writec c))))))

  (= file-write
     (fn (file s)
       " Writes the string `s' to `file' "
       (w/outfile p file
         ; is using 'disp correct?
         (disp s p))))

  (= file-remove rmfile)

  (def file-table-valid-key (f)
    " Determines if `f' is valid for use as a key in
      a file-table.
      See also [[file-table]] "
    (and (isnt f ".") (isnt f "..") (~re-match "[]:/\\*?[]" f)))

  (def file-table-directory (f)
    (get-attachment 'file-table-directory f))

  ; global caches
  ; warning.  Since Arc (unlike underlying scheme) does
  ; *not* have "weak references", we are thus compelled to
  ; periodically uncache these.
  ; TODO: uncaching thingy
  (= ctt (table) mtt (table))

  (= get-args
    (fn (args . opts)
      (with (pargs (pair args)
             popts (pair opts))
        (map [let (k v) _
               (aif (assoc k pargs)
                    (cadr it)
                    v)] popts))))

  (def file-table args
    " Creates a special table-like object whose keys are symbol or
      strings representing files.  Files are stored in `path'.
      A file-table's keys can be assigned to in order to update
      the specified file.
      You may also use tagged 'fromfile and 'tofile arguments to
      specify a function that will accept a string (used when reading
      from a file) and a function that will emit a string (used when
      writing to a file).
      Usage: (file-table [path ['fromfile <fn>] ['tofile <fn>]])
      See also [[file-table-w/read]] "
    (let (tofile fromfile) (get-args (cdr args) 'tofile idfn 'fromfile idfn)
      (let (reader writer checkf validf keys
            mt ct path prepath) nil
        (if args (= path (car args)) (= path "."))
        (zap qualified-path path)
        (if (file-exists path) (err:tostring:write "file-table: file exists - " path))
        (ensure-dir path)
        (= mt (or (mtt path) (= (mtt path) (table)))) ; modification times table
        (= ct (or (ctt path) (= (ctt path) (table)))) ; file contents table
        (= checkf
           (fn (f)
             (if
               (~isa f 'string)
                 (err:tostring:write "file-table reference: key is not a string - " f)
               (~file-table-valid-key f)
                 (err:tostring:write "file-table reference: key is not a valid filename - " f))))
        (= prepath
           (fn (s) (file-join path s)))
        (= keys
           (fn ()
             ; can possibly memoize this, but not sure if mtime on
             ; the directory will change if a file is deleted /
             ; added to it
             (keep (andf file-table-valid-key file-exists:prepath) (dir path))))
        (= reader
           (fn (f)
             ; silently transform symbols to strings, just for
             ; the foo!bar syntax
             (if (isa f 'sym) (= f (string f)))
             (checkf f)
             (let pf (prepath f)
               (if (file-exists pf)
                 ; figure out if it's already memoized and that
                 ; the file hasn't been updated by something else
                 (if (aand (mt f) (is it (mtime pf)))
                     (fromfile (ct f))
                     ; not yet, or file has been updated - load it
                     (do
                       (= (ct f) (file-contents pf))
                       (= (mt f) (mtime pf))
                       (fromfile (ct f))))
                 ; doesn't exist - delete it from the table just in
                 ; case the table does have it (i.e. if the file
                 ; was deleted in the filesystem).
                 (= (mt f) nil (ct f) nil)))))
        (= writer
           (fn (v f)
             ; silently transform symbols to strings
             (if (isa f 'sym) (= f (string f)))
             (checkf f)
             (let pf (prepath f)
               (if (dir-exists pf)
                 (err:tostring:write "file-table writer: key " f " would overwrite directory - " pf)
                 (if
                   v
                     (let sv (tofile v)
                       (if (~isa sv 'string)
                         (err:tostring:write "file-table writer: value " v " not a string in 'tofile " tofile))
                       (file-write pf sv)
                       (= (mt f) (mtime pf))
                       (= (ct f) sv)
                       v)
                   ; set to nil - delete the file if it exists
                   (file-exists pf)
                     (do
                       (file-remove pf)
                       (= (mt f) nil (ct f) nil))
                   ; else just clear any table entries
                     (= (mt f) nil (ct f) nil))))))
        ; the fake table we return
        (add-attachments
           'keys keys
           '= writer
           'file-table-directory path
           (annotate 'table reader)))))

  (def *file-table-debug ()
    (prn "ctt :")
    (prn ctt)
    (prn "mtt :")
    (prn mtt)))

(def file-table-w/read ((o path "."))
  " Creates a file-table which can store any object that
    can be trivially 'read using the Arc reader.
    See also [[file-table]] [[read]] "
  (file-table path
    'tofile [tostring (write _)]
    'fromfile readfile1))

