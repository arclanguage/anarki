; file-table.arc
; by AmkG
; A persistent string-to-string table system for Arc

(require "lib/settable-fn.arc")

(let (file-contents file-write file-remove) nil
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
  ; as far as I know, not imported anywhere in base arc
  ; TODO: change this once we figure out what the delete-file
  ; primitive is in Arc.
  (= file-remove ($ delete-file))
  (def file-table-valid-key (f)
    " Determines if `f' is valid for use as a key in
      a file-table.
      See also [[file-table]] "
    (and (isnt f ".") (isnt f "..") (~re-match "[]:/\\*?[]" f)))
  ; should really memoize on the result of path-expanding
  ; the given path, i.e. if given foo/bar, it should
  ; memoize on /home/arcstuff/foo/bar , or something.
  (defmemo file-table (path)
    " Creates a special table-like object whose keys are symbol or
      strings representing files.  Files are stored in `path'.
      A file-table's keys can be assigned to with strings to update
      the specified file. "
    (if (file-exists path) (err:string "file-table: file exists - " path))
    (ensure-dir path)
    (let (reader writer checkf validf keys
          mt ct) nil
      (= mt (table)) ; modification times table
      (= ct (table)) ; file contents table
      (= checkf
         (fn (f)
           (if
             (~isa f 'string)
               (err:string "file-table reference: key is not a string - " f)
             (~file-table-valid-key f)
               (err:string "file-table reference: key is not a valid filename - " f))))
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
                   (ct f)
                   ; not yet, or file has been updated - load it
                   (do
                     (= (ct f) (file-contents pf))
                     (= (mt f) (mtime pf))
                     (ct f)))
               ; doesn't exist - delete it from the table just in
               ; case the table does have it (i.e. if the file
               ; was deleted in the filesystem).
               (= (mt f) nil (ct f) nil)))))
      (= writer
         (fn (v f)
           ; silently transform symbols to strings
           (if (isa f 'sym) (= f (string f)))
           (if (isa v 'sym) (= v (string v)))
           (checkf f)
           (if (~isa v 'string)
             (err:string "file-table writer: value is not a string - " v))
           (let pf (prepath f)
             (if (dir-exists pf)
               (err:string "file-table writer: key " f " would overwrite directory - " pf)
               (if
                 v
                   (do
                     (file-write pf v)
                     (= (mt f) (mtime pf))
                     (= (ct f) v)
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
         (annotate 'table reader)))))

