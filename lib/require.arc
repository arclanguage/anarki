(unless (and (bound 'required-files*) required-files*)
  (= required-files* (table)))

(def require (files)
  " Loads `file(s)' if it/they has not yet been `require'd.  (Can be fooled by changing
    the name ((require \"foo.arc\") as opposed to (require \"./foo.arc\")), but
    this should not be a problem.)
    See also [[load]]. "
  (each file (check files alist (list files))
    (zap string file)
    (or (required-files* file)
      (do (set required-files*.file)
          (load file)))))
