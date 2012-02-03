(unless (and (bound 'required-files*) required-files*)
  (= required-files* (table)))

(def require (file)
  " Loads `file' if it has not yet been `require'd.  Can be fooled by changing
    the name ((require \"foo.arc\") as opposed to (require \"./foo.arc\")), but
    this should not be a problem.
    See also [[load]]. "
  (or (required-files* file)
    (do (set required-files*.file)
        (load file))))
