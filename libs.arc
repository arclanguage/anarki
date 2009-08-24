(map load '("strings.arc"
            "pprint.arc"
            "code.arc"
            "html.arc"
            "srv.arc"
            "app.arc"
            "prompt.arc"))

; We define 'require here, rather than in a file in load/ so that things in
; load/ can use it for load-ordering.
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

(def autoload ((o dirname "load"))
  " 'require each file in `dirname' ending in \".arc\".
    See also [[require]] "
  (each f (dir-exists&dir dirname)
    (if (endmatch ".arc" f) (require (+ dirname "/" f)))))

(autoload)
