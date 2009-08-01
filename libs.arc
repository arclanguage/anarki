(map load '("strings.arc"
            "pprint.arc"
            "code.arc"
            "html.arc"
            "srv.arc"
            "app.arc"
            "prompt.arc"))

; TODO: allow specification of dependencies for load-ordering in some way
(def autoload ((o dirname "load"))
  (each f (dir-exists&dir dirname)
    (if (endmatch ".arc" f) (load (+ dirname "/" f)))))

(autoload)
