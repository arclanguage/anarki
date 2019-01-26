(def require (files)
"loads file(s) if they have not been required."
  (each file (check files alist (list files))
    ($.aload-unique (string file) )))