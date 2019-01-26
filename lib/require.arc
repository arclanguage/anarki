(def require (files)
"loads file(s) if they have not been required."
  (each file (check files alist (list files))
    ($.aload-unique (string file) )))

(def list-required-files ()
  "returns a list of the absolute filepaths loaded by requre."
  ($.hash-values $.arc-loaded-files))