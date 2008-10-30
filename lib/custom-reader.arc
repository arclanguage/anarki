; custom-reader.arc

; Written by Chris Hooper, released into the public domain

; set the reader to a given function
(def set-reader (f)
  ($.set-reader f))

; restore Arc's default reader
(def restore-reader ()
  ($.set-reader $.read))
