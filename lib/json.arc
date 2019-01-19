;wrapper for loading json
($ (require json))
($.json-null nil)
(= read-json $.read-json)
(= write-json $.write-json)

(def json-string (j)
  (tostring (write-json j)))

; read JSON from file, return hash
(def read-json-file (file)
  (w/infile inf file 
    (read-json inf)))

; write object to JSON file
(def write-json-file (file j)
  (w/outfile outf file 
    (w/stdout outf (write-json j))))

