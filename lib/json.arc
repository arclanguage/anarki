;wrapper for loading json
($ (require json))
($.json-null nil)
(= read-json $.read-json)
(= write-json $.write-json)

; read in JSON string, return hash
(def read-json-fromstring (str (o port stdin))
  (fromstring str 
    (read-json (port))))

; read JSON from file
(def read-json-file (file)
    (w/infile inf file (read-json inf)))

; create JSON from table, hash or other object
; TODO: test that (read-json-file "test.json") 
; and (obj-to-json (read-json-file "test.json"))
; are equivalent
(def obj-to-json (obj)
    (tostring (write-json obj)))

; write object to JSON file, disp won't escape quotes
(def write-json-file (file obj)
    (w/outfile outf file 
        (w/stdout outf (disp (obj-to-json obj)))))

