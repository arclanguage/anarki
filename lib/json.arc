;wrapper for loading json.ss
($ (require (file "lib/json.ss")))
(= read-json $.read-json)
(= write-json $.write-json)
