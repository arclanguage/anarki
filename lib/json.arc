;wrapper for loading json
($ (require json))
($.json-null nil)
(= read-json $.read-json)
(= write-json $.write-json)
