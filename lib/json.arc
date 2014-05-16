;wrapper for loading json.ss
($ (require (file "lib/json.ss")))
($ (xdef read-json read-json))
($ (xdef write-json write-json))