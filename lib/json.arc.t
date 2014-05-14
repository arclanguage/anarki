($ (require (file "lib/json.ss")))
($ (xdef json-read read-json))

(suite json
       parse-integer (assert-same 345
                                  (w/instring f "345" (json-read f)))
       parse-decimal-point (assert-same 0.25
                                        (w/instring f "0.25" (json-read f)))
       parse-positive-sign (assert-same 0.025
                                        (w/instring f "+0.025" (errsafe:json-read f)))
       parse-negative-sign (assert-same -3.25
                                        (w/instring f "-3.25" (errsafe:json-read f)))
       parse-positive-exponent (assert-same 1000.0
                                            (w/instring f "1e3" (json-read f)))
       parse-negative-exponent (assert-same 0.001
                                            (w/instring f "1e-3" (json-read f))))
