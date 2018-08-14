(require 'lib/json.arc)

(suite json
       (test parse-integer
             (assert-same 345
                          (w/instring f "345" (read-json f))))
       (test parse-decimal-point
             (assert-same 0.25
                          (w/instring f "0.25" (read-json f))))
       (test fail-to-parse-positive-sign
             ; The JSON standard requires positive numbers to be
             ; written without the sign.
             (assert-same nil
                          (w/instring f "+0.025"
                            (errsafe:read-json f))))
       (test parse-negative-sign
             (assert-same -3.25
                          (w/instring f "-3.25" (read-json f))))
       (test parse-positive-exponent
             (assert-same 1000.0
                          (w/instring f "1e3" (read-json f))))
       (test parse-negative-exponent
             (assert-same 0.001
                          (w/instring f "1e-3" (read-json f)))))

