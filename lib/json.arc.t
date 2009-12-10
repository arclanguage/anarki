(test-iso "json-read should parse digits"
  345
  (w/instring f "345" (json-read f)))

(test-iso "json-read should parse decimal points"
  0.25
  (w/instring f "0.25" (json-read f)))

(test-iso "json-read should parse sign"
  0.025
  (w/instring f "+0.025" (errsafe:json-read f)))

(test-iso "json-read should parse negative sign"
  -3.25
  (w/instring f "-3.25" (errsafe:json-read f)))

(test-iso "json-read should parse exponent"
  1000.0
  (w/instring f "1e3" (json-read f)))

(test-iso "json-read should parse negative exponent"
  0.001
  (w/instring f "1e-3" (json-read f)))
