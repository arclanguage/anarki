; Operations on bits.

;use: (require "bitops.scm")

(module bitops mzscheme

; so we can use xdef
(require "ac.scm")


(provide (all-defined))

(xdef 'bit-and bitwise-and)
(xdef 'bit-or bitwise-ior)
(xdef 'bit-not bitwise-not)
(xdef 'bit-xor bitwise-xor)
(xdef 'bit-shift arithmetic-shift)

)
