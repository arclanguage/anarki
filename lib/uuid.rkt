(module uuid racket/base
  (require ffi/unsafe)
  (provide uuid)

  (define uuid
    (get-ffi-obj "uuid_generate" (ffi-lib (if (eqv? (system-type 'os) 'macosx) "libSystem" "libuuid") '("1" ""))
      (_fun (out : _bytes = (make-bytes 16)) -> _void -> (uuid-unparse out))))

  (define uuid-unparse
    (get-ffi-obj "uuid_unparse" (ffi-lib (if (eqv? (system-type 'os) 'macosx) "libSystem" "libuuid") '("1" ""))
      (_fun (uuid : _bytes) (out : _bytes = (make-bytes 32)) -> _void -> (cast out _bytes _string/utf-8)))))

