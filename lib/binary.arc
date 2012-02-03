; originally from http://awwx.ws/binary1.arc

(require "lib/scheme.arc")

(extend type (x) (scheme.bytes? x)
  'binary)

(extend ac-literal (x) (errsafe:isa x 'binary)
  scheme-t)

(extend coerce (x totype . args) (is totype 'binary)
  (case (type x)
    binary x
    string (scheme.string->bytes/utf-8 x)
           (err "Can't coerce" x type)))

(def binary (x)
  (coerce x 'binary))

(extend coerce (x totype . args) (isa x 'binary)
  (case totype
    binary x
    string (scheme.bytes->string/utf-8 x)
           (err "Can't coerce" x type)))

(extend len (x) (isa x 'binary)
  (scheme.bytes-length x))
