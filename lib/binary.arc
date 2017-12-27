; originally from http://awwx.ws/binary1.arc

(defextend type (x) ($.bytes? x)
  'binary)

(defextend ac-literal (x) (errsafe:isa x 'binary)
  scheme-t)

(defextend coerce (x totype . args) (is totype 'binary)
  (case (type x)
    binary x
    string ($.string->bytes/utf-8 x)
           (err "Can't coerce" x type)))

(def binary (x)
  (coerce x 'binary))

(defextend coerce (x totype . args) (isa x 'binary)
  (case totype
    binary x
    string ($.bytes->string/utf-8 x)
           (err "Can't coerce" x type)))

(defextend len (x) (isa x 'binary)
  ($.bytes-length x))

(defextend iso (x y) (isa x 'binary)
  (ac-denil:$.bytes=? x y))
