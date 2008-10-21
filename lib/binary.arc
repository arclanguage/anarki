; binary.arc
;
; This lib exposes the byte-string type from MzScheme to Arc. Byte-strings are
; arrays of binary data that look a lot like strings. They are created with
; newbytes (just like newstring) and can be read from/written to a port using 
; readbytes and writebytes. They also support array-index syntax (b n).
;
; Written by Chris Hooper and released into the public domain.

(def abytes (x)
  (isa x 'bytes))

(= newbytes $.make-bytes)

(defcall bytes (b n) ($.bytes-ref b n))

(redef coerce (x t)
   (if (is t 'bytes) (coerce-to-bytes x) 
       (~isa x 'bytes) (old x t)
       (coerce-from-bytes x t)))

(def coerce-to-bytes (x)
  (case (type x)
    cons ($.list->bytes x)
    string ($.string->bytes/utf-8 x)
    (err "Can't coerce" x 'bytes)))

(def coerce-from-bytes (x t)
  (case t
    cons ($.bytes->list x)
    string ($.bytes->string/utf-8 x)
    (err "Can't coerce" x t)))

(redef len (x)
  (if (abytes x) ($.bytes-length x)
      (old x)))

; Binary I/O

(def peekb (port)
  (withs (p (if port port stdin)
          b ($.peek-byte p))
    (if ($.eof-object? b) nil b)))

; read (at most) n bytes from a port
(def readbytes (n port)
  (let b ($.read-bytes n port)
    (if ($.eof-object? b) nil b)))

; write a byte-string to a port
(def writebytes (b (o port stdout) (o start 0) (o end (len b)))
  ($.write-bytes b port start end))

; byte ports are the binary equivalent of string ports
(= inbytes $.open-input-bytes)
(= outbytes $.open-output-bytes)
(mac w/inbytes (var bs . body) 
   `(let ,var (inbytes ,bs)
     (after (do ,@body) (close ,var))))
(mac w/outbytes (var . body) `(let ,var (outbytes) ,@body))
