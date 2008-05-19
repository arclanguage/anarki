(= caddr cadr:cdr)
(= cadddr caddr:cdr)

(mac w/ostring (str . body)
  `(w/outstring ,str 
     ,@body
     (inside ,str)))

; some constants

(= *ter* "\r\n")

(= *ret* "\n")

(= *http-port* 80)

(def ter-line (line)
  "tells if line is a termination line"
  (or (is (len line) 0) (is (line 0) #\Return)))

; actions to take in response to a code

(= *response-actions* (table))

(mac def-on-code (f args codes . body)
  "associates an action to take in response to codes"
  (with (stream (car args) resp (cadr args)) 
    `(do
       (def ,f (,stream ,resp) ,@body)
       ,@(map (fn (code) `(= (*response-actions* ,code) ,f)) codes))))

(def dispatch-on-code (code stream resp)
  "calls action corresponding to code"
  (aif (*response-actions* code)
    (it stream resp)
    (err (string "Unknown code: " code))))

; header management

(mac each-line (var stream until . body)
  (w/uniq (f s u)
    `(with (,s ,stream ,f nil ,u ,until)
       (= ,f (fn (,var)
               (when (and ,var (no (,u ,var)))
                 (do ,@body (,f (readline ,s))))))
       (,f (readline ,s)))))

(def read-header (stream)
  "read the header and returns an hash table representing it"
  (with (header (table) oldname nil)
    (each-line line stream ter-line
      (if (and oldname (mem (line 0) '(#\Space #\Tab))) ; continuation?
        (= (header oldname) ; append to preceding value 
           (string (header oldname) 
                   (trim line 'both [pos _ " \t\r\n"])))
        (let div (pos #\: line) ; new field
          (if (no div) (err "Malformed header"))
            (= oldname (upcase (cut line 0 div)))
            (= (header oldname)
               (trim (cut line (+ 1 div)) 'both [pos _ " \t\r"])))))
    header))

(def skip-header (stream)
  (when (no (ter-line (readline stream)))
    (skip-header stream)))

(def mk-header header
  "builds a header"
  (w/ostring s
    (each h header
      (with (name (car h) value (cadr h))
        (disp (string name ": " value *ter*) s)))
    (disp *ter* s)))

; body

; chunked data
(def read-chunk-size (s)
  "read chunk length"
  (aif (readline s)
    (on-err (fn (x) 0)
            (fn ()
              (let n (read (instring (string "#x" it)))
                (if (no (exact n))
                  (err "Cannot read chunk size")
                  n))))
    (err "Cannot read chunk size line")))

(def read-chunk (s size)
  "read a chunck of length size"
  (let chunk (w/ostring o (echo-upto s o size))
    (readline s) ; go to new line
    chunk))

(def read-all-chunks (s)
  "read all the chunks of the message and the footer"
  (with (body (w/ostring chunks
                (let size (read-chunk-size s)
                   (while (> size 0)
                     (disp (read-chunk s size) chunks)
                     (= size (read-chunk-size s)))))
         footer (read-header s)) ; footer and header have the same structure
    (list body footer)))

(def echo-upto (s-in s-out n)
  "echoes n bytes from stream s-in to stream s-out, or until s-in is
   finished"
  (unless (<= n 0)
    (awhen (readb s-in)
      (writeb it s-out)
      (echo-upto s-in s-out (- n 1)))))

; normal data
(def read-body (s header)
  (let length (header "CONTENT-LENGTH")
    (w/ostring str
      (if length
        (echo-upto s str (readstring1 length))
        (let c (readb s)
          (while c (writeb c str) (= c (readb s))))))))

; url management
; url: (list protocol host page port)

(= url-protocol car)
(= url-host cadr)
(= url-page caddr)
(= url-port cadddr)

(def str->url (str)
  "if the protocol is missing the whole string is considered to be the page"
  (withs (pr-end (findsubseq "://" str)
          host-end (if pr-end (or (pos #\/ str (+ 3 pr-end))
                                  (len str)) 0)
          port (if (> host-end 0) 
                   (or (pos #\: (cut str 0 host-end) (+ 3 pr-end))
                       host-end) 0))
    (list (if pr-end (cut str 0 pr-end))
          (if (> host-end 0) (cut str (+ 3 pr-end) port))
          (if (< host-end (len str)) (cut str host-end (len str)) "/")
          (if (and (no (is port 0)) (no (is port host-end)))
            (read (instring (cut str (1+ port) host-end)))
            *http-port*))))
