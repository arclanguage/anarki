;wrapper for loading json
($ (require json))
($.json-null nil)
(= read-json $.read-json)
(= write-json $.write-json)

(def json-string (j)
  (tostring (write-json j)))

; read JSON from file, return hash
(def read-json-file (file)
  (w/infile inf file 
    (read-json inf)))

; write object to JSON file
(def write-json-file (file j)
  (w/outfile outf file 
    (w/stdout outf (write-json j))))

; This code is in the public domain.

(def hexdigit (c)
  (or (<= #\a c #\f) (<= #\A c #\F) (<= #\0 c #\9)))

(def json-unicode-digits (j)
  (let u (firstn 4 j)
    (unless (and (is (len u) 4) (all hexdigit u)) (err "need 4 hexadecimal digits after \\u"))
    (coerce (coerce (coerce u 'string) 'int 16) 'char)))

(def json-backslash-char (c)
  (case c
    #\" #\"
    #\\ #\\
    #\/ #\/
    #\b #\backspace
    #\f #\page
    #\n #\newline
    #\r #\return
    #\t #\tab
    (err "not able to handle " c " after backslash")))

(def decode-json-backslash (j a)
    (if (no j) (err "missing char after backslash"))
    (if (is (car j) #\u)
         (list (nthcdr 5 j) (json-unicode-digits (cdr j)))
         (list (cdr j) (json-backslash-char (car j)))))

(def decode-json-string (j a)
  (if (no j) (err "tailing \" not found in string"))
  (case (car j)
    #\\ (let (j2 c) (decode-json-backslash (cdr j) a)
          (decode-json-string j2 (cons c a)))
    #\" (list (cdr j) a)
        (decode-json-string (cdr j) (cons (car j) a))))

(def parse-json-string (j)
  (if (is (car j) #\")
    (let (j2 a) (decode-json-string (cdr j) nil)
      (list j2 (coerce (rev a) 'string)))))

(def decode-json-array (j a)
  (if (no j)
       (err "unexpected end of input")
      (is (car j) #\])
       (list (cdr j) a)
       (withs ((j2 v) (decode-json-value j)
               x (car j2))
         (if (is x #\]) (list (cdr j2) (+ a (list v)))
             (is x #\,) (decode-json-array (cdr j2) (+ a (list v)))
             (err "expected , or ] for array")))))

(def parse-json-array (j)
  (if (is (car j) #\[) (decode-json-array (cdr j) nil)))

(def decode-json-object (j o)
  (if (no j)
       (err "unexpected end of input")
      (is (car j) #\})
       (list (cdr j) o)
       (let (j2 k) (decode-json-value j)
         (if (no j2)
              (err "unexpected end of input")
             (isnt (car j2) #\:)
              (err "unexpected char" (car j2))
              (let (j3 v) (decode-json-value (cdr j2))
                (if (no j3)
                     (err "unexpected end of input")
                    (is (car j3) #\})
                     (list (cdr j3) (do (= (o k) v) o))
                    (is (car j3) #\,)
                     (decode-json-object (cdr j3) (do (= (o k) v) o))
                     (err "expected comma or close } for object")))))))
             
(def parse-json-object (j)
  (if (is (car j) #\{) (decode-json-object (cdr j) (table))))

(def json-number-character? (c)
  (find c ".-+eE1234567890"))

(def jspan (tst lst)
  ((afn (a lst)
     (if (and lst (tst (car lst)))
          (self (cons (car lst) a) (cdr lst))
          (list (rev a) lst)))
   nil lst))

; returns unparsed string of json number
(def parse-json-number (j)
  (let (a b) (jspan json-number-character? j)
    (if a (list b (coerce a 'string)))))

(def parse-json-sym (jsym out j)
  (if (begins j (coerce jsym 'cons))
      (list (nthcdr (len jsym) j) out)))

(def alt (j alts)
  (if (no alts) (err (string "unable to parse: " (coerce (firstn 20 j) 'string))))
  (let r ((car alts) j)
    (if r r (alt j (cdr alts)))))

(def decode-json-value (s)
  (alt s (list
    parse-json-string
    parse-json-number
    parse-json-object
    parse-json-array
    [parse-json-sym "false" nil _]
    [parse-json-sym "true"  t   _]
    [parse-json-sym "null"  nil _])))

(def fromjson (s)
  (let (j v) (decode-json-value (coerce s 'cons))
    (if j (err "unexpected input after value"))
    v))

(def 4hex (i)
  (let s (coerce i 'string 16)
    (if (< (len s) 4)
         (string (coerce (n-of (- 4 (len s)) #\0) 'string) s)
         s)))

(def encode-json-string (str)
  (tostring
    (each c str
      (pr
        (let i (coerce c 'int)
          (if (<  i 32)
               (string "\\u" (4hex i))
               (case c #\" "\\\""
                       #\\ "\\\\"
                       c)))))))

(def string>json (v)
  (string "\"" (encode-json-string v) "\""))

(def integer>json (v) (string v))

(def object>json (o)
  (string
   "{"
   (apply
    string
    (intersperse
    ","
    (accum a
      (maptable (fn (k v)
                  (a (string (string>json (string k)) ":" (tojson v))))
                o))))
   "}"))


(def array>json (v)
  (string "[" (apply string (intersperse "," (map tojson v))) "]"))

(def tojson (v)
  (if (isa v 'string)   (string>json v)
      (or (is v nil)
          (isa v 'cons))
                        (array>json v)
      (is v 'true)      "true"
      (is v 'false)     "false"
      (is v 'null)      "null"
      (isa v 'sym)      (string>json (coerce v 'string))
      (isa v 'table)    (object>json v)
      (isa v 'tem)      (tojson rep.v.1)
      (isa v 'int)      (integer>json v)
      (err "can't convert" v)))
