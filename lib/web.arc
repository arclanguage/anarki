; written by Mark Huetsch and Brian J Rubinton
; same license as Arc

(require "lib/re.arc")

(= protocol* "HTTP/1.0"
   useragent* (+ "User-Agent: Mozilla/5.0 " 
                 "(Windows; U; Windows NT 5.1; uk; rv:1.9.1.2) "
                 "Gecko/20090729 "
                 "Firefox/3.5.2")
   content-type* "Content-Type: application/x-www-form-urlencoded")

; 1) prepare header lines and body
; 2) build request
; 3) perform-io (send request & receive response)
; 4) return header lines and body
(def mkreq (url (o querylist) (o method "GET") (o cookies))
  (withs (parsed-url (parse-url url)
          full-query (build-query parsed-url!query
                                  querylist)
          method     (upcase method)
          header     (req-header  parsed-url!path
                                  parsed-url!host
                                  full-query
                                  method
                                  cookies)
          body       (req-body full-query method)
          request    (+ header
                        (str-rn 2)
                        body)
          response   (perform-io  parsed-url!resource
                                  parsed-url!host
                                  parsed-url!port
                                  request))
    response))

(def parse-url (url)
  (withs ((resource url) (split-at "://" (ensure-resource (strip-after "#" url)))
          (hp pq)        (split-at "/" url)
          (host port)    (split-at ":" hp)
          (path query)   (split-at "?" pq))
    (obj resource resource
         host     host
         port     (select-port port resource)
         path     path
         query    query)))

(def ensure-resource (url)
  (if (posmatch "://" url)
    url
    (+ "http://" url)))

(def strip-after (delim s)
  (car (split-at delim s)))

; Split string s at first instance of delimeter.
; Return split list.
(def split-at (delim s)
  (iflet i (posmatch delim s)
    (list (cut s 0 i) (cut s (+ i (len delim))))
    (list s)))

(def select-port (portstr resource)
  (if (nonblank portstr)
    (int portstr)
    (default-port resource)))

(def default-port (resource)
  (if (is resource "https")
    443
    80))

(def build-query (querystr querylist)
  (+ querystr
     (and (nonblank querystr)
          querylist
          '&) 
     (to-query-str querylist)))

(def to-query-str (querylist)
  (if querylist
    (joinstr (map [joinstr _ "="] (pair (map [coerce _ 'string] querylist)))
             "&")))

(def req-header (path host query method cookies)
  (reduce +
    (intersperse (str-rn)
                 (flat:list 
                   (first-req-line method path query)
                   (request-header host)
                   (entity-header  method query) 
                   (cookie-header  cookies)))))

(def first-req-line (method path query)
  (+ method " " (build-uri path method query) " " protocol*))

(def build-uri (path method (o query ""))
  (+ "/" path (and (is method "GET")
                   (nonblank query)
                   (+ "?" query))))

(def request-header (host)
  (list (+ "Host: " host) useragent*))

(def entity-header (method query)
  (if (is method "POST")
    (list (+ "Content-Length: " (len (utf-8-bytes query)))
          content-type*)))

(def cookie-header (cookies)
  (if cookies (encode-cookies cookies)))

(def encode-cookies (cookielist)
  (let joined-list (map [joinstr _ #\=] (pair cookielist))
    (+ "Cookie: "
       (if (len> joined-list 1)
         (reduce [+ _1 "; " _2] joined-list)
         (car joined-list))
       ";")))

(def req-body (query method)
  (if (and (is method "POST") (nonblank query))
    (+ query (str-rn))))

(def str-rn ((o n 1))
  (if (<= n 1)
    (string #\return #\newline)
    (string #\return #\newline (str-rn (- n 1)))))

(mac w/io (io request func)
  (w/uniq (i o response)
    `(let (,i ,o) ,io
      (disp ,request ,o)
      (let ,response (,func ,i)
        (close ,i ,o)
        ,response))))

; TODO bad code smell -- resource host port passed in then immediately sent out
; write a macro that defines w/io for a given pair of io ports
; w/io can then be used within that closure without knowledge of the particular ports used,
; or the data needed to open such ports (i.e. resource host port)
(def perform-io (resource host port send-request)
  (w/io (get-io resource host port)
        send-request
        receive-response))

(def get-io (resource host port)
  (if (is resource "https")
    (ssl-connect host port)
    (socket-connect host port)))

(def receive-response ((o s (stdin)))
  (list (read-header s) (read-body s)))

; Read each line from port until a blank line is reached.
(def read-header ((o s (stdin)))
  (accum a
    (whiler line (readline s) blank
      (a line))))

; Read remaining lines from port.
(def read-body ((o s (stdin)))
  (tostring 
    (whilet line (readline s)
      (pr line))))

; Convenience functions.
; Note: these ignore the response header: (car (mkreq url))
(def get-url (url)
  (cdr (mkreq url)))

(def post-url (url args)
  (cdr (mkreq url args "POST")))

; TODO write functions to parse/tokenize header lines

(def google (q)
  (get-url (+ "www.google.com/search?q=" (urlencode q))))

