; written by Mark Huetsch and Brian J Rubinton
; same license as Arc
;
; Primary interface:
;   1)  mkreq - Build request, send request, receive response as (list header body).
;   2) defreq - Create named http request function wrappers for mkreq.
;             - Intended for multi-use http requests.
;             - Additional url parameters can be passed into wrapper for each request.
;             - example: see (defreq google ...)

(require "lib/re.arc")

(= protocol* "HTTP/1.0"
   useragent* "Web.arc/1.0"
   content-type* "Content-Type: application/x-www-form-urlencoded")

; TODO would it be more idiomatic to define (mac defreq ...)?
; user would create a request function
; so, ((defreq google ___)), then (google ___) would work
(mac defreq (name url querylist method cookies)
  `(def ,name (o param)
    (mkreq ,url (list ,querylist ,param) method cookies)))

(mac w/io (io request func)
  (w/uniq (i o response)
    `(let (,i ,o) ,io
      (disp ,request ,o)
      (let ,response (,func ,i)
        (close ,i ,o)
        ,response))))

(def mkreq (url (o querylist) (o method "GET") (o cookies))
  (let url (parse-url url)
    (w/io (get-io   url!resource url!host url!port)
          (buildreq url!host url!port url!path
                    (build-query url!query
                                 querylist)
                    (upcase method)
                    cookies)
          receive-response)))

; TODO refactor to return list of 5 items in this order? is that easier to read/use?
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
  (if (posmatch "://" url) url (+ "http://" url)))

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
  (if (is resource "https") 443 80))

(def build-query (querystr querylist)
  (string querystr
     (and (nonblank querystr)
          querylist
          '&) 
     (to-query-str querylist)))

; TODO should this urlencode all the items in querylist?
(def to-query-str (querylist)
  (if querylist
    (joinstr (map [joinstr _ "="] (pair (map [urlencode:coerce _ 'string] querylist)))
             "&")))

; TODO rename req-header or request-header
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

; TODO should this be memoized via defmemo?
(def buildreq (host port path query method cookies)
  (+ (req-header path host query method cookies)
     (str-rn 2)
     (req-body query method)))

(def str-rn ((o n 1))
  (if (<= n 1)
    (string #\return #\newline)
    (string #\return #\newline (str-rn (- n 1)))))

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

