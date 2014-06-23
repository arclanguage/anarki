; written by Mark Huetsch and Brian J Rubinton
; same license as Arc
;
; Primary interface:
;   1)  mkreq - Build request, send request, receive response as (list header body).
;   2) defreq - Create named function wrappers for mkreq.
;             - Intended for multi-use http requests.
;             - Url query parameters can be passed into wrapper for each request.
;             - example: see (defreq google '(q "bagels new york city"))
;   3)  mkuri - Construct a uri from url and query list.
;             - Only compatible with GET requests.

(= protocol* "HTTP/1.0"
   useragent* "Web.arc/1.0"
   content-type* "Content-Type: application/x-www-form-urlencoded")

(mac w/io (io request func)
  (w/uniq (i o response)
    `(let (,i ,o) ,io
      (disp ,request ,o)
      (let ,response (,func ,i)
        (close ,i ,o)
        ,response))))

(def mkreq (url (o querylist) (o method "GET") (o cookies) (o headers))
  (let url (parse-url url)
    (w/io (get-io   url!resource url!host url!port)
          (buildreq url!host
                    url!path
                    (build-query url!query querylist)
                    (upcase method)
                    cookies
                    headers)
          receive-response)))

(mac defreq (name url (o querylist) (o method "GET") (o cookies))
  `(def ,name ((o param))
    (mkreq ,url (join ,querylist param) ,method ,cookies)))

(def mkuri (url (o querylist))
  (let url (parse-url url)
    (+ url!resource "://"
       url!host ":"
       url!port
       (build-uri url!path "GET" (build-query url!query querylist)))))

(def parse-url (url)
  (withs ((resource url) (split-at (ensure-resource (strip-after url "#")) "://")
          (hp pq)        (split-at url "/")
          (host port)    (split-at hp ":")
          (path query)   (split-at pq "?"))
    (obj resource resource
         host     host
         port     (select-port port resource)
         path     path
         query    query)))

(def ensure-resource (url)
  (if (posmatch "://" url) url (+ "http://" url)))

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

(def to-query-str (querylist)
  (if querylist
    (joinstr (map [joinstr _ "="]
                  (map (fn ((k v))
                         (list k urlencode.v))
                       (pair:map [coerce _ 'string] querylist)))
             "&")))

(def build-header (host path query method cookies headers)
  (reduce +
    (intersperse (str-rn)
                 (flat:list
                   (first-req-line method path query)
                   (request-header host)
                   (entity-header  method query)
                   (cookie-header  cookies)
                   headers))))

(def first-req-line (method path query)
  (+ method " " (build-uri path method query) " " protocol*))

(def build-uri (path method (o query ""))
  (+ "/" path (and (is method "GET")
                   (nonblank query)
                   (+ "?" query))))

(def request-header (host)
  (list (+ "Host: " host)
        (+ "User-Agent: " useragent*)))

(def entity-header (method query)
  (if (is method "POST")
    (list (+ "Content-Length: " (len (utf-8-bytes query))) content-type*)))

(def cookie-header (cookies)
  (if cookies (encode-cookies cookies)))

(def encode-cookies (cookielist)
  (let joined-list (map [joinstr _ #\=] (pair cookielist))
    (+ "Cookie: "
       (if (len> joined-list 1)
         (reduce [+ _1 "; " _2] joined-list)
         (car joined-list))
       ";")))

(def build-body (query method)
  (if (and (is method "POST") (nonblank query))
    (+ query (str-rn))
    nil))

(def buildreq (host path query method cookies headers)
  (+ (build-header host path query method cookies headers)
     (str-rn 2)
     (build-body query method)))

(def str-rn ((o n 1))
  (if (<= n 1)
    (string #\return #\newline)
    (string (str-rn) (str-rn (- n 1)))))

(def get-io (resource host port)
  (if (is resource "https")
    (ssl-connect host port)
    (socket-connect host port)))

(def receive-response ((o s (stdin)))
  (list (slurp-header s) (slurp-body s)))

(def slurp-header ((o s (stdin)))
  " Read each line from port until a blank line is reached. "
  (accum a
    (whiler line (readline s) blank
      (a line))))

(def slurp-body ((o s (stdin)))
  " Read remaining lines from port. "
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
; TODO refactor google func to use defreq macro
(def google (q)
  (get-url (+ "www.google.com/search?q=" (urlencode q))))

