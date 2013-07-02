; written by Mark Huetsch
; same license as Arc

($ (require openssl))
($ (xdef ssl-connect (lambda (host port)
                       (ar-init-socket
                         (lambda () (ssl-connect host port))))))
; todo move this to ac.scm

(require "lib/re.arc")

(def parse-server-cookies (s)
  (map [map trim _]
       (map [matchsplit "=" _]
            (tokens s #\;))))

(def read-headers ((o s (stdin)))
  (accum a
    (whiler line (readline s) blank
      (a line))))

(def parse-server-headers (lines)
  (list (firstn 3 (only.tokens car.lines))
        (some [aand (begins-rest "Set-Cookie:" _) parse-server-cookies.it]
              cdr.lines)))

(def to-query-str (arglist)
  (if arglist
    (joinstr (map [joinstr _ "="] (pair (map [coerce _ 'string] arglist)))
             "&")))

(def build-query (url-argstr arglist)
  (+ "" url-argstr (and url-argstr arglist '&) (to-query-str arglist)))

(def parse-url (url)
  (withs ((resource url)    (split-by "://" (ensure-resource (strip-after url "#")))
          (host+port path+query)  (split-by "/" url)
          (host port)    (split-by ":" host+port)
          (path query)      (split-by "?" path+query))
    (obj resource resource
         host     host
         port     (select-port port resource)
         filename path
         query    query)))

(def select-port (portstr resource)
  (if (nonblank portstr)
    (only (int portstr)) ; todo learn why "only" is necessary
    (default-port resource)))

; support additional URI schemes?
(def default-port(resource)
  (if (is resource "https")
    443
    80))

(def encode-cookie (o)
  (let joined-list (map [joinstr _ #\=] (tablist o))
    (+ "Cookie: "
       (if (len> joined-list 1)
         (reduce [+ _1 "; " _2] joined-list)
         (car joined-list))
       ";")))

(= protocol* "HTTP/1.0"
   useragent* (+ "User-Agent: Mozilla/5.0 " 
                 "(Windows; U; Windows NT 5.1; uk; rv:1.9.1.2) "
                 "Gecko/20090729 "
                 "Firefox/3.5.2")
   content-type* "Content-Type: application/x-www-form-urlencoded")

(def cookie-header (cookie)
  (if cookie
    (encode-cookie cookie)))

(def entity-header (method query)
  (if (is method "POST")
    (list (+ "Content-Length: " (len (utf-8-bytes query)))
          content-type*)))

(def request-header (host)
  (list (+ "Host: " host) useragent*))

(def first-req-line (method filename query)
  (+ method " " (build-uri filename method query) " " protocol*))

(def req-header (filename host query method cookie)
  (reduce +
    (intersperse "\r\n" ; should i use #\return #\newline ?
                 (flat 
                   (list 
                     (first-req-line method filename query)) 
                     (request-header host)
                     (entity-header method query) 
                     (cookie-header cookie)))))

(def req-body (query method)
  (if (and (is method "POST") (nonblank query))
    (+ query "\r\n")))

(def build-uri (filename method (o query ""))
  (+ "/" filename (and ; evals to last expr if all t
                    (is method "GET")
                    (nonblank query)
                    (+ "?" query))))

(def get-i-o (resource host port)
  (if (is "https" resource)
    (ssl-connect host port)
    (socket-connect host port)))

(def sendreq (resource host port req)
  (let (in out) (get-i-o resource
                       host
                       port)
    (disp req out)
    (with (header (parse-server-headers (read-headers in))
           body   (tostring (whilet line (readline in) (prn line))))
      (close in out)
      (list header body))))

; todo refactor. http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html
; todo add json
(def mkreq (url (o arglist) (o method "GET") (o cookie))
  (withs (parsed-url (parse-url url)
          full-query (build-query parsed-url!query
                                  arglist)
          method     (upcase method)
          header     (req-header parsed-url!filename
                                 parsed-url!host
                                 full-query
                                 method
                                 cookie)
          body       (req-body full-query method)
          request    (+ header
                        "\r\n\r\n"
                        body)
          response   (sendreq parsed-url!resource
                              parsed-url!host
                              parsed-url!port
                              request))
    (list (car response) (cdr response))))

(def get-url (url)
  ((mkreq url) 1))

(def post-url (url args)
  ((mkreq url args "POST") 1))

(def split-by(delim s) ;isn't this available elsewhere?
  (iflet idx (posmatch delim s)
    (list (cut s 0 idx) (cut s (+ idx len.delim)))
    (list s nil)))

(def strip-after(s delim)
  ((split-by delim s) 0))

(def ensure-resource(url)
  (if (posmatch "://" url)
    url
    (+ "http://" url)))

(def google (q)
  (get-url (+ "www.google.com/search?q=" (urlencode q))))

; just some preliminary hacking
(mac w/browser body
  `(withs (cookies* (table)
                    get-url
                    (fn (url) (let (parsed-header html) (mkreq url '() "GET" cookies*)
                                (= cookies* (fill-table cookies* (flat (parsed-header 1))))
                                html))
                    post-url
                    (fn (url args) (let (parsed-header html) (mkreq url args "POST" cookies*)
                                     (= cookies* (fill-table cookies* (flat (parsed-header 1))))
                                     html)))
     (do ,@body)))
