; written by Mark Huetsch
; same license as Arc

($ (require openssl))
($ (xdef ssl-connect (lambda (host port)
                       (ar-init-socket
                         (lambda () (ssl-connect host port))))))

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

(def args->query-string (args)
  (if args
    (let equals-list (map [joinstr _ "="] (pair (map [coerce _ 'string] args)))
      (joinstr equals-list "&"))
    ""))

(def parse-url (url)
  (withs ((resource url)    (split-by "://" (ensure-resource:strip-after url "#"))
          (host+port path+query)  (split-by "/" url)
          (host portstr)    (split-by ":" host+port)
          (path query)      (split-by "?" path+query))
    (obj resource resource
         host     host
         port     (or (only.int portstr) default-port.resource)
         filename path
         query    query)))

; TODO: only handles https for now
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

; TODO this isn't very pretty
(def get-or-post-url (url (o args) (o method "GET") (o cookie))
  (withs (method            (upcase method)
          parsed-url        (parse-url url)
          full-args         (let query parsed-url!query
                              (+ "" query (and query args '&) args->query-string.args))
          request-path      (+ "/" (parsed-url 'filename)
                               (if (and (is method "GET") (> (len full-args) 0))
                                   (+ "?" full-args)))
          header-components (list (+ method " " request-path " HTTP/1.0")
                                  (+ "Host: " (parsed-url 'host))
                                  "User-Agent: Mozilla/5.0 (Windows; U; Windows NT 5.1; uk; rv:1.9.1.2) Gecko/20090729 Firefox/3.5.2"))
    (when (is method "POST")
      (pushend (+ "Content-Length: "
                  (len (utf-8-bytes full-args)))
               header-components)
      (pushend "Content-Type: application/x-www-form-urlencoded"
               header-components))
    (when cookie
      (push (encode-cookie cookie) header-components))
    (withs (header          (reduce [+ _1 "\r\n" _2] header-components)
            body            (if (is method "POST") (+ full-args "\r\n"))
            request-message (+ header "\r\n\r\n" body))
      (let (in out) (if (is "https" (parsed-url 'resource))
                      (ssl-connect (parsed-url 'host) (parsed-url 'port))
                      (socket-connect (parsed-url 'host) (parsed-url 'port)))
        (disp request-message out)
        (with (header (parse-server-headers (read-headers in))
               body   (tostring (whilet line (readline in) (prn line))))
          (close in out)
          (list header body))))))

(def get-url (url)
  ((get-or-post-url url) 1))

(def post-url (url args)
  ((get-or-post-url url args "POST") 1))



(def split-by(delim s)
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
                    (fn (url) (let (parsed-header html) (get-or-post-url url '() "GET" cookies*)
                                (= cookies* (fill-table cookies* (flat (parsed-header 1))))
                                html))
                    post-url
                    (fn (url args) (let (parsed-header html) (get-or-post-url url args "POST" cookies*)
                                     (= cookies* (fill-table cookies* (flat (parsed-header 1))))
                                     html)))
     (do ,@body)))
