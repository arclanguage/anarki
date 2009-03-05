(require "lib/http-get/http-utils.arc")

; response handling 

(def-on-code http-continue (s r) ("100")
  (skip-header s)
  (read-response s))

(def-on-code read-resource (s r) ("200")
  (withs (head (read-header s)
          filter (if (iso (head "CONTENT-TYPE") "text/html")
                     (fn (x) (rem #\Return x))
                     idfn))
    (if (iso (head "TRANSFER-ENCODING") "chunked")
      (withs (r (read-all-chunks s)
              body (car r)
              footer (cadr r))
        (list head (filter body) footer))
      (list head (filter (read-body s head))))))
    
(def-on-code follow-link (s r) ("301" "302" "303" "307")
  "follows the link in the header Location:..."
  (withs (head (car (read-resource s r))
          link (head "LOCATION"))
    (if link
        (get-request (str->url link))
        (err "Cannot follow without location"))))

(def-on-code anerror (s r) ("400" "404" "500")
  (read-resource s r) ; skip header and message body
  (err r))

(def read-response (stream)
  "read response and call function to handle it"
  (withs (resp (trim (readline stream) 'both [pos _ *ret*]) ; read response
          code (if resp (cadr (tokens resp)))) ; get code
    (when (no resp)
      (err "Cannot read response"))
    (if code
        (dispatch-on-code code stream resp)
        (err (string "Malformed response: " resp)))))

; requests sending

(def mk-request (type u . header)
  "builds an http request"
  (w/ostring s
    (disp (string (upcase type) " " (url-page u) " HTTP/1.1" *ter* 
                   "Host: " (url-host u) ":" (url-port u) *ter*) s)
    (disp (apply mk-header header) s)))

(def exec-request (request host (o port *http-port*))
  "exec a request to host and manage the answer"
  (let in-out (connect-socket host port)
    (protect (fn ()
               (disp request (cadr in-out))
               (flush-socket (cadr in-out))
               (read-response (car in-out)))
             (fn ()
               (map close in-out)))))

(mac defreq (name type . headers)
  "creates a function that execs a particular request on a url"
  `(def ,name (u)
    (exec-request (mk-request ,type u ,@headers) (url-host u) (url-port u))))

(defreq get-request "GET" '("Connection" "close"))
(defreq head-request "HEAD" '("Connection" "close"))
(defreq options-request "OPTIONS" '("Connection" "close"))

; save page on disk

(def save-page (u out-name (o overwrite t))
  "save a page, if file already exists and overwrite is nil, then the page is 
   not downloaded"
  (if (or overwrite (no (file-exists out-name)))
    (w/outfile o out-name
      (disp (cadr (get-request (str->url u))) o))))

(def exec-multi-request (reqs host (o port *http-port*))
  "send requests and read answers with a unique connection"
  (let (s-in s-out) (connect-socket host port)
    (protect (fn ()
               (each r reqs (disp r s-out)) ; send requests
               (flush-socket s-out)
               (map [read-response s-in] reqs))
             (fn ()
               (close s-in)
               (close s-out)))))

(def maplist (f l)
  (if l (cons (f l) (maplist f (cdr l)))))

(mac defmultireq (name type)
  "creates a function that execs multiple requests on a url"
  `(def ,name (urls)
    (if urls
      (with (host (url-host (car urls)) port (url-port (car urls)))
        (exec-multi-request
          (maplist [if (~iso (url-host (car _)) host) 
                         (err "Multiple requests on different hosts")
                       (cdr _)
                         (mk-request ,type (car _)) ; not last
                       (mk-request ,type (car _) '("Connection" "close"))]
                   urls)
          host port)))))

(defmultireq multi-get-request "GET")

(mac http-req (type u . headers)
  "performs an http request"
  `(exec-request (mk-request ,type ,u ,@headers) (url-host ,u) (url-port ,u)))

(def url-data (pairs)
  " key-value pairs for use in post or cookie data"
  (reduce (fn (a b) (string a "&" b))
    (map (fn (l) (string (car l) "=" (cadr l))) pairs)))

(def http-post (u vars . headers)
  "performs a post request with vars to url u"
  (= data (url-data vars) length (len data))
  (= content-type
    '("Content-Type" "application/x-www-form-urlencoded"))
  (= content
      (list "Content-Length" (string length *ter* *ter* data)))
  (if (~empty headers)
    (http-req "POST" u headers content-type content)
    (http-req "POST" u content-type content)))
