(load "http-get/http-utils.arc")

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
	  link (head "LOCATION" head))
    (if link
	(get-request (str->url link))
	(err "Cannot follow without location"))))

(def-on-code anerror (s r) ("400" "404" "500")
  (read-resource s r) ; skip header and message body
  (err r))

(def read-response (stream)
  "read response and call function to handle it"
  (withs (resp (readline stream) ; read response
	  code (if resp (cadr (tokens resp)))) ; get code
    (when (no resp)
      (err "Cannot read response"))
    (if code
	(dispatch-on-code code stream (trim resp 'both [find _ *ret*]))
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
  "cretes a function that execs a particular request on a url"
  `(def ,name (u)
    (exec-request (mk-request ,type u ,@headers) (url-host u) (url-port u))))

(defreq get-request "GET" '("Connection" "close"))
(defreq head-request "HEAD" '("Connection" "close"))
(defreq options-request "OPTIONS" '("Connection" "close"))

; save page on disk

(def save-page (u out-name (o overwrite))
  "save a page, if file already exists and overwrite is nil, then the page is 
   not downloaded"
  (if (or overwrite (no (file-exists out-name)))
    (w/outfile o out-name
      (disp (cadr (get-request (str->url u))) o))))
