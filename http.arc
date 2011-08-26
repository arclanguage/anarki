;;; http.arc: dealing with the HTTP protocol

(deftem http-msg
  prot  nil     ; protocol  "HTTP/1.1"
  hds   nil)    ; headers   (("Content-Type" "html") ("Location" "/new"))

; A "request" is a message from the client to the server

(deftem (http-req http-msg)
  meth    nil    ; method [downcased sym]    get, post
  path    nil    ; path                      "/some/thing"
  qs      nil    ; query string              "foo=bar&baz=42"
  args    nil    ; args of the qs/form post  (("foo" "bar") ("baz" "42"))
  cooks   nil)   ; sent cookies              (("sessid" "MTgY4h2"))

; A "response" is a message from the server to the client

(deftem (http-resp http-msg)
  sta     nil    ; status code   404
  rea     nil)   ; reason        "Not Found"

(= http-ok+         "200 OK"
   http-created+    "201 Created"
   http-found+      "302 Found"
   http-notmod+     "304 Not Modified"
   http-bad+        "400 Bad Request"
   http-forbidden+  "403 Forbidden"
   http-notfound+   "404 Not Found")


(def read-headers ((o from (stdin)))
  (unless (is (peekc from) #\newline)  ; for suckers using \n instead of \r\n
    (let line (readline from)
      (awhen (pos #\: line)
        (cons (list (normalize-hdname:cut line 0 it)
                    (trim:cut line (+ it 1)))
              (read-headers from))))))

(def normalize-hdname (name)  ; "content-type" -> "Content-Type"
  (string:intersperse #\- (map capitalize (tokens name #\-))))

(def read-req ((o from (stdin)))
  (withs ((m pa pro) (read-reqline from)
          (rpa qs)   (tokens pa #\?)
          hds        (read-headers from))
    (inst 'http-req  'prot pro  'meth (sym:downcase m)
                     'path rpa  'qs qs   'hds hds
                     'cooks (parse-cooks hds)
                     'args (only.parse-args qs))))

(def read-reqline ((o from (stdin)))  (tokens:readline from))

(def parse-args (argstr)  ; "foo=bar&baz=42" -> (("foo" "bar") ("baz" "42"))
  (map [map urldecode (tokens _ #\=)] (tokens argstr #\&)))

(def parse-cooks (reqhds)
  (reduce join
    (map [map [tokens (trim _) #\=] (tokens _.1 #\;)]
         (keep [caris _ "Cookie"] reqhds))))

(def read-resp ((o from (stdin)))
  (let (pro st . reas) (tokens (readline from))
    (inst 'http-resp 'prot pro  'sta (int st)
                     'rea (string:intersperse " " reas)
                     'hds (read-headers from))))

(def pr-headers (hds)
  (each (n v) hds  (prrn n ": " v))
  (prrn))

(def prrn args  ; print with \r\n at the end
  (map1 disp args)
  (prn #\return))

; we call "head" the top part of an HTTP message,
; i.e: the status or request line plus the headers

(def reqhead (meth path hds)
  (prrn upcase.meth " " path " HTTP/1.0")
  ; 1.0 because a 1.1 client should be able to deal with
  ; "Transfert-Encoding: chunked" (and we don't, at least yet)
  (pr-headers hds))

(def resphead ((o sta http-ok+) (o hds httpd-hds*))
  (prrn "HTTP/1.1 " sta)
  (pr-headers hds))

(def redirect (loc (o sta http-found+) (o hds httpd-hds*))
  (resphead sta (copy hds 'Location loc)))


;; httpd: generic HTTP server.
; put it behind a reverse proxy, and code your own "framework".
; doesn't deal with logging, gzipping, slow and bad clients,
; keep-alive, limits of req/<time>: nginx can do it for us

(= httpd-hds*    (obj Server        "http.arc"
                      Content-Type  "text/html"  ; set encoding in your HTML
                      Connection    "closed")
   stop-httpd*    nil
   httpd-handler  nil)  ; ** the function your web app has to define **

(def httpd-serve ((o port 8080))
  (w/socket s port
    (until stop-httpd*
      (let (in out ip) (socket-accept s)
        (thread:handle-req in out ip))))
  (prn "httpd done"))

(def handle-req (in out ip)
  (after
    (let req (read-req in)
      (= req!ip ip)  ; TODO: check and use X-Real-IP
      (read-body req in)
      (w/stdout out (httpd-handler req)))
    (close in out)))

(def read-body (req (o from (stdin)))
  (awhen (aand (alref req!hds "Content-Length") (errsafe:int it))
    (= req!body (readbytes it from))
    (when (findsubseq "x-www-form-urlencoded" (alref req!hds "Content-Type"))
      (= req!args (join req!args (parse-args:string (map [coerce _ 'char] req!body)))))))

(def start-httpd ((o port 8080))
  (wipe stop-httpd*)
  (prn "httpd: serving on port " port)
  (thread:httpd-serve port))


;; Very basic HTTP client.  still a work in progress: incomplete/ugly
;
; /!\ To have the code below working, you need to patch Arc to get
; client sockets.  here the function is called 'client-socket

(def parse-url (url)
  (with (prot "http"  host nil  port 80  path "/")
    (awhen (findsubseq "://" url)  ; explicit protocol?
      (= prot (downcase:cut url 0 it)
         url (cut url (+ it 3))))
    (aif (pos #\/ url)  ; deal with host & path
      (= host (cut url 0 it)
         path (cut url it))
      (= host url))
    (awhen (pos #\: host)  ; explicit port?
      (= port (int (cut host inc.it))
         host (cut host 0 it)))
    (list prot host port path)))

(def mk-http-req (method host path (o hds) (o port 80) (o body))
  (let (in out) (socket-connect host port)
    (w/stdout out
      (reqhead (upcase method) path hds)
      (prt body)
      (flushout))
    (after (list (read-resp in) in)
           (close out))))

(def http-get (url)  ; consume the headers and return the output stream
  (let (prot host port path) (parse-url url)
    (cadr (mk-http-req 'GET host path (obj Host host
                                           Connection "close") port))))


; hard drives crash, files get lost, cool URLs don't die

(let _infile infile
  (def infile (url)
    (if (begins downcase.url "http://")
      (http-get url)
      (_infile url)))
)

; arc> (filechars "http://www.faqs.org/rfcs/rfc2616.html")
; arc> (load "http://hacks.catdancer.ws/json.arc")


;; todo:
; * http-ok+ & co: remove the "+"? "*"?
;   not sure about "httpd" too.  at least rename 'httpd-serve to 'serve-http?
;
; * handle file uploads
;
; * deal with user@pwd in 'parse-url
;
; * actually wrong to use a table for httpd-hds*: it's legal to use the
; same header twice.  normally should be not break to change to an assoc
; list ('pr-headers would still work).  should make it.
;
; * maybe make it event-based or rewrite Arc to have a sane, really
; lightweight threading facility à la Erlang
