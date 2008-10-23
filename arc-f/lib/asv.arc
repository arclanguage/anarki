; HTTP Server.

; could make form fields that know their value type because of
; gensymed names, and so the receiving fn gets args that are not
; strings but parsed values.

(in-package asv)
(using <arc>v3)
(using <arc>v3-thread)
(using <arc>v3-sync)
(using <arc>v3-packages)
(using <files>v1)
(using <strings>v1)

; pre-release interface: in flux, so new symbols
; will be added here all the time ^^
(interface v1-pre
  asv stop-asv allow-asv-break
  defop defop*
  defopr defopr*
  defoph defoph*
  ; defop special functions
  REDIRECT HEADER RESPOND CONTENT-TYPE
  ; utility
  url-to url-to-me)

; TODO: determine if we can write to the installation
; directory and use that, otherwise base on ~/asv/, else
; throw an error or something.
(= arcdir*  (file-join (arc-installation-path) "asv"))
(= logdir*  (file-join arcdir* "logs")
   rootdir* (file-join arcdir* "public_html")
   breaksrv* nil) 
 
; add the following files to the rootdir to use error pages
(= errorpages* (listtab '((404 "404.html") (500 "500.html"))))

(= serverheader* "Server: asv/v1-pre")

(= abortsema* (sema))
(wipe serverthread*)

(def asv ((o port 8080))
  (stop-asv)
  ; TODO: load user information from asv/app.arc
  (= serverthread* (thread (serve port)))
  t)

(def stop-asv ()
  (when (and serverthread* (~dead serverthread*))
    (sema-post abortsema*)
    (sync serverthread*)
    (wipe serverthread*)))

(def allow-asv-break ((o allow t))
  (= breaksrv* allow))

(def serve (port)
  (ensure-srvdirs)
  (w/socket s port
    (prn "ready to serve port " port)
    (= currsock* s)
    ; server loop
    (breakable:while t
      (if (is (sync abortsema* s) abortsema*)
          (break t)
          (if breaksrv* 
              (handle-request s)
              (errsafe (handle-request s))))))
  (prn "quit server"))

(def ensure-srvdirs ()
  (ensure-dir arcdir*)
  (ensure-dir logdir*))

(= srv-noisy* nil)

; http requests currently capped at 2 meg by socket-accept

; should threads process requests one at a time? no, then
; a browser that's slow consuming the data could hang the
; whole server.

; wait for a connection from a browser and start a thread
; to handle it. also arrange to kill that thread if it
; has not completed in threadlife* seconds.

(= srvthreads* nil threadlimit* 50 threadlife* 30)

; Could auto-throttle ips, e.g. if one has more than x% of recent requests.

(= requests* 0 requests/ip* (table) throttle-ips* (table) throttle-time* 30)

(def handle-request (s (o life threadlife*))
  (if (len< (pull dead srvthreads*) threadlimit*)
      (let (i o ip) (socket-accept s)
        (++ requests*)
        (= (requests/ip* ip) (+ 1 (or (requests/ip* ip) 0)))
        (let th (thread 
                  (if (throttle-ips* ip) (sleep (rand throttle-time*)))
                  (handle-request-thread i o ip))
          (push th srvthreads*)
          (thread (sleep life)
                  (unless (dead th) (prn "srv thread took too long"))
                  (break-thread th)
                  (close i o))))
      (sleep .2)))

; Currently trapping errors and responding with 500 error and message. 
; Headers already committed though so you get both 200 and 500 headers.
(def handle-request-thread (i o ip)
  (with (nls 0 lines nil line nil responded nil)
    (after
      (whilet c (unless responded (readc i))
        (if srv-noisy* (pr c))
        (if (is c #\newline)
            (if (is (++ nls) 2) 
                (let (type op args n cooks) (parseheader (rev lines))
                  ; (srvlog 'srv ip type op cooks)
                  ; TODO: logging
                  (case type
                    "head" (respond o op args cooks ip type)
                    "get"  (respond o op args cooks ip type)
                    "post" (handle-post i o op n cooks ip)
                           (respond-err o 404 "Unknown request: " (car lines)))
                  (assert responded))
                (do (push (string (rev line)) lines)
                    (wipe line)))
            (unless (is c #\return)
              (push c line)
              (= nls 0))))
      (close i o)))
  (harvest-fnids))

; Could ignore return chars (which come from textarea fields) here by
; (unless (is c #\return) (push c line))

(def handle-post (i o op n cooks ip)
  (if srv-noisy* (pr "Post Contents: "))
  (if (no n)
      (respond-err o 500 "Post request without Content-Length.")
      (let line nil
        (whilet c (and (> n 0) (readc i))
          (if srv-noisy* (pr c))
          (-- n)
          (push c line)) 
        (if srv-noisy* (pr "\n\n"))
        (respond o op (parseargs (string (rev line))) cooks ip))))

(= srvops* (table) optimes* (table))

; TODO: complete all of these, at least for HTTP 1.0
(= statuscodes* (listtab '((200 "OK") (302 "Moved Temporarily") (404 "Not Found") (500 "Internal Server Error"))))

; TODO: consider instead using some sort of per-directory configuration file
(= ext-mimetypes* (listtab '(
  ("gif"  "image/gif")
  ("jpg"  "image/jpeg")
  ("png"  "image/png")
  ("ico"  "image/x-icon")
  ("css"  "text/css; charset=utf-8")
  ("js"   "text/javascript; charset=utf-8")
  ("json" "application/json; charset=utf-8")
  ("xml"  "application/xml; charset=utf-8")
  ("pdf"  "application/pdf")
  ("swf"  "application/x-shockwave-flash")
  )))

(= textmime* "text/html; charset=utf-8")

(def header ((o type textmime*) (o code 200))
  (string "HTTP/1.0 " code " " (statuscodes* code) "\r\n"
          serverheader* "\r\n"
          "Content-Type: " type "\r\n"
          "Connection: close"))

(def err-header (code)
  (header textmime* code))

(def save-optime (name elapsed)
  (unless (optimes* name) (= (optimes* name) (queue)))
  (enq-limit elapsed (optimes* name) 1000))

(mac defop-raw (name parms . body)
  (w/uniq t1
    `(= (srvops* ,(op-of-sym name) )
        (fn ,parms 
          (let ,t1 (msec)
            (do1 (do ,@body)
                 (symeval!save-optime ',name (- (msec) ,t1))))))))

(def op-of-sym (x)
  (err "expected symbol for web operation name - " x))
(defm op-of-sym ((t op sym))
  (withs (pk (pkg-of op)
          pn (if pk (pkg-name pk)))
    (if pn (string pn "/" (unpkg op))
           (string op))))

(mac defop-base (name parm . body)
  (w/uniq (gs gso end abort br)
    `(defop-raw ,name (,gs ,parm)
       (point ,abort
         (withs ((,gso ,end HEADER RESPOND)
                   (symeval!make-reflector ,gs)
                 CONTENT-TYPE
                   (fn (type)
                     (HEADER "Content-type" type))
                 ; base redirect
                 ,br
                   (fn (url)
                     (HEADER "Location" url)
                     (RESPOND 302)
                     (,abort nil))
                 REDIRECT
                   ; br:url-to
                   (symeval!compose ,br symeval!url-to))
           (after
             (w/stdout ,gso
               ,@body)
             (,end)))))))

(def make-reflector (str)
  " Creates a pipe and reflects data given through the pipe to
    the stream.  Before the first byte is sent, it transmits
    a constructed response and header.  It also provides a way
    of ending the transmission, a way of adding headers, and a
    way of giving a different response "
  (withs ((p-in p-out) (pipe)
          wait         (sema)
          headers      (table "Content-type" textmime*)
          response     200
          HEADER       (fn (key val)
                         (= (headers key) val))
          RESPOND      (fn (code) (= response code))
          END          (fn () (close p-out) (sema-wait wait)))
    (thread:after
      (let tmp (readb p-in)
        ; emit the header
        (w/stdout str
          (prn "HTTP/1.0 " response " " (statuscodes* response) "\r")
          (prn serverheader* "\r")
          (each (k . v) headers
            (prn k ": " v "\r"))
          (prn "Connection: close\r\n\r"))
          (while tmp
            ; should really be using something like sendfile
            (writeb tmp str)
            (= tmp (readb p-in))))
      (sema-post wait))
    (list p-out END HEADER RESPOND)))

(mac defop (name parm . body)
  `(defop-base ,name ,parm 
     ,@body))

(mac defop* (name parm . body)
  `(defop-base ,(unpkg name) ,parm
     ,@body))

(mac defsop (name parm auth . body)
  (w/uniq (test auth-var)
    `(withs
         ( ,auth-var ,auth
           ,test     (if (acons ,auth-var)
                         [some _ ,auth-var]
                         (testify ,auth-var)))
        (defop ,name ,parm
           (if (,test (,parm 'ip))
               (do ,@body)
               (symeval!pr "Permission denied"))))))


; Defines op as a redirector.  Its retval is new location.

(mac defopr (name parm . body)
  (w/uniq (gs rd abort)
    `(defop-raw ,name (,gs ,parm)
       (let ,rd (do ,@body)
         (w/stdout ,gs
           (symeval!prn "HTTP/1.0 302 Moved Temporarily\r")
           (symeval!prn symeval!serverheader* "\r")
           (symeval!prn "Location: " ,rd "\r")
           (symeval!prn "Connection: close\r\n\r"))))))

(mac defopr* (name parm . body)
  `(defopr ,(unpkg name) ,parm ,@body))

; Defines op as an html-only emitting operation

(mac defoph (name parm . body)
  (w/uniq (gs)
    `(defop-raw ,name (,gs ,parm)
       (w/stdout ,gs
         (symeval!prn (symeval!header))
         (symeval!prn "\r")
         ,@body))))

(mac defoph* (name parm . body)
  `(defoph ,(unpkg name) ,parm ,@body))

;(mac testop (name . args) `((srvops* ',name) ,@args))

(deftem request
  args   nil
  cooks  nil
  ip     nil
  opname nil)

(= unknown-msg* "Unknown operator.")

(def extension (file)
  (let tok (tokens file #\.)
    (if (is (len tok) 1)
        tok 
        (car (rev tok)))))

(def filemime (file)
  (aif (ext-mimetypes* (extension file))
       it
       textmime*))

(def file-exists-in-root (file)
  (and (~empty file)
       (prefix (qualified-path rootdir*)
               (qualified-path (file-join rootdir* file)))
       (file-exists (file-join rootdir* file))))

(def respond (str op args cooks ip (o type))
  (aif
    (srvops* op)
      (let req (inst 'request 'args args 'cooks cooks 'ip ip 'opname op)
        (it str req))
    (file-exists-in-root op)
      (if (is type "head")
          (do (disp (header (filemime it)) str)
              (disp "\r\n\r\n" str))
          (respond-file str it))
      (if (is type "head")
          (do (disp (err-header 404) str)
              (disp "\r\n\r\n" str))
          (respond-err str 404 unknown-msg*))))

(def respond-file (str file (o code 200))
  (do (disp (header (filemime file) code) str)
      (disp "\r\n\r\n")
      (w/infile i file
        (whilet b (readb i)
          (writeb b str)))))
        
(def respond-err (str code msg . args)
  (aif (file-exists-in-root (errorpages* code))
       (respond-file str it code)
       (w/stdout str
         (prn (err-header code)) 
         (prn "\r")
         (apply pr msg args))))
  
(def parseheader (lines)
  (let (type op args) (parseurl (car lines))
    (list type
          op
          args
          (and (is type 'post)
               (some (fn (s)
                       (and (begins (upcase s) "CONTENT-LENGTH:")
                            (coerce (cadr (tokens s)) 'int)))
                     (cdr lines)))
          (some (fn (s)
                  (and (begins (upcase s) "COOKIE:")
                       (parsecookies s)))
                (cdr lines)))))

; (parseurl "GET /p1?foo=bar&ug etc") -> (get p1 (("foo" "bar") ("ug")))

(def parseurl (s)
  (let (type url) (tokens s)
    (let (base args) (tokens url #\?)
      (list (downcase type)
            (cut base 1)
            (if args
                (parseargs args)
                nil)))))

; I don't urldecode field names or anything in cookies; correct?

(def parseargs (s)
  (map (fn ((k v)) (list k (urldecode v)))
       (map [tokens _ #\=] (tokens s #\&))))

(def parsecookies (s)
  (map [tokens _ #\=] 
       (cdr (tokens s [or (whitec _) (is _ #\;)]))))

(def arg (req key) (alref (req 'args) key))
(def cookie (req key) (alref (req 'args) key))
(def source-ip (req) (req 'ip))

(mac w/args (args req . body)
  (w/uniq greq
    `(withs (,greq ,req
             ,@(w/collect:each a args
                 (collect a)
                 (collect `(symeval!arg ,greq ,(string:unpkg a)))))
      ,@body)))

(def reassemble-args (req)
  (aif (req 'args)
       (apply string "?" (intersperse "&"
                                      (map (fn ((k v))
                                             (string k "=" (urlencode v)))
                                           it)))
       ""))

(def url-to (op . args)
  " Creates a string of the URL to the desired web operation.
    GET arguments may be specified with additional parameters:
      (url-to 'op 'param1 \"value1\"
                  'param2 \"value2\")
    See also [[url-to-me]] "
  (zap urlify op)
  (if args
      (tostring:let first t
        (pr op "?")
        (each (k v) (pair args)
          (if first (wipe first)
                    (pr "&"))
          (pr (urlencode:string k) "=" (urlencode:string v))))
      op))

(def urlify (op)
  (err "'url-to: expects either a string or symbol for target"))
(defm urlify ((t op string)) op)
(defm urlify ((t op sym)) (op-of-sym op))

(def url-to-me (req)
  " Creates a string of the URL to the web operation described
    in the request `req'.
    Most often used to get a link to the same operation.
    See also [[url-to]] "
  (string req!opname (reassemble-args req)))

; start asv
(asv)

;---------------------TODO!

(= fns* (table) fnids* nil timed-fnids* nil)

; count on huge (expt 64 10) size of fnid space to avoid clashes

(def new-fnid ()
  (check (sym (rand-string 10)) ~fns* (new-fnid)))

(def fnid (f)
  (atlet key (new-fnid)
    (= (fns* key) f)
    (push key fnids*)
    key))

(def timed-fnid (lasts f)
  (atlet key (new-fnid)
    (= (fns* key) f)
    (push (list key (seconds) lasts) timed-fnids*)
    key))

; Within f, it will be bound to the fn's own fnid.  Remember that this is
; so low-level that need to generate the newline to separate from the headers
; within the body of f.

(mac afnid (f)
  `(atlet it (new-fnid)
     (= (fns* it) ,f)
     (push it fnids*)
     it))

;(defop test-afnid req
;  (tag (a href (url-for (afnid (fn (req) (prn "\r") (pr "my fnid is " it)))))
;    (pr "click here")))

; To be more sophisticated, instead of killing fnids, could first 
; replace them with fns that tell the server it's harvesting too 
; aggressively if they start to get called.  But the right thing to 
; do is estimate what the max no of fnids can be and set the harvest 
; limit there-- beyond that the only solution is to buy more memory.

(def harvest-fnids ((o n 20000)) 
  (when (len> fns* n) 
    (pull (fn ((id created lasts))
            (when (> (since created) lasts)    
              (wipe (fns* id))
              t))
          timed-fnids*)
    (atlet nharvest (trunc (/ n 10))
      (let (kill keep) (or (errsafe:split (rev fnids*) nharvest)
                           (split (rev fnids*) (trunc:/ (len fnids*) 10)))
        (= fnids* (rev keep)) 
        (each id kill 
          (wipe (fns* id)))))))

(= fnurl* "x" rfnurl* "r" rfnurl2* "y" jfnurl* "a")

(= dead-msg* "\nUnknown or expired link.")
 
(defop x req
  (aif (fns* (sym (arg req "fnid")))
       (it req)
       (pr dead-msg*)))

#|
(defopr-raw y (str req)
  (aif (fns* (sym (arg req "fnid")))
       (w/stdout str (it req))
       "deadlink"))
|#

; For asynchronous calls; discards the page.  Would be better to tell
; the fn not to generate it.

(defop-raw a (str req)
  (aif (fns* (sym (arg req "fnid")))
       (tostring (it req))))

(defopr r req
  (aif (fns* (sym (arg req "fnid")))
       (it req)
       "deadlink"))

(defop* deadlink req
  (pr dead-msg*))

(def url-for (fnid)
  (string fnurl* "?fnid=" fnid))

(def flink (f)
  (string fnurl* "?fnid=" (fnid (fn (req) (prn "\r") (f req)))))

(def rflink (f)
  (string rfnurl* "?fnid=" (fnid f)))
  
; Since it's just an expr, gensym a parm for (ignored) args.

(mac w/link (expr . body)
  `(tag (a href (flink (fn (,(uniq)) ,expr)))
     ,@body))

(mac w/rlink (expr . body)
  `(tag (a href (rflink (fn (,(uniq)) ,expr)))
     ,@body))

(mac onlink (text . body)
  `(w/link (do ,@body) (pr ,text)))

; bad to have both flink and linkf; rename flink something like fnid-link

(mac linkf (text parms . body)
  `(tag (a href (flink (fn ,parms ,@body))) (pr ,text)))

(mac rlinkf (text parms . body)
  `(tag (a href (rflink (fn ,parms ,@body))) (pr ,text)))

;(defop top req (linkf 'whoami? (req) (pr "I am " (get-user req))))

;(defop testf req (w/link (pr "ha ha ha") (pr "laugh")))

(mac w/link-if (test expr . body)
  `(tag-if ,test (a href (flink (fn (,(uniq)) ,expr)))
     ,@body))

(def fnid-field (id)
  (gentag input type 'hidden name 'fnid value id))

; f should be a fn of one arg, which will be http request args.
; Could also make a version that uses just an expr, and var capture.
; Is there a way to ensure user doesn't use "fnid" as a key?

(mac aform (f . body)
  (w/uniq ga
    `(tag (form method 'post action fnurl*)
       (fnid-field (fnid (fn (,ga)
                           (prn "\r")
                           (,f ,ga))))
       ,@body)))

; Like aform except creates a fnid that will last for lasts seconds
; (unless the server is restarted).

(mac timed-aform (lasts f . body)
  (w/uniq (gl gf gi ga)
    `(withs (,gl ,lasts
             ,gf (fn (,ga) (prn "\r") (,f ,ga)))
       (tag (form method 'post action fnurl*)
         (fnid-field (if ,gl (timed-fnid ,gl ,gf) (fnid ,gf)))
         ,@body))))

(mac arform (f . body)
  `(tag (form method 'post action rfnurl*)
     (fnid-field (fnid ,f))
     ,@body))

(mac aformh (f . body)
  `(tag (form method 'post action fnurl*)
     (fnid-field (fnid ,f))
     ,@body))

(mac arformh (f . body)
  `(tag (form method 'post action rfnurl2*)
     (fnid-field (fnid ,f))
     ,@body))

; only unique per server invocation

(= unique-ids* (table))

(def unique-id ((o len 8))
  (let id (sym (rand-string (max 5 len)))
    (if (unique-ids* id)
        (unique-id)
        (= (unique-ids* id) id))))

(def srvlog (type . args)
  (w/appendfile o (string logdir* type "-" (memodate))
    (w/stdout o (apply prs (seconds) args) (prn))))

(with (lastasked nil lastval nil)

(def memodate ()
  (let now (seconds)
    (if (or (no lastasked) (> (- now lastasked) 60))
        (= lastasked now lastval (date))
        lastval)))

)

(= frontpage-admin-links*
  '(
    ("Manage applications"	"prompt")
    ("Add user"			"admin")
    ("Arc Prompt"		"repl")
   ))
(= frontpage-user-links*
  '(
    ("Arc reference"		"help")
    ("Log out"			"logout")
   ))
#|
(defop* || req
  (whitepage
    (pr "Arc server is running.")
    (let u (get-user req)
      (when u
        (pr "  Hello, ") (prbold u)
        (when (admin u)
          (each l frontpage-admin-links*
            (br)
            (apply link l)) )
        (each l frontpage-user-links*
          (br)
          (apply link l)) )
      (when (no u)
        (br)
        (link "Log in" "login") ))))
|#

(defop* || req
  (pr "<html>")
  (pr "<head><title>Arc Server</title></head>")
  (pr "<body><p>Hello world!</p></body>")
  (pr "</html>"))


#|
(defop* topips req
  (when (admin (get-user req))
    (whitepage
      (sptab
        (each ip (let leaders nil 
                   (maptable (fn (ip n)
                               (when (> n 100)
                                 (insort (compare > requests/ip*)
                                         ip
                                         leaders)))
                             requests/ip*)
                   leaders)
          (let n (requests/ip* ip)
            (row ip n (pr (num (* 100 (/ n requests*)) 1)))))))))
|#

(def ttest (ip)
  (let n (requests/ip* ip) 
    (list ip n (num (* 100 (/ n requests*)) 1))))

(def ensure-srvinstall ()
  (ensure-dir arcdir*)
  (ensure-dir logdir*)
  (ensure-dir rootdir*)
  (load-userinfo))

