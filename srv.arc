; (server) then http://tintin.archub.org:8080/foo 

; could make form fields that know their value type because of
; gensymed names, and so the receiving fn gets args that are not
; strings but parsed values.

; write w/socket

; set breaksrv* to t to be able to ^c the server

(= arcdir* "arc/" logdir* "arc/logs/" quitsrv* nil breaksrv* nil) 

(def serve ((o port 8080))
  (nil! quitsrv*)
  (ensure-install)
  (let s (open-socket port) 
    (prn "ready to serve port " port) ; (flushout)
    (= currsock* s)
    (after (while (no quitsrv*) 
             (if breaksrv* 
                 (handle-request s)
                 (errsafe (handle-request s))))
           (close s)
           (prn "quit server"))))

(def serve1 ((o port 8080))
  (let s (open-socket port)
    (after (handle-request s) (close s))))

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

(= requests* 0 requests/ip* (table) throttle-ips* (table) throttle-time* 60)

(def handle-request (s (o life threadlife*))
  (if (< (len (= srvthreads* (rem dead srvthreads*))) 
              threadlimit*)
      (with ((i o ip) (socket-accept s))
        (++ requests*)
        (= (requests/ip* ip) (+ 1 (or (requests/ip* ip) 0)))
        (let th (thread (fn () 
                          (if (throttle-ips* ip) (sleep (rand throttle-time*)))
                          (handle-request-thread i o ip)))
          (push th srvthreads*)
          (thread (fn ()
                    (sleep life)
                    (unless (dead th) (prn "srv thread took too long"))
                    (break-thread th)
                    (close o)
                    (close i)))))
      (sleep .2)))

(def handle-request-thread (i o ip)
  (with (nls 0 lines nil line nil responded nil)
    (after
      (whilet c (and (no responded) (readc i))
        (if srv-noisy* (pr c))
        (if (is c #\newline)
            (if (is (++ nls) 2) 
                (do (let (type op args n cooks) (parseheader (rev lines))
                      (srvlog 'srv ip type op cooks)
                      (case type
                        get  (respond o op args cooks ip)
                        post (handle-post i o op n cooks ip)
                             (respond-err o "Unknown request: " (car lines))))
                    (= responded t))
                (do (push (coerce (rev line) 'string) lines)
                    (= line nil)))
            (unless (is c #\return)
              (push c line)
              (= nls 0))))
      (close o)
      (close i)))
  (harvest-fnids))

; Could ignore return chars (which come from textarea fields) here by
; (unless (is c #\return) (push c line))

(def handle-post (i o op n cooks ip)
  (if srv-noisy* (pr "Post Contents: "))
  (if (no n)
      (respond-err o "Post request without Content-Length.")
      (let line nil
        (whilet c (and (> n 0) (readc i))
          (if srv-noisy* (pr c))
          (-- n)
          (push c line)) 
        (if srv-noisy* (pr "\n\n"))
        (respond o op (parseargs (coerce (rev line) 'string)) cooks ip))))

(= header* "HTTP/1.0 200 OK
Content-Type: text/html
Connection: close")

(= gif-header* "HTTP/1.0 200 OK
Content-Type: image/gif
Connection: close")

(= rdheader* "HTTP/1.0 302 Moved")

(= srvops* (table) redirectors* (table) optimes* (table))

(def save-optime (name elapsed)
  (unless (optimes* name) (= (optimes* name) (queue)))
  (enq-limit elapsed (optimes* name) 1000))

; For ops that want to add their own headers.  They must thus remember 
; to prn a blank line before anything meant to be part of the page.

(mac defop-raw (name parms . body)
  (w/uniq t1
    `(= (srvops* ',name) 
        (fn ,parms 
          (let ,t1 (msec)
            (do1 (do ,@body)
                 (save-optime ',name (- (msec) ,t1))))))))

(mac defopr-raw (name parms . body)
  `(= (redirectors* ',name) t
      (srvops* ',name)      (fn ,parms ,@body)))

(mac defop (name parm . body)
  (w/uniq gs
    `(defop-raw ,name (,gs ,parm) 
       (w/stdout ,gs (prn) ,@body))))

; Defines op as a redirector.  Its retval is new location.

(mac defopr (name parm . body)
  (w/uniq gs
    `(do (t! (redirectors* ',name))
         (defop-raw ,name (,gs ,parm)
           ,@body))))

;(mac testop (name . args) `((srvops* ',name) ,@args))

(deftem request
  args  nil
  cooks nil
  ip    nil)

(= unknown-msg* "Unknown operator.")

(def respond (str op args cooks ip)
  (w/stdout str
    (if (gifname op)
        (do (prn gif-header*)
            (prn)
            (w/infile i (coerce op 'string)
              (whilet b (readb i)
                (writeb b str))))
        (aif (srvops* op)
             (let req (inst 'request 'args args 'cooks cooks 'ip ip)
               (if (redirectors* op)
                   (do (prn rdheader*)
                       (let loc (it str req) ; may write to str, e.g. cookies
                         (prn "Location: " loc))
                       (prn))
                   (do (prn header*)
                       (it str req))))
             (respond-err str unknown-msg*)))))

(def gifname (sym)
  (let str (coerce sym 'string)
    (and (endmatch ".gif" str) (~find #\/ str))))

(def respond-err (str msg . args)
  (w/stdout str
    (prn header*)
    (prn)
    (apply pr msg args)))

(def parseheader (lines)
  (let (type op args) (parseurl (car lines))
    (list type
          op
          args
          (and (is type 'post)
               (some (fn (s)
                       (and (begins s "Content-Length:")
                            (coerce (cadr (tokens s)) 'int)))
                     (cdr lines)))
          (some (fn (s)
                  (and (begins s "Cookie:")
                       (parsecookies s)))
                (cdr lines)))))

; (parseurl "GET /p1?foo=bar&ug etc") -> (get p1 (("foo" "bar") ("ug")))

(def parseurl (s)
  (let (type url) (tokens s)
    (let (base args) (tokens url #\?)
      (list (coerce (downcase type) 'sym)
            (coerce (subseq base 1) 'sym)
            (if args
                (parseargs args)
                nil)))))

; don't urldecode field names or anything in cookies; correct?

(def parseargs (s)
  (map (fn ((k v)) (list k (urldecode v)))
       (map [tokens _ #\=] (tokens s #\&))))

(def parsecookies (s)
  (map [tokens _ #\=] 
       (cdr (tokens s [or (whitec _) (is _ #\;)]))))

(def arg (req key) (alref (req 'args) key))

; *** Warning: does not currently urlencode args, so if need to do
; that replace v with (urlencode v).

(def reassemble-args (req)
  (aif (req 'args)
       (apply string "?" (intersperse '&
                                      (map (fn (pair)
                                            (let (k v) pair
                                              (string k '= v)))
                                           it)))
       ""))

(= fns* (table) fnids* nil timed-fnids* nil)

; count on huge (expt 64 10) size of fnid space to avoid clashes

(def new-fnid ()
  (let key (sym (rand-string 10))
    (if (fns* key)
        (new-fnid)
        key)))

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
;  (tag (a href (url-for (afnid (fn (req) (prn) (pr "my fnid is " it)))))
;    (pr "click here")))

; To be more sophisticated, instead of killing fnids, could first 
; replace them with fns that tell the server it's harvesting too 
; aggressively if they start to get called.  But the right thing to 
; do is estimate what the max no of fnids can be and set the harvest 
; limit there-- beyond that the only solution is to buy more memory.

(def harvest-fnids ((o n 20000)) 
  (when (> (len fns*) n) 
    (atomic
      (pull (fn ((id created lasts))
              (when (> (- (seconds) created) lasts)    
                (nil! (fns* id))
                t))
            timed-fnids*))
    (atlet nharvest (truncate (/ n 10))
      (let (kill keep) (splitn nharvest (rev fnids*)) 
        (= fnids* (rev keep)) 
        (each id kill 
          (nil! (fns* id)))))))

(= fnurl* "x" rfnurl* "r" rfnurl2* "y" jfnurl* "a")
 
(defop-raw x (str req)
  (let id (sym (arg req "fnid"))
    (aif (fns* id)
         (w/stdout str (it req))
         (w/stdout str (prn) (pr "unknown or expired link")))))

(defopr-raw y (str req)
  (let id (sym (arg req "fnid"))
    (aif (fns* id)
         (w/stdout str (it req))
         "deadlink")))

; For asynchronous calls; discards the page.  Would be better to tell
; the fn not to generate it.

(defop-raw a (str req)
  (let id (sym (arg req "fnid"))
    (aif (fns* id) (tostring (it req)))))

(defopr r req
  (let id (sym (arg req "fnid"))
    (aif (fns* id)
         (it req)
         "deadlink")))

(defop deadlink req
  (pr "unknown or expired link"))

(def url-for (fnid)
  (string fnurl* "?fnid=" fnid))

(def flink (f)
  (string fnurl* "?fnid=" (fnid (fn (req) (prn) (f req)))))

; couldn't I just say (fnid f) here?

(def rflink (f)
  (string rfnurl* "?fnid=" (fnid (fn (req) (f req)))))
  
; Since it's just an expr, gensym a parm for (ignored) args.

(mac w/link (expr . body)
  (w/uniq g
    `(tag (a href (flink (fn (,g) ,expr)))
       ,@body)))

(mac w/rlink (expr . body)
  (w/uniq g
    `(tag (a href (rflink (fn (,g) ,expr)))
       ,@body)))

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
  (w/uniq g
    `(tag-if ,test (a href (flink (fn (,g) ,expr)))
       ,@body)))

; f should be a fn of one arg, which will be http request args.
; Could also make a version that uses just an expr, and var capture.
; Is there a way to ensure user doesn't use "fnid" as a key?

(mac aform (f . body)
  (w/uniq (gi ga)
    `(let ,gi (fnid (fn (,ga)
                      (prn)
                      (,f ,ga)))
       (tag (form method 'post action fnurl*)
         (gentag input type 'hidden name 'fnid value ,gi)
         ,@body))))

; Like aform except creates a fnid that will last for lasts seconds
; (unless the server is restarted).

(mac timed-aform (lasts f . body)
  (w/uniq (gl gf gi ga)
    `(withs (,gl ,lasts
             ,gf (fn (,ga) (prn) (,f ,ga))
             ,gi (if ,gl (timed-fnid ,lasts ,gf) (fnid ,gf)))
       (tag (form method 'post action fnurl*)
         (gentag input type 'hidden name 'fnid value ,gi)
         ,@body))))

(mac arform (f . body)
  (w/uniq gi
    `(let ,gi (fnid ,f)
       (tag (form method 'post action rfnurl*)
         (gentag input type 'hidden name 'fnid value ,gi)
         ,@body))))

(mac aformh (f . body)
  (w/uniq gi
    `(let ,gi (fnid ,f)
       (tag (form method 'post action fnurl*)
         (gentag input type 'hidden name 'fnid value ,gi)
         ,@body))))

(mac arformh (f . body)
  (w/uniq gi
    `(let ,gi (fnid ,f)
       (tag (form method 'post action rfnurl2*)
         (gentag input type 'hidden name 'fnid value ,gi)
         ,@body))))

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

(defop || req
  (pr "It's alive."))

(defop topips req
  (when (admin (get-user req))
    (whitepage
      (spacetable
        (each ip (let leaders nil 
                   (maptable (fn (ip n)
                               (when (> n 100)
                                 (insort (compare > requests/ip*)
                                         ip
                                         leaders)))
                             requests/ip*)
                   leaders)
          (let n (requests/ip* ip)
            (row ip n (num (* 100 (/ n requests*)) 1))))))))

(def ensure-install ()
  (ensure-dir arcdir*)
  (ensure-dir logdir*)
  (when (empty hpasswords*)
    (create-acct "frug" "frug")
    (writefile1 'frug adminfile*))
  (load-userinfo))


