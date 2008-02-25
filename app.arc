; Application Server.  Layer inserted 2 Sep 06.

; todo: def a general notion of apps of which the programming app is 
;   one and the news site another.
; give each user a place to store data?  A home dir?

; A user is simply a string: "pg". Use /whoami to test user cookie.

(= hpwfile*   "arc/hpw"
   adminfile* "arc/admins"
   cookfile*  "arc/cooks")

(def asv ((o port 8080))
  (load-userinfo)
  (serve port))

(def load-userinfo ()
  (= hpasswords*   (safe-load-table hpwfile*)
     admins*       (map string (errsafe (readfile adminfile*)))
     cookie->user* (safe-load-table cookfile*))
  (maptable (fn (k v) (= (user->cookie* v) k))
            cookie->user*))

; idea: a bidirectional table, so don't need two vars (and sets)

(= cookie->user* (table) user->cookie* (table) logins* (table))

(def get-user (req) 
  (let u (aand (alref (req 'cooks) "user") (cookie->user* (sym it)))
    (when u (= (logins* u) (req 'ip)))
    u))

(mac when-umatch (user req . body)
  `(if (is ,user (get-user ,req))
       (do ,@body)
       (mismatch-message)))

(def mismatch-message () (prn "Dead link: users don't match."))

(mac when-umatch/r (user req . body)
  `(if (is ,user (get-user ,req))
       (do ,@body)
       "mismatch"))

(defop mismatch req (mismatch-message))

(mac uform (user req after . body)
  `(aform (fn (,req)
            (when-umatch ,user ,req
              ,after))
     ,@body))

(mac urform (user req after . body)
  `(arform (fn (,req)
             (when-umatch/r ,user ,req 
               ,after))
     ,@body))

; Like onlink, but checks that user submitting the request is the
; same it was generated for.  Really should log the username and
; ip addr of every genlink, and check if they match.

(mac userlink (user text . body)  
  (w/uniq req
    `(linkf ,text (,req) 
       (when-umatch ,user ,req ,@body))))


(defop admin req (admin-gate (get-user req)))

(def admin-gate (u)
  (if (admin u)
      (admin-page u)
      (login-page 'login nil
                  (fn (u ip)  (admin-gate u)))))

(def admin (u) (and u (mem u admins*)))

(def user-exists (u) (and u (hpasswords* u) u))

(def admin-page (user . msg)
  (whitepage 
    (prbold "Admin: ")
    (hspace 20)
    (pr user " | ")
    (w/link (do (logout-user user)
                (whitepage (pr "Bye " user ".")))
      (pr "logout"))
    (when msg (hspace 10) (map pr msg))
    (br2)
    (aform (fn (req)
             (when-umatch user req
               (with (u (arg req "u") p (arg req "p"))
                 (if (or (no u) (no p) (is u "") (is p ""))
                      (pr "Bad data.")
                     (user-exists u)
                      (admin-page user "User already exists: " u)
                      (do (create-acct u p)
                          (admin-page user))))))
      (pwfields "create (server) account"))))

; need to define a notion of a hashtable that's always written
; to a file when modified

(def cook-user (user)
  (let id (new-user-cookie)
    (= (cookie->user*   id) user
       (user->cookie* user)   id)
    (save-table cookie->user* cookfile*)
    id))

; Unique-ids are only unique per server invocation.

(def new-user-cookie ()
  (let id (unique-id)
    (if (cookie->user* id) (new-user-cookie) id)))

(def logout-user (user)
  (wipe (logins* user))
  (wipe (cookie->user* (user->cookie* user)) (user->cookie* user))
  (save-table cookie->user* cookfile*))

(def create-acct (user pw)
  (set-pw user pw))

(def disable-acct (user)
  (set-pw user (rand-string 20))
  (logout-user user))
  
(def set-pw (user pw)
  (= (hpasswords* user) (and pw (shash pw)))
  (save-table hpasswords* hpwfile*))

(def hello-page (user ip)
  (whitepage (prs "hello" user "at" ip)))

(defop login req (login-page 'login))

; switch is one of: register, login, both
; afterward is a function on the newly created user, ip addr
;  or can be a list of such a fn and a string, in which case call fn
;  then redirect to string

; classic example of something that should just "return" a val
; via a continuation rather than going to a new page.

; ugly code-- too much duplication

(def login-page (switch (o msg nil) (o afterward hello-page))
  (whitepage
    (pagemessage msg)
    (when (in switch 'login 'both)
      (prbold "Login")
      (br2)
      (if (acons afterward)
          (let (f url) afterward
            (arformh (fn (req)
                       (logout-user (get-user req))
                       (aif (good-login (arg req "u") (arg req "p") (req 'ip))
                            (do (= (logins* it) (req 'ip))
                                (prcookie (user->cookie* it))
                                (f it (req 'ip))
                                url)
                            (flink (fn ignore (login-page switch 
                                                          "Bad login." 
                                                          afterward)))))
              (pwfields)))
          (aformh  (fn (req)
                     (logout-user (get-user req))
                     (aif (good-login (arg req "u") (arg req "p") (req 'ip))
                          (do (= (logins* it) (req 'ip))
                              (prcookie (user->cookie* it))
                              (prn)
                              (afterward it (req 'ip)))
                          (do (prn)
                              (login-page switch "Bad login." afterward))))
            (pwfields)))
      (br2))
    (when (in switch 'register 'both)
      (prbold "Create Account")
      (br2)
      (if (acons afterward)
          (let (f url) afterward
            (arformh (fn (req)
                       (logout-user (get-user req))
                       (with (user (arg req "u") pw (arg req "p"))
                         (aif (bad-newacct user pw)
                              (flink (fn ignore
                                       (login-page switch it afterward)))
                              (do (create-acct user pw)
                                  (= (logins* user) (req 'ip))
                                  (prcookie (cook-user user))
                                  (f user (req 'ip))
                                  url))))
              (pwfields "create account")))
          (aformh (fn (req)
                    (logout-user (get-user req))
                    (with (user (arg req "u") pw (arg req "p"))
                      (aif (bad-newacct user pw)
                           (do (prn)
                               (login-page switch it afterward))
                           (do (create-acct user pw)
                               (= (logins* user) (req 'ip))
                               (prcookie (cook-user user))
                               (prn)
                               (afterward user (req 'ip))))))
            (pwfields "create account"))))))
  
(def prcookie (cook)
  (prn "Set-Cookie: user=" cook "; expires=Sun, 17-Jan-2038 19:14:07 GMT"))

(def pwfields ((o label "login"))
  (inputs u username 20 nil
          p password 20 nil)
  (br)
  (submit label))

(= good-logins* (queue) bad-logins* (queue))

(def good-login (user pw ip)
  (let record (list (seconds) ip user)
    (if (and user pw (aand (shash pw) (is it (hpasswords* user))))
        (do (unless (user->cookie* user) (cook-user user))
            (enq-limit record good-logins*)
            user)
        (do (enq-limit record bad-logins*)
            nil))))

; can remove this once sha1 installed on pi

; Create a file in case people have quote chars in their pws.  I can't 
; believe there's no way to just send the chars.

(def shash (str)
  (let fname (+ "/tmp/shash" (rand-string 10))
    (w/outfile f fname (disp str f))
    (let res (tostring (system (+ "openssl dgst -sha1 <" fname)))
      (do1 (cut res 0 (- (len res) 1))
           (rmfile fname)))))

(def bad-newacct (user pw)
  (if (no (goodname user 2 15))
       "Usernames can only contain letters, digits, dashes and 
        underscores, and should be between 2 and 15 characters long.  
        Please choose another."
      (let dcuser (downcase user)
        (some [is dcuser (downcase _)] (keys hpasswords*)))
       "That username is taken. Please choose another."
      (or (no pw) (< (len pw) 4))
       "Passwords should be a least 4 characters long.  Please 
        choose another."
       nil))

(def goodname (str (o min 1) (o max nil))
  (and (isa str 'string)
       (>= (len str) min)
       (~find (fn (c) (no (or (alphadig c) (in c #\- #\_))))
              str)
       (isnt (str 0) #\-)
       (or (no max) (<= (len str) max))
       str))


(defop logout req
  (aif (get-user req)
       (do (logout-user it)
           (pr "Logged out."))
       (pr "You were not logged in.")))

(defop whoami req
  (aif (get-user req)
       (prs it 'at (req 'ip))
       (do (pr "You are not logged in. ")
           (w/link (login-page 'both) (pr "Log in"))
           (pr "."))))



(= formwid* 60 bigformwid* 80 numwid* 8 formatdoc-url* nil)

; Eventually figure out a way to separate type name from format of 
; input field, instead of having e.g. toks and bigtoks

(def varfield (typ id val)
  (if (in typ 'string 'string1 'url)
       (gentag input type 'text name id value val size formwid*)
      (in typ 'num 'int 'posint)
       (gentag input type 'text name id value val size numwid*)
      (in typ 'users 'toks)
       (gentag input type 'text name id value (tostring (apply prs val))
                     size formwid*)    
      (is typ 'sexpr)
       (gentag input type 'text name id 
                     value (tostring (map [do (write _) (sp)] val))
                     size formwid*)
      (in typ 'syms 'text 'doc 'mdtext 'mdtext2 'lines 'bigtoks)
       (let text (if (in typ 'syms 'bigtoks)
                      (tostring (apply prs val))
                     (in typ 'mdtext 'mdtext2)
                      (unmarkdown val)
                     (no val)
                      ""
                     val)
         (tag (textarea cols (if (is typ 'doc) bigformwid* formwid*) 
                        rows (needrows text formwid* 4)
                        wrap 'virtual 
                        style (if (is typ 'doc) "font-size:8.5pt")
                        name id)
           (prn) ; needed or 1 initial newline gets chopped off
           (pr text))
         (when (and formatdoc-url* (in typ 'mdtext 'mdtext2))
           (pr " ")
           (tag (font size -2)
             (link "help" formatdoc-url* (gray 175)))))
      (caris typ 'choice)
       (menu id (cddr typ) val)
      (is typ 'yesno)
       (menu id '("yes" "no") (if val "yes" "no"))
      (is typ 'hexcol)
       (gentag input type 'text name id value val); was (hexrep val)
       (err "unknown varfield type" typ)))

(def text-rows (text wid (o pad 3))
  (+ (trunc (/ (len text) (* wid .8))) pad))

(def needrows (text cols (o pad 0))
  (+ pad (max (+ 1 (count #\newline text))
              (roundup (/ (len text) (- cols 5))))))

(def varline (typ id val)
  (if (in typ 'users 'syms 'toks 'bigtoks)  (apply prs val)
      (is typ 'lines)                       (map prn val)
      (is typ 'yesno)                       (pr (if val 'yes 'no))
      (caris typ 'choice)                   (varline (cadr typ) nil val)
      (text-type typ)                       (pr (or val ""))
                                            (pr val)))

(def text-type (typ) (in typ 'string 'string1 'url 'text 'mdtext 'mdtext2))

; Newlines in forms come back as /r/n.  Only want the /ns. Currently
; remove the /rs in individual cases below.  Could do it in aform or
; even in the parsing of http requests, in the server.

; Need the calls to striptags so that news users can't get html
; into a title or comment by editing it.  If want a form that 
; can take html, just create another typ for it.

(def readvar (typ str (o fail nil))
  (case (carif typ)
    string  (striptags str)
    string1 (if (is str "") fail (striptags str))
    url     (if (is str "") str (valid-url str) (striptags str) fail)
    num     (let n (saferead str) (if (number n) n fail))
    int     (let n (saferead str)
              (if (number n) (round n) fail))
    posint  (let n (saferead str)
              (if (and (number n) (> n 0)) (round n) fail))
    text    (striptags str)
    doc     (striptags str)
    mdtext  (md-from-form str)
    mdtext2 (md-from-form str t)                      ; for md with no links
 ;  sym     (aif (tokens str) (sym (car it)) fail)
 ;  syms    (map sym (tokens str))
    sexpr   (errsafe (readall str))
    users   (rem [no (goodname _)] (tokens str))
    toks    (tokens str)
    bigtoks (tokens str)
 ;  lines   (or (splitlines (= sss str)) fail)
    choice  (readvar (cadr typ) str)
    yesno   (is str "yes")
    hexcol  (if (hex>color str) str fail) ; was (or (hex>color str) fail)
            (err "unknown readvar type" typ)))

(def splitlines (str)
  (map [rem #\return _] (split (cons #\newline "") str)))

(= fail* (uniq))
  
; Takes a list of fields of the form (type label value view modify) and 
; a fn f and generates a form such that when submitted (f label newval) 
; will be called for each valid value.  Finally done is called.

(def vars-form (user fields f done (o button "update") (o lasts))
  (timed-aform lasts
               (fn (req)
                 (when-umatch user req
                   (each (k v) (req 'args)
                     (let name (sym k)
                       (awhen (find [is (cadr _) name] fields)
                         (let (typ id val mod) it
                           (when (and mod v)
                             (let newval (readvar typ v fail*)
                               (unless (is newval fail*)
                                 (f name newval))))))))
                   (done)))
     (tab
       (showvars fields))
     (unless (all [no (_ 4)] fields)  ; no modifiable fields
       (br)
       (submit button))))
                
(def showvars (fields)
  (each (typ id val view mod question) fields
    (when view
      (when question
        (tr (td (prn question))))
      (tr (unless question (tag (td valign 'top)  (pr id ":")))
          (td ((if mod varfield varline) typ id val)))
      (prn))))

; http://daringfireball.net/projects/markdown/syntax

(def md-from-form (str (o nolinks))
  (markdown (trim (rem #\return (esc<>& str)) 'end) 60 nolinks))

(def markdown (s (o maxurl) (o nolinks))
  (let ital nil
    (tostring
      (forlen i s
        (iflet (newi spaces) (indented-code s i (if (is i 0) 2 0))
               (do (pr  "<p><pre><code>")
                 (let cb (code-block s (- newi spaces 1))
                   (pr cb)
                   (= i (+ (- newi spaces 1) (len cb))))
                 (pr "</code></pre>"))
               (iflet newi (parabreak s i (if (is i 0) 1 0))
                      (do (unless (is i 0) (pr "<p>"))
                          (= i (- newi 1)))
                      (and (is (s i) #\*)
                           (or ital 
                               (atend i s) 
                               (and (~whitec (s (+ i 1)))
                                    (pos #\* s (+ i 1)))))
                       (do (pr (if ital "</i>" "<i>"))
                           (= ital (no ital)))
                      (and (no nolinks)
                           (or (litmatch "http://" s i) 
                               (litmatch "https://" s i)))
                       (withs (n   (urlend s i)
                               url (cut s i n))
                         (tag (a href url rel 'nofollow)
                           (pr (if (no maxurl) url (ellipsize url maxurl))))
                         (= i (- n 1)))
                       (writec (s i))))))))

(def indented-code (s i (o newlines 0) (o spaces 0))
  (let c (s i)
    (if (nonwhite c)
         (if (and (> newlines 1) (> spaces 1))
             (list i spaces)
             nil)
        (atend i s)
         nil
        (is c #\newline)
         (indented-code s (+ i 1) (+ newlines 1) 0)
         (indented-code s (+ i 1) newlines       (+ spaces 1)))))

(def parabreak (s i (o newlines 0))
  (let c (s i)
    (if (or (nonwhite c) (atend i s))
        (if (> newlines 1) i nil)
        (parabreak s (+ i 1) (+ newlines (if (is c #\newline) 1 0))))))
           

; Returns the index of the first char not part of the url beginning
; at i, or len of string if url goes all the way to the end.

; Note that > immediately after a url (http://foo.com>) will cause
; an odd result, because the > gets escaped to something beginning
; with &, which is treated as part of the url.  Perhaps the answer
; is just to esc<>& after markdown instead of before.

(def urlend (s i)
  (let c (s i)
    (if (atend i s)
         (if ((orf punc delimc whitec) c) i (+ i 1))
        (if (or (whitec c)
                (delimc c) 
                (and (punc c)
                     ((orf whitec delimc) (s (+ i 1)))))
            i
            (urlend s (+ i 1))))))

(def delimc (c)
  (in c #\( #\) #\[ #\] #\{ #\} #\"))


(def code-block (s i)
  (tostring
    (until (let left (- (len s) i 1)
             (or (is left 0)
                 (and (> left 2)
                      (is (s (+ i 1)) #\newline)
                      (nonwhite (s (+ i 2))))))
     (writec (s (++ i))))))

(def unmarkdown (s)
  (tostring
    (forlen i s
      (if (litmatch "<p>" s i)
           (do (++ i 2) 
               (unless (is i 2) (pr "\n\n")))
          (litmatch "<i>" s i)
           (do (++ i 2) (pr #\*))
          (litmatch "</i>" s i)
           (do (++ i 3) (pr #\*))
          (litmatch "<a href=" s i)
           (let endurl (posmatch [in _ #\> #\space] s (+ i 9))
             (if endurl
                 (do (pr (cut s (+ i 9) (- endurl 1)))
                     (= i (aif (posmatch "</a>" s endurl)
                               (+ it 3)
                               endurl)))
                 (writec (s i))))
          (litmatch "<pre><code>" s i)
           (awhen (findsubseq "</code></pre>" s (+ i 12))
             (pr (cut s (+ i 11) it))
             (= i (+ it 12)))
          (litmatch "<pre><code>" s i)
           (awhen (findsubseq "</code></pre>" s (+ i 12))
             (pr (cut s (+ i 11) it))
             (= i (+ it 12)))
          (writec (s i))))))


(mac defopl (name parm . body)
  `(defop ,name ,parm
     (if (get-user ,parm)
         (do ,@body) 
         (login-page 'both
                     "You need to be logged in to do that."
                     (list (fn (u ip))
                           (string ',name (reassemble-args ,parm)))))))

