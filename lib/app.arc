; Application Server.  Layer inserted 2 Sep 06.

; ideas:
; def a general notion of apps of which prompt is one, news another
; give each user a place to store data?  A home dir?

; A user is simply a string: "pg". Use /whoami to test user cookie.

(= hpwfile*   (+ srvdir* "hpw")
   emailfile* (+ srvdir* "emails")
   oidfile*   (+ srvdir* "openids")
   adminfile* (+ srvdir* "admins")
   cookfile*  (+ srvdir* "cooks"))

(def asv ((o port 8080))
  (load-userinfo)
  (serve port))

(def load-userinfo ()
  (= hpasswords*   (safe-load-table hpwfile*)
     emails*       (safe-load-table emailfile*)
     openids*      (safe-load-table oidfile*)
     admins*       (map string (errsafe (readfile adminfile*)))
     cookie->user* (safe-load-table cookfile*))
  (maptable (fn (k v) (= (user->cookie* v) k))
            cookie->user*))

; idea: a bidirectional table, so don't need two vars (and sets)

(= cookie->user* (table) user->cookie* (table) logins* (table))

(def get-user (req)
"Gets the user id string associated with 'req'.
Returns nil if no logged-in user."
  (let u (aand (alref req!cooks "user") (cookie->user* (sym it)))
    (when u (= (logins* u) req!ip))
    u))

(mac when-umatch (user req . body)
  `(if (is ,user (get-user ,req))
     (do ,@body)
     (mismatch-message)))

(def mismatch-message ()
  (prn "Dead link: users don't match."))

(mac when-umatch/r (user req . body)
  `(if (is ,user (get-user ,req))
     (do ,@body)
     "mismatch"))

(defop mismatch req (mismatch-message))

(mac uform (user req after . body)
"Like [[aform]] but also authenticates that the form is submitted by 'user'."
  `(aform (fn (,req)
            (when-umatch ,user ,req
              ,after))
     ,@body))

(mac urform (user req after . body)
"Like [[arform]] but also authenticates that the form is submitted by 'user'."
  `(arform (fn (,req)
             (when-umatch/r ,user ,req
               ,after))
     ,@body))

; Like onlink, but checks that user submitting the request is the
; same it was generated for.  For extra protection could log the
; username and ip addr of every genlink, and check if they match.

(mac ulink (user text . body)
  (w/uniq req
    `(linkf ,text (,req)
       (when-umatch ,user ,req ,@body))))


(defop admin req (admin-gate (get-user req)))

(def admin-gate (u)
  (if (admin u)
    (admin-page u)
    (login-page nil
                (fn (u ip)  (admin-gate u)))))

(def admin (u)
"Does user 'u' possess administrator privileges for the site?"
  (and u (mem u admins*)))

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
               (with (u (arg req "u")
                      p (arg req "p")
                      e (arg req "e"))
                 (if (or (no u) (no p) (is u "") (is p "") (no e))
                      (pr "Bad data.")
                     (user-exists u)
                      (admin-page user "User already exists: " u)
                      (do (create-acct u p e)
                          (admin-page user))))))
      (pwfields "create (server) account"))))

(def cook-user (user)
  (let id (new-user-cookie)
    (= (cookie->user*   id) user
       (user->cookie* user)   id)
    (save-table cookie->user* cookfile*)
    id))

(def new-user-cookie ()
  ; watch for collisions across server invocations
  (check (unique-id) ~cookie->user* (new-user-cookie)))

(def logout-user (user)
  (wipe (logins* user))
  (wipe (cookie->user* (user->cookie* user)) (user->cookie* user))
  (save-table cookie->user* cookfile*))

(def create-acct (user pw email)
  (set (dc-usernames* (downcase user)))
  (set-pw user pw)
  (set-email user email))

(def disable-acct (user)
  (set-pw user (rand-string 20))
  (logout-user user))

(def set-pw (user pw)
"Updates password for 'user'."
  (= (hpasswords* user) (and pw (shash pw)))
  (save-table hpasswords* hpwfile*))

(def set-email (user email)
  (= emails*.user email)
  (save-table emails* emailfile*))

(def hello-page (user ip)
  (whitepage (prs "hello" user "at" ip)))

(defop login req (login-page))

; afterward is either a function on the newly created username and
; ip address, in which case it is called to generate the next page
; after a successful login, or a pair of (function url), which means
; call the function, then redirect to the url.

; classic example of something that should just "return" a val
; via a continuation rather than going to a new page.

(def login-page ((o msg nil) (o afterward hello-page))
  (whitepage
    (pagemessage msg)
    (login-form afterward)
    (hook 'login-form afterward)
    (br2)
    (signup-form afterward)))

(def login-form (afterward)
  (prbold "Login")
  (br2)
  (fnform (fn (req) (login-handler req afterward))
          (fn ()
            (pwfields)
            (br2)
            (tag div (link "Forgot your password?" "/forgotpw")))
          (acons afterward)))

(def login-handler (req afterward)
  (logout-user (get-user req))
  (aif (good-login (arg req "u") (arg req "p") req!ip)
    (login it req!ip (user->cookie* it) afterward)
    (failed-login "Bad login." afterward)))

(def signup-form (afterward)
  (prbold "Create Account")
  (br2)
  (fnform (fn (req) (signup-handler req afterward))
          (fn ()
            (inputs u username 20 nil
                    p password 20 nil
                    e email 20 nil)
            (br)
            (submit "signup"))
          (acons afterward)))

(def signup-handler (req afterward)
  (logout-user (get-user req))
  (with (user   (arg req "u")
         pw     (arg req "p")
         email  (arg req "e"))
    (aif (bad-newacct user pw email)
      (failed-login it afterward)
      (do (create-acct user pw email)
          (login user req!ip (cook-user user) afterward)))))

(def login (user ip cookie afterward)
  (= (logins* user) ip)
  (prcookie cookie)
  (if (acons afterward)
    (let (f url) afterward
      (f user ip)
      url)
    (do (prrn)
        (afterward user ip))))

(def failed-login (msg afterward)
  (if (acons afterward)
    (flink (fn ignore (login-page msg afterward)))
    (do (prrn)
        (login-page msg afterward))))

(def prcookie (cook)
  (prrn "Set-Cookie: user=" cook "; expires=Sun, 17-Jan-2038 19:14:07 GMT"))

(def pwfields ((o label "login"))
  (inputs u username 20 nil
          p password 20 nil)
  (br)
  (submit label))

(defop forgotpw req
  (prbold "What's your username?")
  (aform (fn (req)
           (let user (arg req "u")
             (iflet email emails*.user
               (do (email-forgotpw-link user email)
                   (pr "We've emailed you a link. Please click on it within the next few minutes to reset your password.")
                   (br2)
                   (pr "If you don't see it, check your spam folder."))
               (pr "You didn't provide a valid email. Please create a fresh account."))))
    (input "u" "" 20)
    (submit "I've forgotten my password. Help!")))

(def email-forgotpw-link (user email)
  (pipe-to (system "sendmail -t")
    (prn "To: " email)
    (prn "Subject: reset your password")
    (prn "From: " (emails* admins*.0))
    (prn)
    (prn "To reset your password, go here:")
    (prn site-url* (flink (fn ignore
                            (forgotpw-reset-page user))))))

(def forgotpw-reset-page (user (o msg))
  (if msg (pr msg))
  (aform (fn (req)
           (let newpw (arg req "p")
             (if (len< newpw 4)
               (forgotpw-reset-page user "Passwords should be at least 4 characters. Please choose another.")
               (do (set-pw user newpw)
                   (prn "Password changed.")
                   (link "Try logging in now." "/")))))
    (single-input "New password: " 'p 20 "update" t)))

(= good-logins* (queue) bad-logins* (queue))

(def good-login (user pw ip)
  (let record (list (seconds) ip user)
    (if (and user pw (aand (shash pw) (is it (hpasswords* user))))
      (do (unless (user->cookie* user) (cook-user user))
          (enq-limit record good-logins*)
          user)
      (do (enq-limit record bad-logins*)
          nil))))

; Create a file in case people have quote chars in their pws.  I can't
; believe there's no way to just send the chars.

(def shash (str)
  (let fname (mktemp "shash")
    (w/outfile f fname (disp str f))
    (after (chomp:tostring:system:+ "openssl dgst -sha512 <" fname)
      (rmfile fname))))

(= dc-usernames* (table))

(def username-taken (user)
  (when (empty dc-usernames*)
    (each (k v) hpasswords*
      (set (dc-usernames* (downcase k)))))
  (dc-usernames* (downcase user)))

(def bad-newacct (user pw email)
  (if (no (goodname user 2 15))
       "Usernames can only contain letters, digits, dashes and
        underscores, and should be between 2 and 15 characters long.
        Please choose another."
      (username-taken user)
       "That username is taken. Please choose another."
      (or (no pw) (len< pw 4))
       "Passwords should be a least 4 characters long.  Please
        choose another."
      (or (no email) (len< email 6))
       "Please provide an email, in case you need your password emailed to you."
       nil))

(def goodname (str (o min 1) (o max nil))
"Is 'str' a valid username?"
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
    (prs it 'at req!ip)
    (do (pr "You are not logged in. ")
        (w/link (login-page) (pr "Log in"))
        (pr "."))))

(= formwid* 60 bigformwid* 80 numwid* 16 formatdoc-url* nil)

; Eventually figure out a way to separate type name from format of
; input field, instead of having e.g. toks and bigtoks

(def varfield (typ id val)
  (if (in typ 'string 'string1 'url)
       (gentag input type 'text name id value val size formwid*)
      (in typ 'num 'int 'posint 'sym)
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
                     (is typ 'lines)
                      (tostring (apply pr (intersperse #\newline val)))
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
       (gentag input type 'text name id value val)
       (err "unknown varfield type" typ)))

(def text-rows (text wid (o pad 3))
  (+ (trunc (/ (len text) (* wid .8))) pad))

(def needrows (text cols (o pad 0))
  (+ pad (max (+ 1 (count #\newline text))
              (roundup (/ (len text) (- cols 5))))))

(def varline (typ id val (o liveurls))
  (if (in typ 'users 'syms 'toks 'bigtoks)  (apply prs val)
      (is typ 'lines)                       (map prn val)
      (is typ 'yesno)                       (pr (if val 'yes 'no))
      (caris typ 'choice)                   (varline (cadr typ) nil val)
      (is typ 'url)                         (if (and liveurls (valid-url val))
                                                (link val val)
                                                (pr val))
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
    string1 (if (blank str) fail (striptags str))
    url     (if (blank str) "" (valid-url str) (clean-url str) fail)
    num     (let n (errsafe:read str) (if (number n) n fail))
    int     (let n (errsafe:read str)
              (if (number n) (round n) fail))
    posint  (let n (errsafe:read str)
              (if (and (number n) (> n 0)) (round n) fail))
    text    (striptags str)
    doc     (striptags str)
    mdtext  (md-from-form str)
    mdtext2 (md-from-form str t)                      ; for md with no links
    sym     (or (sym:car:tokens str) fail)
    syms    (map sym (tokens str))
    sexpr   (errsafe (readall str))
    users   (rem [no (goodname _)] (tokens str))
    toks    (tokens str)
    bigtoks (tokens str)
    lines   (lines str)
    choice  (readvar (cadr typ) str)
    yesno   (is str "yes")
    hexcol  (if (hex>color str) str fail)
            (err "unknown readvar type" typ)))

; dates should be tagged date, and just redefine <

(def varcompare (typ)
  (if (in typ 'syms 'sexpr 'users 'toks 'bigtoks 'lines 'hexcol)
       (fn (x y) (> (len x) (len y)))
      (is typ 'date)
       (fn (x y)
         (or (no y) (and x (date< x y))))
       (fn (x y)
         (or (empty y) (and (~empty x) (< x y))))))


; (= fail* (uniq))

(def fail* ()) ; coudn't possibly come back from a form

; Takes a list of fields of the form (type label value view modify) and
; a fn f and generates a form such that when submitted (f label newval)
; will be called for each valid value.  Finally done is called.

(def vars-form (user fields f done (o button "update") (o lasts))
  (taform lasts
          (if (all [no (_ 4)] fields)
            (fn (req))
            (fn (req)
              (when-umatch user req
                (each (k v) req!args
                  (let name (sym k)
                    (awhen (find [is (cadr _) name] fields)
                      ; added sho to fix bug
                      (let (typ id val sho mod) it
                        (when (and mod v)
                          (let newval (readvar typ v fail*)
                            (unless (is newval fail*)
                              (f name newval))))))))
                (done))))
   (tab
     (showvars fields))
   (unless (all [no (_ 4)] fields)  ; no modifiable fields
     (br)
     (submit button))))

(def showvars (fields (o liveurls))
  (each (typ id val view mod question) fields
    (when view
      (when question
        (tr (td (prn question))))
      (tr (unless question (tag (td valign 'top)  (pr id ":")))
          (td (if mod
                (varfield typ id val)
                (varline  typ id val liveurls))))
      (prn))))

; http://daringfireball.net/projects/markdown/syntax

(def md-from-form (str (o nolinks))
  (markdown (trim (rem #\return (esc-tags str)) 'end) 60 nolinks))

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
                 (or (headmatch "http://" s i)
                     (headmatch "https://" s i)))
             (withs (n   (urlend s i)
                     url (clean-url (cut s i n)))
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

; If i is start a paragraph break, returns index of start of next para.

(def parabreak (s i (o newlines 0))
  (let c (s i)
    (if (or (nonwhite c) (atend i s))
      (if (> newlines 1) i nil)
      (parabreak s (+ i 1) (+ newlines (if (is c #\newline) 1 0))))))

; Returns the indices of the next paragraph break in s, if any.

(def next-parabreak (s i)
  (unless (atend i s)
    (aif (parabreak s i)
      (list i it)
      (next-parabreak s (+ i 1)))))

(def paras (s (o i 0))
  (if (atend i s)
    nil
    (iflet (endthis startnext) (next-parabreak s i)
      (cons (cut s i endthis)
            (paras s startnext))
      (list (trim (cut s i) 'end)))))


; Returns the index of the first char not part of the url beginning
; at i, or len of string if url goes all the way to the end.

; Note that > immediately after a url (http://foo.com>) will cause
; an odd result, because the > gets escaped to something beginning
; with &, which is treated as part of the url.  Perhaps the answer
; is just to esc-tags after markdown instead of before.

; Treats a delimiter as part of a url if it is (a) an open delimiter
; not followed by whitespace or eos, or (b) a close delimiter
; balancing a previous open delimiter.

(def urlend (s i (o indelim))
  (let c s.i
    (if (atend i s)
      (if (closedelim c)
            (if indelim (+ i 1) i)
          (or punc.c whitec.c opendelim.c)
            i
          :else
            (+ i 1))
      (let nextc (s (+ i 1))
        (if (or whitec.c
                (and punc.c whitec.nextc)
                (and (or whitec.nextc punc.nextc)
                     (or opendelim.c
                         (and closedelim.c no.indelim))))
          i
          (urlend s (+ i 1) (or (opendelim c)
                                (and indelim (~closedelim c)))))))))

(def opendelim (c)  (in c #\< #\( #\[ #\{))

(def closedelim (c) (in c #\> #\) #\] #\}))


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
      (if (headmatch "<p>" s i)
           (do (++ i 2)
               (unless (is i 2) (pr "\n\n")))
          (headmatch "<i>" s i)
           (do (++ i 2) (pr #\*))
          (headmatch "</i>" s i)
           (do (++ i 3) (pr #\*))
          (headmatch "<a href=" s i)
           (let endurl (posmatch [in _ #\> #\space] s (+ i 9))
             (if endurl
               (do (pr (cut s (+ i 9) (- endurl 1)))
                   (= i (aif (posmatch "</a>" s endurl)
                             (+ it 3)
                             endurl)))
               (writec (s i))))
          (headmatch "<pre><code>" s i)
           (awhen (posmatch "</code></pre>" s (+ i 12))
             (pr (cut s (+ i 11) it))
             (= i (+ it 12)))
          (writec (s i))))))

(mac defopl (name parm . body)
"Like [[defop]], but requires a logged in user."
  `(defop ,name ,parm
     (if (get-user ,parm)
       (do ,@body)
       (login-page "You need to be logged in to do that."
                   (list (fn (u ip))
                         (string ',name (reassemble-args ,parm)))))))

