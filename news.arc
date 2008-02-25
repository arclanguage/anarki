; News.  2 Sep 06.

; to run news: (nsv)
; put usernames of admins, separated by whitespace, in arc/admins

(= this-site*    "My Forum"
   site-url*     "http://news.yourdomain.com/"
   parent-url*   "http://www.yourdomain.com"
   favicon-url*  ""
   site-desc*    "What this site is about."               ; for rss feed
   site-color*   orange
   prefer-url*   t)


; Structures

; Could add (html) types like choice, yesno to profile fields.  But not 
; as part of deftem, which is defstruct.  Need another mac on top of 
; deftem.  Should not need the type specs in user-fields.

(deftem profile
  id         nil
  name       nil
  created    (seconds)
  auth       0
  member     nil
  submitted  nil
  karma      1
  weight     .5
  ignore     nil
  nodowns    nil
  email      nil
  about      nil
  showdead   nil
  noprocrast nil
  firstview  nil
  lastview   nil
  maxvisit   20 
  minaway    180
  topcolor   nil
  keys       nil)

(deftem item
  id         nil
  type       nil
  by         nil
  ip         nil
  time       (seconds)
  url        nil
  title      nil
  text       nil
  votes      nil   ; elts each (time ip user type score)
  score      0
  sockvotes  0
  dead       nil
  deleted    nil
  parent     nil
  kids       nil)


; Load and Save

(= newsdir*  "arc/news/"
   storydir* "arc/news/story/"
   profdir*  "arc/news/profile/"
   votedir*  "arc/news/vote/")

(= votes* (table) profs* (table))

(def nsv ((o port 8080))
  (map ensure-dir (list arcdir* newsdir* storydir* votedir* profdir*))
  (unless stories* (load-items))
  (if (empty profs*) (load-users))
  (asv port))

(def load-users ()
  (pr "load users: ")
  (noisy-each 100 id (dir profdir*)
    (load-user id)))

(def load-user (u)
  (= (votes* u) (load-table (+ votedir* u))
     (profs* u) (temload 'profile (+ profdir* u)))
  u)

(def init-user (u)
  (= (votes* u) (table) (profs* u) (inst 'profile 'id u))
  (save-votes u)
  (save-prof u)
  u)

; Need this because can create users on the server (for other apps)
; without setting up places to store their state as news users.
; See the admin op in app.arc.  So all calls to login-page from the 
; news app need to call this in the after-login fn.

(def ensure-news-user (u)
  (if (profs* u) u (init-user u)))

(def save-votes (u) (save-table (votes* u) (+ votedir* u)))

(def save-prof  (u) (save-table (profs* u) (+ profdir* u)))

(mac uvar (u k) `((profs* ,u) ',k))

(mac karma (u) `(uvar ,u karma))

(def users (f) (keep f (keys profs*)))

(= stories* nil comments* nil items* (table) url->story* (table)
   maxid* 0 initload* 15000)

; The dir expression yields stories in order of file creation time 
; (because arc infile truncates), so could just rev the list instead of
; sorting, but sort anyway.

; Note that stories* etc only include the initloaded (i.e. recent)
; ones, plus those created since this server process started.

; Could be smarter about preloading by keeping track of most popular pages.

(def load-items ()
  (pr "load items: ") 
  (with (items (table)
         ids   (sort > (map [coerce _ 'int] (dir storydir*))))
    (if ids (= maxid* (car ids)))
    (noisy-each 100 id (firstn initload* ids)
      (let i (load-item id)
        (push i (items i!type))))
    (= stories*  (rev items!story) comments* (rev items!comment))
    (hook 'initload items))
  (ensure-topstories))

(def ensure-topstories ()
  (aif (errsafe (readfile1 (+ newsdir* "topstories")))
       (= ranked-stories* (map item it))
       (do (prn "ranking stories.") 
           (gen-topstories))))

(def astory   (i) (is i!type 'story))

(def acomment (i) (is i!type 'comment))

(def load-item (id)
  (let i (temload 'item (string storydir* id))
    (= (items* id) i (i 'id) id)
    (awhen (and (astory i) (live i) i!url)
      (= (url->story* it) i))
    i))

(def new-item-id ()
  (let id (++ maxid*)
    (if (file-exists (string storydir* id)) (new-item-id) id)))

(def item (id)
  (or (items* id) (errsafe (load-item id))))

(def kids (x) (map item x!kids))

; For use on external item references (from urls).
; Checks id is int because people try e.g. item?id=363/blank.php

(def safe-item (id)
  (let id (if (isa id 'string) (saferead id) id)
    (and (ok-id id) (item id))))

(def ok-id (id) 
  (and (exact id) (<= 1 id maxid*)))

(def arg->item (req key)
  (safe-item:saferead (arg req key)))

(def live (i) (nor i!dead i!deleted))

(def live-child (d) (find live (kids d)))

(def save-item (i) (save-table i (string storydir* i!id)))

(def kill (i)
  (assert i!dead)
  (save-item i))

(def newslog args (apply srvlog 'news args))


; Ranking

; Votes divided by the age in hours to the gravityth power.
; Would be interesting to scale gravity in a slider.

(= gravity* 1.4 timebase* 120 front-threshold* 1)

(def frontpage-rank (s (o gravity gravity*))
  (/ (- (realscore s) 1)
     (expt (/ (+ (item-age s) timebase*) 60) gravity)))

(def realscore (i) (- i!score i!sockvotes))

(def item-age (i) (hours-since i!time))

(def user-age (u) (hours-since (uvar u created)))

; Only looks at the 1000 most recent stories, which might one day be a 
; problem if there is massive spam. 

(def gen-topstories ()
  (= ranked-stories* (rank-stories 180 1000 (memo frontpage-rank))))

(def save-topstories ()
  (writefile1 (map [_ 'id] (firstn 180 ranked-stories*))
              (+ newsdir* "topstories")))
 
(def rank-stories (n consider scorefn)
  (bestn n (compare > scorefn) (recent-stories consider)))

; The n most recent stories.  Use firstn when add virtual lists.

(def recent-stories (n (o id maxid*) (o acc nil))
  (if (or (< n 1) (< id 1))
      (rev acc)
      (let s (item id)
        (if (storylike s)
            (recent-stories (- n 1) (- id 1) (cons s acc))
            (recent-stories n       (- id 1) acc)))))

(def storylike (i) (and i (astory i)))

(def adjust-rank (s (o scorefn frontpage-rank))
  (insortnew (compare > (memo scorefn)) s ranked-stories*)
  (save-topstories))

; If something rose high then stopped getting votes, its score would
; decline but it would stay near the top.  Newly inserted stories would
; thus get stuck in front of it. I avoid this by regularly adjusting 
; the rank of a random top story.

(def rerank-random ((o depth 15))
  (when ranked-stories*
    (adjust-rank (ranked-stories* (rand (min depth (len ranked-stories*))))))
  (save-topstories))

(def topstories (user n (o threshold front-threshold*))
  (firstn-that n 
               [and (>= (realscore _) threshold) (cansee user _)]
               ranked-stories*))

; If had ip of current request could add clause below to make ignore
; tighter better, but wait till need to.

(def cansee (user i)
  (if i!deleted (admin user)
      i!dead    (or (is user i!by) (seesdead user))
                    ; ip of this request is i!ip
                t))

(def seesdead (user)
  (or (and user (uvar user showdead) (no (uvar user ignore)))
      (editor user)))

(def visible (user is)
  (keep [cansee user _] is))

(def cansee-descendant (user c)
  (or (cansee user c)
      (some [cansee-descendant user (item _)] 
            c!kids)))
  
(def editor (u) 
  (and u (or (admin u) (> (uvar u auth) 0))))

(def member (u) 
  (and u (or (admin u) (uvar u member))))


; Page Layout

(= up-url* "grayarrow.gif" down-url* "graydown.gif" logo-url* "y18.gif")

(defopr favicon.ico req favicon-url*)

(mac npage (title . body)
  `(tag html 
     (tag head 
       (prn "<link rel=\"stylesheet\" type=\"text/css\" href=\"news.css\">")
       (prn "<link rel=\"shortcut icon\" href=\"" favicon-url* "\">")
       (tag script (pr votejs*))
       (tag title (pr ,title)))
     (tag body 
       (center
         (tag (table border 0 cellpadding 0 cellspacing 0 width "85%"
                     bgcolor sand)
           ,@body)))))

(= pagefns* nil)

(mac fulltop (user label title whence . body)
  (w/uniq (gu gl gt gw)
    `(with (,gu ,user ,gl ,label ,gt ,title ,gw ,whence)
       (npage (+ this-site* (if ,gt (+ bar* ,gt) ""))
         (if (check-procrast ,gu)
             (do (pagetop 'full ,gl ,gt ,gu ,gw)
                 (hook 'page ,gu ,gl)
                 ,@body)
             (row (procrast-msg ,gu ,gw)))))))

(mac longpage (user t1 label title whence . body)
  (w/uniq (gu gt)
    `(with (,gu ,user ,gt ,t1)
       (fulltop ,gu ,label ,title ,whence
         (trtd ,@body)
         (trtd (vspace 10)
               (color-stripe (main-color ,gu))
               (br)
               (center
                 (hook 'longfoot)
                 (admin-bar ,gu (- (msec) ,gt) ,whence)))))))

(def admin-bar (user elapsed whence)
  (when (admin user)
    (br2)
    (w/bars
      (pr (len items*) "/" maxid* " loaded")
      (pr elapsed " msec")
      (link "settings" "newsadmin")
      (hook 'admin-bar user whence))))

(def color-stripe (c)
  (tag (table width "100%" cellspacing 0 cellpadding 1)
    (tr (tdcolor c))))

(mac shortpage (user label title whence . body)
  `(fulltop ,user ,label ,title ,whence 
     (trtd ,@body)))

(mac minipage (label . body)
  `(npage (+ this-site* bar* ,label)
     (pagetop nil ,label)
     (trtd ,@body)))


; remember to (= caching* 0) or won't see changes

(defop news.css req
  (pr "
body  { font-family:Verdana; font-size:10pt; color:#828282; }
td    { font-family:Verdana; font-size:10pt; color:#828282; }

.admin td   { font-family:Verdana; font-size:8.5pt; color:#000000; }
.subtext td { font-family:Verdana; font-size:  7pt; color:#828282; }

input    { font-family:Courier; font-size:10pt; color:#000000; }
input[type=\"submit\"] { font-family:Verdana; }
textarea { font-family:Courier; font-size:10pt; color:#000000; }

a:link    { color:#000000; text-decoration:none; } 
a:visited { color:#828282; text-decoration:none; }

.default { font-family:Verdana; font-size: 10pt; color:#828282; }
.admin   { font-family:Verdana; font-size:8.5pt; color:#000000; }
.title   { font-family:Verdana; font-size: 10pt; color:#828282; }
.adtitle { font-family:Verdana; font-size:  9pt; color:#828282; }
.subtext { font-family:Verdana; font-size:  7pt; color:#828282; }
.yclinks { font-family:Verdana; font-size:  8pt; color:#828282; }
.pagetop { font-family:Verdana; font-size: 10pt; color:#222222; }
.comhead { font-family:Verdana; font-size:  8pt; color:#828282; }
.comment { font-family:Verdana; font-size:  9pt; }
.dead    { font-family:Verdana; font-size:  9pt; color:#dddddd; }

.comment a:link, .comment a:visited { text-decoration:underline;}
.dead a:link, .dead a:visited { color:#dddddd; }
.pagetop a:visited { color:#000000;}
.topsel a:link, .topsel a:visited { color:#ffffff; }

.subtext a:link, .subtext a:visited { color:#828282; }
.subtext a:hover { text-decoration:underline; }

.comhead a:link, .subtext a:visited { color:#828282; }
.comhead a:hover { text-decoration:underline; }

.default p { margin-top: 8px; margin-bottom: 0px; }

.pagebreak {page-break-before:always}

pre { overflow: hidden; padding: 2px; }
pre:hover {overflow:auto} "))

; only need pre padding because of a bug in Mac Firefox

; Without setting the bottom margin of p tags to 0, 1- and n-para comments
; have different space at the bottom.  This solution suggested by Devin.
; Really am using p tags wrong (as separators rather than wrappers) and the
; correct thing to do would be to wrap each para in <p></p>.  Then whatever
; I set the bottom spacing to, it would be the same no matter how many paras
; in a comment. In this case by setting the bottom spacing of p to 0, I'm
; making it the same as no p, which is what the first para has.

; supplied by pb
;.vote { padding-left:2px; vertical-align:top; }
;.comment { margin-top:1ex; margin-bottom:1ex; color:black; }
;.vote IMG { border:0; margin: 3px 2px 3px 2px; }
;.reply { font-size:smaller; text-decoration:underline !important; }

(= votejs* "
function byId(id) {
  return document.getElementById(id);
}

function vote(node) {
  var v = node.id.split(/_/);   // {'up', '123'}
  var item = v[1]; 

  // adjust score
  var score = byId('score_' + item);
  var newscore = parseInt(score.innerHTML) + (v[0] == 'up' ? 1 : -1);
  score.innerHTML = newscore + (newscore == 1 ? ' point' : ' points');

  // hide arrows
  byId('up_'   + item).style.visibility = 'hidden';
  byId('down_' + item).style.visibility = 'hidden';

  // ping server
  var ping = new Image();
  ping.src = node.href;

  return false; // cancel browser nav
} ")


; Page top

(= sand (color 246 246 239) textgray (gray 130))

(def main-color (user) 
  (aif (and user (uvar user topcolor))
       (hex>color it)
       site-color*))

(def pagetop (switch label (o title) (o user) (o whence))
  (tr (tdcolor (main-color user)
        (tag (table border 0 cellpadding 0 cellspacing 0 width "100%"
                    style "padding:2px")
          (tr (gen-logo)
              (when (is switch 'full)
                (tag (td style "line-height:12pt; height:10px;")
                  (spanclass pagetop
                    (tag b (link this-site* "news"))
                    (hspace 10)
                    (toprow user label))))
             (if (is switch 'full)
                 (tag (td style "text-align:right;padding-right:4px;")
                   (spanclass pagetop (topright user whence)))
                 (tag (td style "line-height:12pt; height:10px;")
                   (spanclass pagetop (prbold label))))))))
  (map [_ user] pagefns*)
  (spacerow 10))

(def gen-logo ()
  (tag (td style "width:18px;padding-right:4px")
    (tag (a href parent-url*)
      (gentag img src logo-url* width 18 height 18 
                  style "border:1px white solid;"))))

(= toplabels* '(nil "new" "threads" "comments" "leaders" "*"))

(def toprow (user label)
  (w/bars 
    (toplink "new" "newest" label)
    (when user
      (toplink "threads" (threads-url user) label))
    (toplink "comments" "newcomments" label)
    (toplink "leaders"  "leaders"     label)
    (hook 'toprow user label)
    (link "submit")
    (unless (mem label toplabels*)
      (tag (font color white) (pr label)))))

(def toplink (name dest label)
  (tag-if (is name label) (span class 'topsel)
    (link name dest)))

(def topright (user whence (o showkarma t))
  (when user 
    (link user (user-url user))
    (when showkarma (pr  "&nbsp;(" (karma user) ")"))
    (pr "&nbsp;|&nbsp;"))
  (if user
      (rlinkf 'logout (req)
        (when-umatch/r user req
          (logout-user user)
          whence))
      (onlink "login"
        (login-page 'both nil 
                    (list (fn (u ip) 
                            (ensure-news-user u)
                            (newslog u 'top-login ip))
                          whence)))))


; News-Specific Defop Variants

(mac defopt (name parm test msg . body)
  `(defop ,name ,parm
     (if (,test (get-user ,parm))
         (do ,@body)
         (login-page 'both (+ "Please log in" ,msg ".")
                     (list (fn (u ip) (ensure-news-user u))
                           (string ',name (reassemble-args ,parm)))))))

(mac defopg (name parm . body)
  `(defopt ,name ,parm idfn "" ,@body))

(mac defope (name parm . body)
  `(defopt ,name ,parm editor " as an editor" ,@body))

(mac defopa (name parm . body)
  `(defopt ,name ,parm admin " as an administrator" ,@body))

(mac opexpand (definer name parms . body)
  (w/uniq gr
    `(,definer ,name ,gr
       (with (user (get-user ,gr) ip (,gr 'ip))
         (with ,(and parms (mappend [list _ (list 'arg gr (string _))]
                                    parms))
           (newslog user ',name ip ,@parms)
           ,@body)))))

(= newsop-names* nil)

(mac newsop args
  `(do (pushnew ',(car args) newsop-names*)
       (opexpand defop ,@args)))

(mac adop (name parms . body)
  (w/uniq g
    `(opexpand defopa ,name ,parms 
       (let ,g (string ',name)
         (shortpage user ,g ,g ,g
           ,@body)))))


; News Admin

(defopa newsadmin req (newsadmin-page (get-user req)))

; For emergency, real-time changes.  All are reset to the val in the 
; source code when restart server.

(def nad-fields ()
  `((num      caching           ,caching*          t t)
    (posint   front-threshold   ,front-threshold*  t t)
    (int      legit-threshold   ,legit-threshold*  t t)
    (bigtoks  url-kill          ,url-kill*         t t)
    (bigtoks  url-ignore        ,url-ignore*       t t)
    (bigtoks  comment-kill      ,comment-kill*     t t)
    (bigtoks  comment-ignore    ,comment-ignore*   t t)
    (bigtoks  ip-ban            ,ip-ban*           t t)))

; Need a util like vars-form for a collection of variables.
; Or could generalize vars-form to think of places (in the setf sense).

(def newsadmin-page (user)
  (newslog user 'newsadmin)
  (shortpage user "newsadmin" "News Admin Page" "newsadmin"
    (vars-form user (nad-fields)
               (fn (name val)
                 (case name
                   caching            (= caching* val)
                   front-threshold    (= front-threshold* val)
                   legit-threshold    (= legit-threshold* val)
                   url-kill           (= url-kill* val)
                   url-ignore         (= url-ignore* val)
                   comment-kill       (= comment-kill* val)
                   comment-ignore     (= comment-ignore* val)
                   ip-ban             (= ip-ban* val)))
               (fn () (newsadmin-page user))) 
    (br2)
    (aform (fn (req)
             (with (user (get-user req) subject (arg req "id"))
               (if (profs* subject)
                   (do (killallby subject)
                       (submitted-page user subject))
                   (if (admin user) (newsadmin-page user)))))
      (single-input "" 'id 20 "kill all by"))))


; Users

(newsop user (id)
  (if (profs* id)
      (user-page user id)
      (pr "No such user.")))

(def user-page (user subject)
  (let here (user-url subject)
    (shortpage user nil (+ "Profile: " subject) here
      (profile-form user subject)
      (br2)
      (when (some astory:item (uvar subject submitted))
        (underlink "submissions" (submitted-url subject)))
      (when (some acomment:item (uvar subject submitted))
        (sp)
        (underlink "comments" (threads-url subject)))
      (hook 'user user subject))))

(def profile-form (user subject)
  (let prof (profs* subject) 
    (vars-form user
               (user-fields user subject)
               (fn (name val) (= (prof name) val))
               (fn () (save-prof subject)
                      (user-page user subject)))))

(= topcolor-threshold* 250)

(def user-fields (user subject)
  (withs (e (editor user) 
          a (admin user) 
          w (is user subject)
          k (and w (> (karma user) topcolor-threshold*))
          u (or a w)
          m (or a (and (member user) w))
          p (profs* subject))
    `((string  user       ,subject                                  t   nil)
      (string  name       ,(p 'name)                               ,m  ,m)
      (string  created    ,(text-age:user-age subject)              t   nil)
      (string  password   ,(resetpw-link)                          ,w   nil)
      (string  saved      ,(saved-link user subject)               ,u   nil)
      (int     auth       ,(p 'auth)                               ,e  ,a)
      (yesno   member     ,(p 'member)                             ,a  ,a)
      (posint  karma      ,(p 'karma)                               t  ,a)
      (yesno   ignore     ,(p 'ignore)                             ,e  ,e)
      (num     weight     ,(p 'weight)                             ,a  ,a)
      (yesno   nodowns    ,(p 'nodowns)                            ,a  ,a)
      (mdtext  about      ,(p 'about)                               t  ,u)
      (string  email      ,(p 'email)                              ,u  ,u)
      (yesno   showdead   ,(p 'showdead)                           ,u  ,u)
      (yesno   noprocrast ,(p 'noprocrast)                         ,u  ,u)
      (string  firstview  ,(p 'firstview)                          ,a   nil)
      (string  lastview   ,(p 'lastview)                           ,a   nil)
      (posint  maxvisit   ,(p 'maxvisit)                           ,u  ,u)
      (posint  minaway    ,(p 'minaway)                            ,u  ,u)
      (sexpr   keys       ,(p 'keys)                               ,a  ,a)
      (hexcol  topcolor   ,(or (p 'topcolor) (hexrep site-color*)) ,k  ,k))))

(def saved-link (user subject)
  (when (or (admin user) (is user subject))
    (let n (len (voted-stories user subject))
      (if (is n 0)
          ""
          (tostring (underlink n (saved-url subject)))))))

(def resetpw-link ()
  (tostring (underlink "reset password" "resetpw")))


; Main Operators

; remember to set caching to 0 when testing non-logged-in 

(= caching* 0 perpage* 30 maxend* 200)

; Limiting that newscache can't take any arguments except the user.
; To allow other arguments, would have to turn the cache from a single 
; stored value to a hash table whose keys were lists of arguments.

(mac newscache (name user time . body)
  (w/uniq gc
    `(let ,gc (cache (fn () (* caching* ,time))
                     (fn () (tostring (let ,user nil ,@body))))
       (def ,name (,user) 
         (if ,user 
             (do ,@body) 
             (pr (,gc)))))))


(newsop news () (newspage user))

(newsop ||   () (newspage user))

;(newsop index.html () (newspage user))

(newscache newspage user 90
  (rerank-random)
  (listpage user (msec) (topstories user maxend*) nil nil "news" t))

(def listpage (user t1 items label title (o url label) (o number))
  (hook 'listpage user)
  (longpage user t1 label title url
    (display-items user items label title url 0 perpage* number)))


(newsop newest () (newestpage user))

; Note: dead/deleted items will persist for the remaining life of the 
; cached page.  If this were a prob, could make deletion clear caches.

(newscache newestpage user 40
  (rerank-random)
  (listpage user (msec) (newstories user maxend*) "new" "New Links" "newest" t))

(def newstories (user n)
  (firstn-that n [cansee user _] stories*))


(newsop best () (bestpage user))

(newscache bestpage user 1000
  (listpage user (msec) (beststories user maxend*) "best" "Top Links" "best" t))

; As no of stories gets huge, could test visibility in fn sent to best.

(def beststories (user n)
  (bestn n (compare > realscore) (visible user stories*)))


(newsop bestcomments () (bestcpage user))

(newscache bestcpage user 1000
  (listpage user (msec) (bestcomments user maxend*) 
            "best comments" "Best Comments" "bestcomments"))

(def bestcomments (user n)
  (bestn n (compare > realscore) (visible user comments*)))


(newsop lists () 
  (longpage user (msec) "lists" "Lists" "lists"
    (tag table
      (row "" (hspace 10))
      (row (link "best")         "" "Highest voted recent links.")
      (row (link "active")       "" "Most active current discussions.")
      (row (link "bestcomments") "" "Highest voted recent comments.")
      (when (admin user)
        (map [row (link _)] 
             '(optimes killed badguys badlogins goodlogins)))
      (hook 'listspage user))))


(def saved-url (user) (string "saved?id=" user))

(newsop saved (id) 
  (if (profs* id) 
      (savedpage user id) 
      (pr "No such user.")))

(def savedpage (user subject)
  (if (or (is user subject) (admin user))
      (listpage user (msec)
                (sort (compare < item-age) (voted-stories user subject)) 
               "saved" "Saved Links" (saved-url subject) t)
      (pr "Can't display that.")))

(def voted-stories (user subject)
  (keep [and (astory _) (cansee user _)]
        (map item (keys:votes* subject))))


; Story Display

(def display-items (user items label title whence 
                    (o start 0) (o end perpage*) (o number))
  (zerotable
    (let n start
      (each i (if end (cut items start end) items)
        (display-item (and number (++ n)) i user whence t)
        (spacerow (if (acomment i) 15 5))))
    (when end
      (let newend (+ end perpage*)
        (when (and (<= newend maxend*) (< end (len items)))
          (spacerow 10)
          (tr (tag (td colspan  (if number 2 1)))
              (tag (td class 'title)
                (morelink items label title end newend number))))))))

; This code is inevitably complex because the More fn needs to know 
; its own fnid in order to supply a correct whence arg to stuff on 
; the page it generates, like logout and delete links.

(def morelink (items label title end newend number)
  (tag (a href (url-for
                 (afnid (fn (req)
                          (prn)
                          (let user (get-user req)
                            (newslog user 'more label)
                            (longpage user (msec) label title (url-for it)
                              (display-items user items
                                             label title (url-for it)
                                             end newend number))))))
          rel 'nofollow)
    (pr "More")))

(def display-story (i s user whence)
  (when (or (cansee user s) (s 'kids))
    (tr (display-item-number i)
        (td (votelinks s user whence))
        (titlelink s s!url user))
    (tr (tag (td colspan (if i 2 1)))    
        (tag (td class 'subtext)
          (hook 'itemline s user)
          (itemline s user)
          (when (astory s) (commentlink s user))
          (editlink s user)
          (when (admin user)
            (pr bar*)
            (w/rlink (do (zap no s!dead)
                         (save-item s)
                         whence)
              (pr (if (s 'dead) "unkill" "kill"))))
          (deletelink s user whence)))))

(def display-item-number (i)
  (when i (tag (td align 'right valign 'top class 'title)
            (pr i "."))))

(= follow-threshold* 5)

(def titlelink (s url user)
  (tag (td class 'title)
    (if (cansee user s)
        (do (let toself (blank url)
              (tag (a href (if toself (item-url s) url)
                      rel  (unless (or toself 
                                       (> (realscore s) follow-threshold*))
                             'nofollow)) 
                (pr (s 'title))))
            (deadmark s user)
            (awhen (and (valid-url url) (sitename url))
              (spanclass comhead
                (pr " (" it ") "))))
        (pr (pseudo-text s)))))

(def pseudo-text (i)
  (if i!deleted "[deleted]" "[dead]"))

(def deadmark (i user)
  (when (and i!dead (seesdead user))
    (pr " [dead]"))
  (when (and i!deleted (admin user))
    (pr " [deleted]")))

(= downvote-threshold* 20 downvote-time* 1440)

(= votewid* 14)
      
(def votelinks (i user whence (o downtoo))
  (center
    (if (and (cansee user i)
             (or (no user)
                 (no ((votes* user) i!id))))
         (do (votelink i user whence 'up)
             (if (and downtoo 
                      (or (admin user)
                          (< (item-age i) downvote-time*))
                      (canvote user i 'down))
                 (do (br)
                     (votelink i user whence 'down))
                 (tag (span id (string "down_" i!id)))))
        (is user i!by)
         (do (tag (font color orange) (pr "*"))
             (br)
             (hspace votewid*))
        (hspace votewid*))))

(def votelink (i user whence dir)
  (tag (a id      (string dir '_ i!id)
          onclick (if user "return vote(this)")
          href    (if user
                      (string "vote?by=" user "&for=" i!id "&dir=" dir)
                      (flink (vote-fn i whence dir))))
    (gentag img src (case dir up up-url* down down-url*)
                border 0 vspace 3 hspace 2)))

(def vote-fn (i whence dir)
  (fn (req)
    (login-page 'both "You have to be logged in to vote."
                (list (fn (u ip)
                        (ensure-news-user u)
                        (newslog u 'vote-login ip)
                        (vote-for u i dir)
                        (logvote u i))
                      whence))))

; Not much stricter than whether to generate the arrow.  Further tests 
; applied in vote-for.

(def canvote (user i dir)
  (and user
       (no ((votes* user) i!id))
       (or (is dir 'up)
           (and (acomment i)
                (> (karma user) downvote-threshold*)
                (no (aand i!parent (is user ((item it) 'by))))))))

; Can't use this for links when not logged in, because doesn't know
; where to redirect after the login.  But that's few fnids anyway
; because pages with those links are cached.  Now that have Javascript
; voting, can use for all other votes because never have to regen
; the page.

(newsop vote (by for dir)
  (let dir (saferead dir)
    (if (isnt by user)
        (pr "User mismatch.")
        (aif (safe-item for)
             (if (and (in dir 'up 'down) (canvote user it dir))
                 (do (vote-for by it dir)
                     (logvote by it))
                 (pr "Can't make that vote."))
             (pr "No such item.")))))

(def itemline (i user)
  (when (cansee user i) 
    (when (news-type i)
      (tag (span id (string "score_" i!id))
        (pr i!score (plural i!score " point"))))
    (byline i)))

(def byline (i)
  (pr " by ")
  (link i!by (user-url i!by))
  (pr " " (text-age:item-age i) " "))

(def user-url (user) (+ "user?id=" user))

(def commentlink (i user)
  (when (cansee user i) (pr bar*))  ; smells like a hack
  (tag (a href (item-url i))
    (let n (- (len (visible user (family i))) 1)
      (if (> n 0)
          (pr n (plural n " comment"))
          (pr "discuss")))))

(def family (i) (cons i (mappend family:item i!kids)))

(= user-changetime* 120 editor-changetime* 1440)

(= everchange* (table) noedit* (table))

(def canedit (user i)
  (or (admin user)
      (and (~noedit* i!type)
           (editor user) 
           (< (item-age i) editor-changetime*))
      (own-changeable-item user i)))

(def own-changeable-item (user i)
  (and (is user i!by)
       (no i!deleted)
       (or (everchange* i!type)
           (< (item-age i) user-changetime*))))

(def editlink (story user)
  (when (canedit user story)
    (pr bar*)
    (link "edit" (edit-url story))))

(def candelete (user story)
  (or (admin user) (own-changeable-item user story)))

(def deletelink (i user whence)
  (when (candelete user i)
    (pr bar*)
    (linkf (if i!deleted "undelete" "delete") (req)
      (let user (get-user req)
        (if (candelete user i)
            (del-confirm-page user i whence)
            (prn "You can't delete that."))))))

; Undeleting stories could cause a slight inconsistency. If a story
; linking to x gets deleted, another submission can take its place in
; url->story.  If the original is then undeleted, there will be two 
; stories with equal claim to be in url->story.  (The more recent will
; win because it happens to get loaded later.)  Not a big problem.

(def del-confirm-page (user i whence)
  (minipage "Confirm"
    (tab 
      (display-item nil
                    i user
                    ; never used so not testable but think correct
                    (flink [del-confirm-page (get-user _) i whence]))
      (spacerow 20)
      (tr (td)
          (td (urform user req
                      (do (when (candelete user i)
                            (= i!deleted (is (arg req "b") "Yes"))
                            (save-item i))
                          whence)
                 (prn "Do you want this to "
                      (if i!deleted "remain" "be")
                      " deleted?")
                 (br2)
                 (but "Yes" "b") (sp) (but "No" "b")))))))

(def permalink (story user)
  (when (cansee user story)
    (pr bar*) 
    (link "link" (item-url story))))

(def logvote (user story)
  (newslog user 'vote (story 'id) (list (story 'title))))

(def text-age (a)
  (tostring
    (if (>= a 1440) (let n (trunc (/ a 1440))
                      (pr n (plural n " day")    " ago"))
        (>= a   60) (let n (trunc (/ a 60))
                      (pr n (plural n " hour")   " ago"))
                    (let n (trunc a)
                      (pr n (plural n " minute") " ago")))))


; Voting

; A user needs legit-threshold karma for a vote to count if there has 
; already been a vote from the same IP address.  A new account below both
; new- thresholds won't affect rankings, though such votes still affect 
; scores unless not a legit-user.

(= legit-threshold* 0 new-age-threshold* 0 new-karma-threshold* 0)

(def legit-user (user) 
  (or (editor user)
      (> (karma user) legit-threshold*)))

(def possible-sockpuppet (user)
  (or (uvar user ignore)
      (< (uvar user weight) .5)
      (and (< (user-age user) new-age-threshold*)
           (< (karma user) new-karma-threshold*))))

(= recent-votes* nil)

; Note: if vote-for by one user changes (s 'score) while s is being
; edited by another, the save after the edit will overwrite the change.
; Actual votes can't be lost because that field is not editable.  Not a
; big enough problem to drag in locking.

(def vote-for (user i (o dir 'up))
  (unless ((votes* user) i!id)
    (atwiths (ip   (logins* user)
              vote (list (seconds) ip user dir i!score))
      (unless (or (and (uvar user ignore)
                       (isnt user i!by))
                  ; prevention of karma-bombing
                  (and (is dir 'down) 
                       (or (and (~editor user) (just-downvoted user i!by))
                           (uvar user nodowns)))
                  (and (no (legit-user user))
                       (find [is (cadr _) ip] i!votes)))
        (case dir up   (++ i!score)
                  down (-- i!score))
        ; canvote protects against sockpuppet downvote of comments 
        (when (and (is dir 'up) (possible-sockpuppet user))
          (++ i!sockvotes))
        (if (storylike i) (adjust-rank i))
        ; get equal karma for comments
        (unless (or (is user i!by)
                    (and (is ip i!ip) (~editor user)))
          (case dir up   (++ (karma i!by)) 
                    down (-- (karma i!by)))
          (save-prof i!by)))
      (push vote i!votes)
      (push (cons i!id vote) recent-votes*)
      (save-item i)
      (= ((votes* user) i!id) vote)
      (save-votes user))))

(def just-downvoted (user victim (o n 3))
  (let prev (firstn n (recent-votes-by user))
    (and (is (len prev) n)
         (all (fn ((id sec ip voter dir score))
                (and (is ((item id) 'by) victim) (is dir 'down)))
              prev))))

; Ugly to pluck out fourth element.  Should read votes into a vote
; template.  They're stored slightly differently in two diff places: 
; in one with the voter in the car and the other without.

(def recent-votes-by (user)
  (keep [is (_ 3) user] recent-votes*))


; Story Submission

(newsop submit ()
  (if user 
      (submit-page user "" "" t) 
      (submit-login-warning "" "" t)))

(def submit-login-warning ((o url) (o title) (o showtext) (o text))
  (login-page 'both "You have to be logged in to submit."
              (fn (user ip) 
                (ensure-news-user user)
                (newslog user 'submit-login ip)
                (submit-page user url title showtext text))))

(def submit-page (user (o url) (o title) (o showtext) (o text "") (o msg))
  (minipage "Submit"
    (pagemessage msg)
    (urform user req
            (process-story (get-user req)
                           (striptags (arg req "u")) 
                           (striptags (arg req "t"))
                           showtext
                           (and showtext (md-from-form (arg req "x") t))
                           (req 'ip))
      (tab
        (row "title"  (input "t" title 50))
        (if prefer-url*
            (do (row "url" (input "u" url 50))
                (when showtext
                  (row "" "<b>or</b>")
                  (row "text" (textarea "x" 4 50 (only.pr text)))))
            (do (row "text" (textarea "x" 4 50 (only.pr text)))
                (row "" "<b>or</b>")
                (row "url" (input "u" url 50))))
        (row "" (submit))
        (spacerow 20)
        (row "" submit-instructions*)))))
      
(= submit-instructions*
   "Leave url blank to submit a question for discussion. If there is 
    no url, the text (if any) will appear at the top of the comments 
    page. If there is a url, the text will be ignored.")

; For use by outside code like bookmarklet.
; http://news.domain.com/submitlink?u=http://foo.com&t=Foo
; Added a confirm step to avoid xss hacks.

(newsop submitlink (u t)
  (if user
      (submit-page user u t)
      (submit-login-warning u t)))

(= title-limit* 100
   retry*     "Please try again."
   toolong*   (string "Please make title < " title-limit* " characters.")
   bothblank* "The url and text fields can't both be blank.  Please
               either supply a url, or if you're asking a question,
               put it in the text field.")

(def process-story (user url title showtext text ip)
  (aif (and (~blank url) (live-story-w/url url))
       (do (vote-for user it)
           (item-url it))
       (if (no user)
            (flink [submit-login-warning url title showtext text])
           (no (and (or (blank url) (valid-url url)) 
                    (~blank title)))
            (flink [submit-page user url title showtext text retry*])
           (len> title title-limit*)
            (flink [submit-page user url title showtext text toolong*])
           (and (blank url) (blank text))
            (flink [submit-page user url title showtext text bothblank*])
           (atlet s (create-story url (scrubtitle title) text user ip)
             (ban-test user s ip url url-kill* url-ignore*)
             (when (uvar user ignore) (kill s))
             (push s!id (uvar user submitted))
             (save-prof user)
             (vote-for user s)
             "newest"))))

(= scrubrules* '(("Breaking: " "") ("Exclusive: " "")))

; Note that by deliberate tricks, someone could thus submit a story 
; with a blank title.

(def scrubtitle (str) (multisubst scrubrules* str))

(def live-story-w/url (url) 
  (aand (url->story* url) (check it live)))

; Kill means stuff with this substring gets killed. Ignore is stronger,
; means that user will be auto-ignored.  Eventually this info should
; be stored on disk and not in the source code.

(= url-kill*        nil
   url-ignore*      '("internetisseriousbusiness")
   comment-kill*    nil
   comment-ignore*  '("http://internetisseriousbusiness"
                      "http://www.internetisseriousbusiness")
   ip-ban*          nil)

; Kill submissions from banned ips, but don't auto-ignore users from
; them, because eventually ips will become legit again.

; Note that ban tests are only applied when a link or comment is
; submitted, not each time it's edited.  This will do for now.

(def ban-test (user i ip string kill-list ignore-list)
  (when (some [posmatch _ string] ignore-list)
    (assert (uvar user ignore))
    (save-prof user))
  (when (or (mem ip ip-ban*) (some [posmatch _ string] kill-list))
    (kill i)))

(def killallby (user) (map kill (submissions user)))

; Only called from repl.

(def kill-whole-thread (c)
  (kill c)
  (map kill-whole-thread:item c!kids))

; Could be stricter.

(def valid-url (url)
  (and (len> url 10) 
       (begins url "http://")
       (~find [in _ #\< #\> #\"] url)))

(def parse-site (url)
  (rev (tokens (cadr (tokens url [in _ #\/ #\?])) #\.)))

(defmemo sitename (url)
  (let toks (parse-site url)
    (if (isa (saferead (car toks)) 'int)
        (tostring (prall toks "" "."))
        (let (t1 t2 t3 . rest) toks  
          (if (or (mem t1 multi-tld-countries*) 
                  (and t3 (mem t2 long-domains*)))
              (string t3 "." t2 "." t1)
              (string t2 "." t1))))))

; Minor bug: can have both google.at and google.co.at.  Same for jp.

(= multi-tld-countries* '("uk" "jp" "au" "in" "ph" "tr" "za" "my" "nz" "br" 
                          "mx" "th" "sg" "id" "pk" "eg" "il" "at"))

(= long-domains* '("blogspot" "wordpress" "livejournal" "blogs" "typepad" 
                   "weebly" "blog-city" "com"))

(def create-story (url title text user ip)
  (newslog user 'create url (list title))
  (let s (inst 'item 'type 'story 'id (new-item-id) 
                     'url url 'title title 'text text 'by user 'ip ip)
    (save-item s)
    (= (items* s!id) s (url->story* url) s)
    (push s stories*)
    s))


; Individual Item Page (= Comments Page of Stories)

(def item-url (story) (string "item?id=" (story 'id)))

(newsop item (id)
  (let s (safe-item id)
    (if (news-type s)
        (item-page user s)
        (pr "No such item."))))

(def news-type (s) 
  (and s (or (storylike s) (acomment s))))

(def item-page (user i)
  (with (title (and (cansee user i)
                    (or i!title (aand i!text (ellipsize (striptags it)))))
         here (item-url i))
    (shortpage user nil title here
      (tab (display-item nil i user here)
           (display-item-text i user)
           (when (and (cansee user i) (live i) (commentable i))
             (spacerow 10)
             (row "" (comment-form i user here))))
      (br2) 
      (when (and i!kids (commentable i))
        (tab (display-subcomments i user here))))))

(def commentable (i) (in i!type 'comment 'story))

(= displayfn* (table))

(= (displayfn* 'story)   (fn (n i user here inlist)
                           (display-story n i user here)))

(= (displayfn* 'comment) (fn (n i user here inlist)
                           (display-comment n i user here nil 0 nil inlist)))

(def display-item (n i user here (o inlist))
  ((displayfn* (i 'type)) n i user here inlist))

(def superparent (i)
  (aif i!parent (superparent:item it) i))

(def display-item-text (s user)
  (when (and (cansee user s) (astory s) (blank s!url) (~blank s!text))
    (spacerow 2)
    (row "" s!text)))


; Edit Item

(def edit-url (story) (string "edit?id=" (story 'id)))

(newsop edit (id)
  (let i (safe-item id)
    (if (and i 
             (cansee user i)
             (editable-type i)
             (or (news-type i) (admin user) (is user i!by)))
        (edit-page user i)
        (pr "No such item."))))

(def editable-type (i) (fieldfn* i!type))

(= fieldfn* (table))

(= (fieldfn* 'story)
   (fn (user s)
     (with (a (admin user)  e (editor user)  x (canedit user s))
       `((string1 title     ,(s 'title)        t ,x)
         (url     url       ,(s 'url)          t ,e)
         (mdtext2 text      ,(s 'text)         t ,x)
         (int     votes     ,(len (s 'votes)) ,a  nil)
         (int     score     ,(s 'score)        t ,a)
         (int     sockvotes ,(s 'sockvotes)   ,e ,a)
         (yesno   dead      ,(s 'dead)        ,e ,e)
         (yesno   deleted   ,(s 'deleted)     ,a ,a)
         (string  ip        ,(s 'ip)          ,e  nil)))))

(= (fieldfn* 'comment)
   (fn (user s)
     (with (a (admin user)  e (editor user)  x (canedit user s))
       `((mdtext  text      ,(s 'text)         t ,x)
         (int     score     ,(s 'score)        t ,a)
         (int     sockvotes ,(s 'sockvotes)   ,e ,a)
         (yesno   dead      ,(s 'dead)        ,e ,e)
         (yesno   deleted   ,(s 'deleted)     ,a ,a)
         (string  ip        ,(s 'ip)          ,e  nil)))))

; Should check valid-url etc here too.  In fact make a fn that
; does everything that has to happen after submitting a story,
; and call it both there and here.

(def edit-page (user s)
  (let here (edit-url s)
    (shortpage user nil "Edit" here
      (tab (display-item nil s user here)
           (display-item-text s user))
      (br2)
      (vars-form user
                 ((fieldfn* s!type) user s)
                 (fn (name val) (= (s name) val))
                 (fn () (save-item s)
                        (if (storylike s) (adjust-rank s))
                        (edit-page user s)))
      (hook 'edit user s))))

 
; Comment Submission

(def comment-login-warning (parent whence (o text))
  (login-page 'both "You have to be logged in to comment."
              (fn (u ip)
                (ensure-news-user u)
                (newslog u 'comment-login ip)
                (addcomment-page parent u whence text))))

(def addcomment-page (parent user whence (o text) (o msg))
  (minipage "Add Comment"
    (pagemessage msg)
    (tab
      (let here (flink [addcomment-page parent (get-user _) whence text msg])
        (display-item nil parent user here))
      (spacerow 10)
      (row "" (comment-form parent user whence text)))))

(def comment-form (parent user whence (o text))
  (urform user req 
          (process-comment (get-user req) parent (arg req "text") (req 'ip) whence)
    (textarea "text" 6 60 
      (aif text (prn (unmarkdown it))))
    (br2)
    (submit (if (astory parent) "add comment" "reply"))))

; Have to remove #\returns because a form gives you back "a\r\nb"
; instead of just "a\nb".   Maybe should just remove returns from
; the vals coming in from any form, e.g. in aform.

(def process-comment (user parent text ip whence)
  (if (no user)
       (flink [comment-login-warning parent whence text])
      (empty text)
       (flink [addcomment-page parent (get-user _) whence text retry*])
       (atlet c (create-comment parent (md-from-form text) user ip)
         (ban-test user c ip text comment-kill* comment-ignore*)
         (when (uvar user ignore) (kill c))
         (push c!id (uvar user submitted))
         (save-prof user)
         (vote-for user c)
         whence)))

(def create-comment (parent text user ip)
  (newslog user 'comment (parent 'id))
  (let c (inst 'item 'type 'comment 'id (new-item-id)
                     'text text 'parent parent!id 'by user 'ip ip)
    (save-item c)
    (= (items* c!id) c)
    (push c!id parent!kids)
    (save-item parent)
    (push c comments*)
    c))


; Comment Display

(def display-comment-tree (c user whence (o indent 0) (o initialpar))
  (when (cansee-descendant user c)
    (display-1comment c user whence indent initialpar)
    (display-subcomments c user whence (+ indent 40))))

(def display-1comment (c user whence indent showpar)
  (row (tab (display-comment nil c user whence t indent showpar showpar))))

(def display-subcomments (c user whence (o indent 0))
  (= c!kids (sort (compare > [frontpage-rank (item _)])
                  c!kids))
  (each k c!kids
    (display-comment-tree (item k) user whence indent)))

(def display-comment (n c user whence (o astree) (o indent 0) 
                                      (o showpar) (o showon))
  (tr (display-item-number n)
      (when astree (td (hspace indent)))
      (tag (td valign 'top)
        (votelinks c user whence t))
      (tag (td class 'default)
        (let parent (and (or (no astree) showpar) (c 'parent))
          (spanclass comhead
            (itemline c user)
            (permalink c user)
            (when parent
              (when (cansee user c) (pr bar*))
              (link "parent" (item-url (item parent))))
            (editlink c user)
            (deletelink c user whence)
            (deadmark c user)
            (when showon
              (pr " | on: ")
              (let s (superparent c)
                (link (ellipsize s!title 50) 
                      (if (empty s!url) (item-url s) s!url)))))
          (when (or parent (cansee user c))
            (br) (vspace 20))
          (spanclass comment
            (if (no (cansee user c))
                 (pr (pseudo-text c))
                (and (no (live c)) (isnt user c!by))
                 (spanclass dead (pr c!text))
                 (tag (font color (comment-color c))
                   (pr c!text))))
          (when (and astree (cansee user c) (live c))
            (para)
            (tag (font size 1)
              (underline (replylink c user whence))))))))

(def replylink (i user whence (o title 'reply))
  (linkf title (req)
    (let user (get-user req)
      (if user
          (addcomment-page i user whence)
          (login-page 'both "You have to be logged in to comment."
                      (fn (u ip)
                        (ensure-news-user u)
                        (newslog u 'comment-login ip)
                        (addcomment-page i u whence)))))))

(def comment-color (c)
  (let s (realscore c)
    (if (> s 0)  black
        (< s -2) (gray 150)
                 (case s -2 (gray 130) -1 (gray  90) 0 (gray  50)))))


; Threads

(def threads-url (user) (string "threads?id=" user))

(newsop threads (id) (threads-page user id))

(def threads-page (user subject)
  (if (profs* subject)
      (withs (title (string subject "'s comments")
              label (if (is user subject) "threads" title)
              here  (threads-url subject))
        (longpage user (msec) label title here
          (awhen (keep [and (cansee user _) (no (subcomment _))]
                       (comments subject perpage*))
            (tab (each c it
                   (display-comment-tree c user here 0 t))))))
      (prn "No such user.")))

(def submissions (user (o limit)) 
  (map item (firstn limit (uvar user submitted))))

(def comments (user (o limit)) 
  ((afn (ids count)
     (if (or (no ids) (is count limit))
         nil
         (let c (item (car ids))
           (consif (and (acomment c) c)
                   (self (cdr ids) 
                         (+ count (if (acomment c) 1 0)))))))
   (uvar user submitted) 0))
     
(def subcomment (c)
  (some [and (acomment _) (is _!by c!by) (no _!deleted)]
        (ancestors c)))

(def ancestors (i)
  (accum a (trav i!parent
                 a:item
                 [self ((item _) 'parent)])))


; Submitted

(def submitted-url (user) (string "submitted?id=" user))
       
(newsop submitted (id) (submitted-page user id))

(def submitted-page (user subject)
  (if (profs* subject)
      (with (label (string subject "'s submissions")
             here  (submitted-url subject))
        (longpage user (msec) label label here
          (if (or (no (uvar subject ignore))
                  (is user subject)
                  (seesdead user))
              (aif (keep [and (astory _) (cansee user _)]
                         (submissions subject))
                   (display-items user it label label here 0 perpage* t)))))
      (pr "No such user.")))


; RSS

(newsop rss () (rsspage nil))

(newscache rsspage user 90 
  (rss-stories (firstn 25 ranked-stories*)))

(def rss-stories (stories)
  (tag (rss version "2.0")
    (tag channel
      (tag title (pr this-site*))
      (tag link (pr site-url*))
      (tag description (pr site-desc*))
      (each s stories
        (tag item
          (let comurl (+ site-url* (item-url s))
            (tag title    (pr (eschtml s!title)))
            (tag link     (pr (if (blank s!url) comurl (eschtml s!url))))
            (tag comments (pr comurl))
            (tag description
              (cdata (link "Comments" comurl)))))))))


; User Stats

(newsop leaders () (leaderspage user))

(= nleaders* 20)

(newscache leaderspage user 180
  (longpage user (msec) "leaders" "Leaders" "leaders"
    (zerotable
      (let i 0
        (each u (firstn nleaders* (leading-users))
          (tr (tdright (pr (++ i) "."))
              (td (hspace 7))
              (td (underlink u (user-url u)))
              (td (hspace 10))
              (tdright (pr (karma u))))
          (if (is i 10) (spacerow 30)))))))

(def leading-users ()
  (sort (compare > [karma _])
        (users [and (> (karma _) 1) (~admin _)])))

(adop editors ()
  (tab (each u (users [is (uvar _ auth) 1])
         (row (link u (user-url u))))))


; Comment Analysis

; Instead of a separate active op, should probably display this info 
; implicitly by e.g. changing color of commentlink or by showing the 
; no of comments since that user last looked.

(newsop active () (active-page user))

(newscache active-page user 90
  (listpage user (msec) (actives user) "active" "Active Threads" "active" t))

(def actives (user (o n maxend*) (o consider 2000))
  (visible user (rank-stories n consider (memo active-rank))))

(= active-threshold* 1500)

(def active-rank (s)
  (apply + (map [max 0 (- active-threshold* (item-age _))]
                (cdr (family s)))))


(newsop newcomments () (newcomments-page user))

(newscache newcomments-page user 60
  (listpage user (msec) (visible user (firstn maxend* comments*))
            "comments" "New Comments" "newcomments"))


; Doc

(defop formatdoc req
  (minipage "Formatting Options"
    (spanclass admin
      (center (widtable 500 formatdoc*)))))

(= formatdoc-url* "formatdoc")

(= formatdoc* 
"Blank lines separate paragraphs.
<p> Text after a blank line that is indented by two or more spaces is 
reproduced verbatim.  (This is intended for code.)
<p> Text surrounded by asterisks is italicized, if the character after the 
first asterisk isn't whitespace.
<p> Urls become links, except in the text field of a submission.<br><br>")


; Noprocrast

(def check-procrast (user)
  (or (no user)
      (no (uvar user noprocrast))
      (let now (seconds)
        (when (no (uvar user firstview))
          (reset-procrast user))
        (or (when (< (/ (- now (uvar user firstview)) 60)
                     (uvar user maxvisit))
              (= (uvar user lastview) now)
              (save-prof user)
              t)
            (when (> (/ (- now (uvar user lastview)) 60)
                     (uvar user minaway))
              (reset-procrast user)
              t)))))
                
(def reset-procrast (user)
  (= (uvar user lastview) (= (uvar user firstview) (seconds)))
  (save-prof user))

(def procrast-msg (user whence)
  (let m (+ 1 (trunc (- (uvar user minaway)
                        (/ (since (uvar user lastview)) 60))))
    (pr "<b>Get back to work!</b>")
    (para "Sorry, you can't see this page.  Based on the anti-procrastination
           parameters you set in your profile, you'll be able to use the site 
           again in " m (plural m " minute") ".")
    (para "(If you got this message after submitting something, don't worry,
           the submission was processed.)")
    (para "To change your anti-procrastination settings, go to your profile 
           by clicking on your username.  If <tt>noprocrast</tt> is set to 
           <tt>yes</tt>, you'll be limited to sessions of <tt>maxvisit</tt>
           minutes, with <tt>minaway</tt> minutes between them.")
    (para)
    (w/rlink whence (underline (pr "retry")))
    (hspace 20)
    (w/rlink (do (reset-procrast user) whence)
      (underline (pr "override")))
    (br2)))


; Reset PW

(defopg resetpw req (resetpw-page (get-user req)))

(def resetpw-page (user (o msg))
  (minipage "Reset Password"
    (if msg
         (pr msg)
        ((orf no blank) (uvar user email))
         (do (pr "Before you do this, please add your email address to your ")
             (underlink "profile" (user-url user))
             (pr ". Otherwise you could lose your account if you mistype 
                  your new password.")))
    (br2)
    (uform user req (try-resetpw user (arg req "p"))
      (single-input "New password: " 'p 20 "reset" t))))

(def try-resetpw (user newpw)
  (if (len< newpw 4)
      (resetpw-page user "Passwords should be a least 4 characters long.  
                          Please choose another.")
      (do (set-pw user newpw)
          (newspage user))))


; Abuse Analysis

(adop badips ()
  (let (bads goods) (badips)
    (tab
      (row "IP" "Dead" "Live")
      (each ip (sort (compare > (memo [len (bads _)]))
                     (rem [len< (bads _) 3] (keys bads)))
        (tr (td ip)
            (tdright
              (w/link (listpage user (msec) (bads ip)
                                (string "dead from " ip) nil "badips" t)
                (pr (len (bads ip)))))
            (tdright
              (w/link (listpage user (msec) (goods ip)
                                (string "live from " ip) nil "badips" t)
                (pr (len (goods ip))))))))))

; Sort by time, instead of putting stories before comments?

(def badips ()
  (with (bads (table) goods (table))
    (each s (+ stories* comments*)
      (if (s 'dead)
          (push s (bads  (s 'ip)))
          (push s (goods (s 'ip)))))
    (ontable k v bads  (zap rev (bads  k)))
    (ontable k v goods (zap rev (goods k)))
    (list bads goods)))

(adop killed ()
  (let deads (fn (items) (firstn maxend* (keep [_ 'dead] items)))
    (display-items user (deads stories*) nil nil "killed")
    (vspace 35)
    (color-stripe textgray)
    (vspace 35)
    (display-items user (deads comments*) nil nil "killed")))

(adop badguys ()
  (sptab (each user (sort (compare > [uvar _ created])
                          (users [uvar _ ignore]))
           (row (link user (user-url user))))))

(adop badlogins ()  (logins-page bad-logins*))

(adop goodlogins () (logins-page good-logins*))

(def logins-page (source)
  (sptab (each (time ip user) (firstn 100 (rev (qlist source)))
           (row time ip user))))


; Stats

(adop optimes ()
  (sptab (each name (sort < newsop-names*)
           (tr (td name)
               (td (hspace 10))
               (tdright (pr (aand (qlist (optimes* name))
                                  (num (avg it) 2 t))))))))

(defop topcolors req
  (minipage "Custom Colors"
    (tab 
      (each c (dedup (map downcase (trues [uvar _ topcolor] (keys profs*))))
        (tr (td c) (tdcolor (hex>color c) (hspace 30)))))))


