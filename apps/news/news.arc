(= this-site*    "Anarki"
   site-url*     "http://site.example.com";your domain name
   parent-url*   "http://github.com/arclanguage/anarki"
; Page Layout

   logo-url*     "arc.png"
   site-desc*    "What this site is about."               ; for rss feed
   site-color*   (color 180 180 180)
   prefer-url*   t

   newsdir*   srvdir*
   storydir*  (+ newsdir* "story/")
   profdir*   (+ newsdir* "profile/")
   votedir*   (+ newsdir* "vote/")
   pagedir*   (+ newsdir* "page/")

; remember to set caching to 0 when testing non-logged-in

   caching*  0
   perpage* 30 
   threads-perpage* 10 
   maxend* 210

; browsers can cache static files for 7200 sec
   static-max-age* 7200

   (max-age* 'news.css) 86400
   (max-age* 'custom.css) 86400

; non static file defops which will return custom (non html) content-types
   op-ctypes* {
    rss           "text/xml" 
    rss-stories   "text/xml" 
    follow        "text/xml"
    personal-data "application/json"
}

;theme css files. Names here should match a css filename
;in the static dir (eg default.css). First element in this
;list will be used as the default
   themes* `("default" "dark")

)

; Look up title on Searx, a free metasearch engine

(def weblink (q)
  (pr bar*)
  (link "web" (+ "https://searx.me/?q=" (urlencode q))))


(def vote-url (user i dir whence)
  (+ "vote?for=" i!id
             "&dir=" dir
             (if user (+ "&by=" user "&auth=" (user->cookie* user)))
             "&whence=" (urlencode whence)))

;(declare 'direct-calls t)   ; you promise not to redefine fns as tables

;(declare 'explicit-flush t) ; you take responsibility for flushing output
                            ; (all existing news code already does)

; Structures

; enable string interpolation just in this file
(declare 'atstrings t)

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
  votes      nil   ; for now just recent, elts each (time id by sitename dir)
  karma      1
  avg        nil
  weight     .5
  ignore     nil
  email      nil
  about      nil
  showdead   nil
  noprocrast nil
  firstview  nil
  lastview   nil
  maxvisit   20
  minaway    180
  font-size  12
  theme      0
  topcolor   nil
  keys       nil
  delay      0)

(deftem item
  id         nil
  type       nil
  by         nil
  ip         nil
  time       (seconds)
  url        nil
  archive-url nil
  title      nil
  text       nil
  votes      nil   ; elts each (time ip user type score)
  score      0
  sockvotes  0
  flags      nil
  dead       nil
  deleted    nil
  parts      nil
  parent     nil
  kids       nil
  keys       nil)

(deftem page 
  id 0
  url nil
  title nil
  text nil)

(= pages* (table) pageid* 0)

; basically the blog code, but for "pages"
; https://github.com/laarc/laarc/blob/master/blog.arc

(def load-pages ()
  (= (pages*) nil)
  (each id (map int (dir pagedir*))
    (= pageid* (max pageid* id)
      (pages* id) (temload 'page (string pagedir* id)))))



;url is not a full url, but the endpoint of a defop ex: "faq"
(def save-page (url title text id)
  (let p (inst 'page 'id id 
               'url url 
               'title title 
               'text text)
    (temstore 'page p (string pagedir* id))
    (= (pages* id) p)))

(def get-page (id) 
  (pages* (errsafe:int id)))

(def page-editlink (id) 
  (link "edit" (string "/editpage?id=" id)))

(def page-createlink () 
  (link "new page" "/newpage"))

(def page-link (url id) 
  (link url (string "/page?id=" id)))

(def page-menu () 
  (each (k p) pages* 
    (do 
      (page-link p!url p!id)
      (pr " | "))))

; returns the new id 
 (def new-page (url title text)
  (let i (++ pageid*) 
    (save-page url title text i) i))

 (def edit-page (url title text id)
  (save-page url title text id))

 (def delete-page (id)
  (= (pages* (errsafe:int id)) nil) 
  (if (file-exists (string pagedir* id))
    (rmfile (string pagedir* id))))

(def page-deletelink (id)
  (link "delete" (rflink (fn (req) (delete-page id) "/"))))

; need to delay macro expansion or else
; the following error occurs: 
; Can't coerce  #s(ar-tagged mac #<procedure: minipage>) fn
; see http://arclanguage.org/item?id=18018
;     http://arclanguage.org/item?id=13306

(def show-page (user id tt tx)
  (eval `(shortpage ,user nil ,tt "/" 
    (do 
      (tag ("h4") (pr ,tt))
      
      (tag ("div") (pr (markdown (trim (rem #\return ,tx) 'both) 80)))
      
      (when (admin ,user)
        (page-editlink ,id)
        (pr " | ")
        (page-deletelink, id))))))

(defop page req 
  (iflet p (get-page (arg req "id"))
    (show-page (get-user req) p!id p!title p!text)))

(defop newpage req (when (admin (get-user req))
  (arform (fn (req) 
     (let i (new-page (arg req "url") 
                      (arg req "title") 
                      (arg req "text"))
     (string "/page?id=" i)))
     (tag table 
       (row (prn "create a page"))
       (row "url:" (input "url"))
       (row "title:" (input "title"))
       (row "text:" (textarea "text" 8 60)) 
       (row (submit "create")))))) 

(defop editpage req (when (admin (get-user req))
  (whenlet p (get-page (arg req "id"))
    (arform (fn (req) 
      (edit-page p!url (arg req "title") (arg req "text") p!id)
      (string "/page?id=" p!id))
    (tag table 
      (row (prn "edit a page"))
      (row "title:" (input "title" (prn p!title)))
      (row "text:" (textarea "text" 8 60 (prn p!text))) 
      (row (submit "edit")))))))

(defop deletepage req (when (admin (get-user req))
  (delete-page (arg req "id"))))

(= votes* (table) profs* (table))
; Load and Save
(= initload-users* nil)

(def nsv ((o port 8080))
  (map ensure-dir (list srvdir* newsdir* storydir* votedir* profdir* pagedir*))
  (unless stories* (load-items))
  (if (and initload-users* (empty profs*)) 
    (load-users))
    (load-pages)
  (asv port))

(def load-users ()
  (pr "load users: ")
  (noisy-each 100 id (dir profdir*)
    (load-user id)))

; For some reason vote files occasionally get written out in a
; broken way.  The nature of the errors (random missing or extra
; chars) suggests the bug is lower-level than anything in Arc.
; Which unfortunately means all lists written to disk are probably
; vulnerable to it, since that's all save-table does.

(def load-user (u)
  (= (votes* u) (load-table (+ votedir* u))
     (profs* u) (temload 'profile (+ profdir* u)))
  u)

; Have to check goodname because some user ids come from http requests.
; So this is like safe-item.  Don't need a sep fn there though.

(def profile (u)
  (or (profs* u)
      (aand (goodname u)
            (file-exists (+ profdir* u))
            (= (profs* u) (temload 'profile it)))))

(def votes (u)
  (or (votes* u)
      (aand (file-exists (+ votedir* u))
            (= (votes* u) (load-table it)))))

(def init-user (u)
  (= (votes* u) (table)
     (profs* u) (inst 'profile 'id u))
  (save-votes u)
  (save-prof u)
  u)

; Need this because can create users on the server (for other apps)
; without setting up places to store their state as news users.
; See the admin op in app.arc.  So all calls to login-page from the
; news app need to call this in the after-login fn.

(def ensure-news-user (u)
  (if (profile u) u (init-user u)))

(def save-votes (u) (save-table (votes* u) (+ votedir* u)))

(def save-prof  (u) (temstore 'profile (profs* u) (+ profdir* u)))

(mac uvar (u k) `((profile ,u) ',k))

(mac karma   (u) `(uvar ,u karma))
(mac ignored (u) `(uvar ,u ignore))

; Note that users will now only consider currently loaded users.

(def users ((o f idfn))
  (keep f (keys profs*)))

(def check-key (u k)
  (and u (mem k (uvar u keys))))

(def author (u i) (is u i!by))


(= stories* nil ranked-stories* nil comments* nil
   items* (table) url->story* (table)
   maxid* 0 initload* 15000)

; The dir expression yields stories in order of file creation time
; (because arc infile truncates), so could just rev the list instead of
; sorting, but sort anyway.

; Note that stories* etc only include the initloaded (i.e. recent)
; ones, plus those created since this server process started.

; Could be smarter about preloading by keeping track of popular pages.

(def load-items ()
  (each file (dir storydir*)
    (if (endmatch ".tmp" file)
      (rmfile (+ storydir* file))))
  (pr "load items: ")
  (with (items (table)
         ids   (sort > (map int (dir storydir*))))
    (if ids (= maxid* (car ids)))
    (noisy-each 100 id (firstn initload* ids)
      (let i (load-item id)
        (push i (items i!type))))
    (= stories*  (rev (merge (compare < !id) items!story items!poll))
       comments* (rev items!comment))
    (hook 'initload items))
  (ensure-topstories))

(def ensure-topstories ()
  (aif (errsafe (readfile1 (+ newsdir* "topstories")))
    (= ranked-stories* (map item it))
    (do (prn "ranking stories.")
        (flushout)
        (gen-topstories))))

(def astory   (i) (is i!type 'story))
(def acomment (i) (is i!type 'comment))
(def apoll    (i) (is i!type 'poll))

(def load-item (id)
  (let i (temload 'item (+ storydir* id))
    (= (items* id) i)
    (awhen (and (astory&live i) (check i!url ~blank))
      (register-url i it))
    i))

; Note that duplicates are only prevented of items that have at some
; point been loaded.

(def register-url (i url)
  (= (url->story* (canonical-url url)) i!id))

(= stemmable-sites* (table))

(def canonical-url (url)
  (if (stemmable-sites* (sitename url))
    (cut url 0 (pos #\? url))
    url))

(def new-item-id ()
  (evtil (++ maxid*) [~file-exists (+ storydir* _)]))

(def item (id)
  (or (items* id) (errsafe:load-item id)))

(def kids (i) (map item i!kids))

; For use on external item references (from urls).  Checks id is int
; because people try e.g. item?id=363/blank.php

(def safe-item (id)
  (ok-id&item (if (isa id 'string) (errsafe:read id) id)))

(def ok-id (id)
  (and (exact id) (<= 1 id maxid*)))

(def live (i) (nor i!dead i!deleted))

(def save-item (i) (temstore 'item i (+ storydir* i!id)))

(def kill (i how)
  (unless i!dead
    (log-kill i how)
    (wipe (comment-cache* i!id))
    (set i!dead)
    (save-item i)))

(= kill-log* nil)

(def log-kill (i how)
  (push (list i!id how) kill-log*))

(mac each-loaded-item (var . body)
  (w/uniq g
    `(let ,g nil
       (down ,g maxid* 0
         (whenlet ,var (items* ,g)
           ,@body)))))

(def loaded-items (test)
  (accum a (each-loaded-item i (test&a i))))

(def newslog args (apply srvlog 'news args))


; Ranking

; Votes divided by the age in hours to the gravityth power.
; Would be interesting to scale gravity in a slider.

(= gravity* 1.8 timebase* 120 front-threshold* 1
   nourl-factor* .4 lightweight-factor* .3 )

(def frontpage-rank (s (o scorefn realscore) (o gravity gravity*))
  (* (/ (let base (- (scorefn s) 1)
          (if (> base 0) (expt base .8) base))
        (expt (/ (+ (item-age s) timebase*) 60) gravity))
     (if (no (in s!type 'story 'poll))  .5
         (blank s!url)                  nourl-factor*
         (lightweight s)                (min lightweight-factor*
                                             (contro-factor s))
                                        (contro-factor s))))

(def contro-factor (s)
  (aif (check (visible-family nil s) [> _ 20])
    (min 1 (expt (/ (realscore s) it) 2))
    1))

(def realscore (i) (- i!score i!sockvotes))

(disktable lightweights* (+ newsdir* "lightweights"))

(def lightweight (s)
  (or s!dead
      (mem 'rally s!keys)  ; title is a rallying cry
      (mem 'image s!keys)  ; post is mainly image(s)
      (lightweights* (sitename s!url))
      (lightweight-url s!url)))

(defmemo lightweight-url (url)
  (in (downcase (last (tokens url #\.))) "png" "jpg" "jpeg"))

(def item-age (i) (minutes-since i!time))

(def user-age (u) (minutes-since (uvar u created)))

; Only looks at the 1000 most recent stories, which might one day be a
; problem if there is massive spam.

(def gen-topstories ()
  (= ranked-stories* (rank-stories 180 1000 (memo frontpage-rank))))

(def save-topstories ()
  (save-file (map !id (firstn 180 ranked-stories*))
             (+ newsdir* "topstories")))

(def rank-stories (n consider scorefn)
  (bestn n (compare > scorefn) (latest-items metastory nil consider)))

; With virtual lists the above call to latest-items could be simply:
; (map item (retrieve consider metastory:item (gen maxid* [- _ 1])))

(def latest-items (test (o stop) (o n))
  (accum a
    (catch
      (down id maxid* 0
        (let i (item id)
          (if (or (and stop (stop i)) (and n (<= n 0)))
            (throw))
          (when (test i)
            (a i)
            (if n (-- n))))))))

(def metastory (i) (and i (in i!type 'story 'poll)))

(def adjust-rank (s (o scorefn frontpage-rank))
  (insortnew (compare > (memo scorefn)) s ranked-stories*)
  (save-topstories))

; If something rose high then stopped getting votes, its score would
; decline but it would stay near the top.  Newly inserted stories would
; thus get stuck in front of it. I avoid this by regularly adjusting
; the rank of a random top story.

(defbg rerank-random 30 (rerank-random))

(def rerank-random ()
  (when ranked-stories*
    (adjust-rank (ranked-stories* (rand (min 50 (len ranked-stories*)))))))

(def topstories (user n (o threshold front-threshold*))
  (retrieve n
            [and (>= (realscore _) threshold) (cansee user _)]
            ranked-stories*))

(= max-delay* 10)

(def cansee (user i)
  (if i!deleted   (admin user)
      i!dead      (or (author user i) (seesdead user))
      (delayed i) (author user i)
      t))

(let mature (table)
  (def delayed (i)
    (and (no (mature i!id))
         (acomment i)
         (or (< (item-age i) (min max-delay* (uvar i!by delay)))
             (do (set (mature i!id))
                 nil)))))

(def seesdead (user)
  (or (and user (uvar user showdead) (no (ignored user)))
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

;inline stylesheet with user-set font size and topcolor
(def userstyle (user)
  (with (font-size (aif (and user (uvar user font-size)) it 12)
         bgcolor (hexrep (main-color user)))
    (pr 
      (tag ("style" "type" "text/css")
        (string  "body { 
                    font-size:" font-size "pt!important; 
                  }
                  .topcolor { 
                     background-color: #" bgcolor 
                  "};")))))

; css link to user-specified theme or default on fail
(def usertheme (user)
   (let theme (string "/" (aif (and user (uvar user theme)) it (themes* 0)) ".css")
    (gentag link "rel" "stylesheet" "type" "text/css" "href"
      (if (file-exists (+ staticdir* theme)) theme "/default.css"))))


(= pagefns* nil)

; page top
(= toplabels* '(nil "welcome" "new" "threads" "comments" "ask" "*")
   showkarma* t)

;Other page templates have been deprecated and folded into this, which
;provides a single macro for generating HTML pages for News (thus its size.)
(mac longpage (user time label title whence . body)
  (w/uniq (gu gl gt gw gd)
    `(with (,gu ,user ,gl ,label ,gt ,title ,gw ,whence ,gd ,time) 
      (do 
       (prn "<!DOCTYPE html>")
       (tag html
         (tag head
           (gentag meta "charset" "UTF-8")
           (userstyle ,gu)
           (usertheme, gu)
           (gentag link "rel" "icon" "href" "/favicon.ico")
           (gentag "meta" "name" "viewport" "value" "width=device-width")
           (tag (script "type" "text/javascript" "src" "/news.js"))
           (tag title (pr (+ this-site* (if ,gt (+ bar* ,gt) "")))))
           (tag body 
           ;(tag (div "class" "layout sand") 
           (tag (div "class" "layout")
           (if (check-procrast ,gu) 
            (do
             (tag (div "class" "topcolor page-header")
               
               (tag (span "id" "navleft")
                 
                 (tag (link rel "icon" href logo-url*))
                 
                 (tag (a "id" "logo" href parent-url*)
                   (tag (img src logo-url*)))

                   (tag (span "id" "page-title") 
                     (link this-site* "news")))
               
               (tag (span "id" "navmain")
                 (w/bars 
                   (toplink "new" "newest" ,gl)
                   (toplink "comments" "newcomments" ,gl)
                   (toplink "ask" "ask" ,gl)
                   (link  "submit")))
                
                (tag (span "id" "navright")
                    (when ,gu
                      (if (noob ,gu)
                        (toplink "welcome " "welcome " ,gl))
                      (tag (span "id" "userlink") 
                        (userlink ,gu ,gu nil))
                      (when showkarma* 
                        (pr (string " (" (karma ,gu) ") |")))
                      (pr "&nbsp;")
                      (toplink "threads" (threads-url ,gu) ,gl)
                      (pr "&nbsp;|&nbsp;")
                      (when (and ,gu (> (karma ,gu) poll-threshold*))
                        (toplink "poll" "newpoll" ,gl)
                        (pr "&nbsp;|&nbsp;")))
                      (if ,gu
                        (rlinkf 'logout (req)
                          (when-umatch/r ,gu req
                            (logout-user ,gu)
                        ,gw))
                        (onlink "login"
                          (news-login-page nil
                                (list (fn (u ip)
                                        (ensure-news-user u)
                                        (newslog ip u 'top-login))
                                      ,gw)))))
                 (hook 'toprow ,gu ,gl))

               (map [_ ,gu] pagefns*)
               (hook 'page ,gu ,gl)

               (tag (div "class" "page-content") ,@body)

               (tag (div "class" "page-footer topcolor")
                  (hook 'longfoot)
                  (tag ("span" "class" "yclinks") 
                    (page-menu)
                    (when (admin, gu)
                      (page-createlink)
                      (pr " | "))
                    (link "rss")
                    (pr " | ")
                    (link "bookmarklet"))
                  
                  (if (bound 'search-bar) 
                    (search-bar ,gu))
                  (when (admin ,gu)
                      (br2)
                      (w/bars
                        (pr (len items*) "/" maxid* " loaded")
                        (pr (round (/ (memory) 1000000)) " mb")
                        (pr (- (msec) ,gd) " msec")
                        (link "settings" "newsadmin")
                        (link "appeditor")
                        (hook 'admin-bar ,gu ,gw)))))

             (tag (div "class" "noprocrast")
               (procrast-msg ,gu ,gw)))
)))))))

(mac shortpage (user label title whence . body)
  `(longpage ,user (msec) ,label ,title ,whence ,@body))

(mac minipage (label . body)
  `(longpage nil (msec) ,label (+ this-site* bar* ,label) "/" ,@body))

(def msgpage (user msg (o title))
  (minipage (or title "Message")
    (tag ("span" "class" "admin")
      (pr msg))))

(def listpage (user t1 items label title (o url label) (o number t))
  (hook 'listpage user)
  (longpage user t1 label title url
    (display-items user items label title url 0 perpage* number)))

(def news-login-page ((o msg nil) (o afterward hello-page))
     (minipage "Login"
      (pagemessage msg)
      (login-form afterward)
      (hook 'login-form afterward)
      (br2)
      (signup-form afterward)))

(def toplink (name dest label)
  (tag-if (is name label) (span class 'topsel)
    (link name dest)))

(def noob (user)
  (and user (< (days-since (uvar user created)) 1)))

; turn off server caching via (= caching* 0) or won't see changes

(def main-color (user)
  (aif (and user (uvar user topcolor))
    (hex>color it)
    site-color*))

; News-Specific Defop Variants

(mac defopt (name parm test msg . body)
  `(defop ,name ,parm
     (if (,test (get-user ,parm))
       (do ,@body)
       (news-login-page (+ "Please log in" ,msg ".")
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
           (newslog ip user ',name ,@parms)
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

(mac edop (name parms . body)
  (w/uniq g
    `(opexpand defope ,name ,parms
       (let ,g (string ',name)
         (shortpage user ,g ,g ,g
           ,@body)))))


; News Admin

(defopa newsadmin req
  (let user (get-user req)
    (newslog req!ip user 'newsadmin)
    (newsadmin-page user)))

; Note that caching* is reset to val in source when restart server.

(def nad-fields ()
  `((num      caching         ,caching*                       t t)
    (bigtoks  comment-kill    ,comment-kill*                  t t)
    (bigtoks  comment-ignore  ,comment-ignore*                t t)
    (bigtoks  lightweights    ,(sort < (keys lightweights*))  t t)))

; Need a util like vars-form for a collection of variables.
; Or could generalize vars-form to think of places (in the setf sense).

(def newsadmin-page (user)
  (shortpage user nil "newsadmin" "newsadmin"
    (vars-form user
               (nad-fields)
               (fn (name val)
                 (case name
                   caching            (= caching* val)
                   comment-kill       (todisk comment-kill* val)
                   comment-ignore     (todisk comment-ignore* val)
                   lightweights       (todisk lightweights* (memtable val))
                   ))
               (fn () (newsadmin-page user)))
    (br2)
    (aform (fn (req)
             (with (user (get-user req) subject (arg req "id"))
               (if (profile subject)
                 (do (killallby subject)
                     (submitted-page user subject))
                 (admin&newsadmin-page user))))
      (single-input "" 'id 20 "kill all by"))
    (br2)
    (aform (fn (req)
             (let user (get-user req)
               (set-ip-ban user (arg req "ip") t)
               (admin&newsadmin-page user)))
      (single-input "" 'ip 20 "ban ip"))))


; Users

(newsop user (id)
  (if (only.profile id)
    (user-page user id)
    (msgpage nil "No such user.")))

(def user-page (user subject)
  (let here (user-url subject)
    (shortpage user nil (+ "Profile: " subject) here
      (profile-form user subject)
      (when (some astory:item (uvar subject submitted))
        (underlink "submissions" (submitted-url subject)))
      (when (some acomment:item (uvar subject submitted))
        (sp)
        (underlink "comments" (threads-url subject)))
      (sp)
      (underlink "rss" "follow?subject=@subject")
      (hook 'user user subject))))

(def profile-form (user subject)
  (let prof (profile subject)
    (vars-form user
               (user-fields user subject)
               (fn (name val)
                 (when (and (is name 'ignore) val (no prof!ignore))
                   (log-ignore user subject 'profile))
                 (= (prof name) val))
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
          p (profile subject))
    `((string  user       ,subject                                  t   nil)
      (string  name       ,(p 'name)                               ,m  ,m)
      (string  created    ,(text-age:user-age subject)              t   nil)
      (string  password   ,(resetpw-link)                          ,w   nil)
      (string  data       ,(personal-data-link)                    ,w   nil)
      (string  saved      ,(saved-link user subject)               ,u   nil)
      (int     auth       ,(p 'auth)                               ,e  ,a)
      (yesno   member     ,(p 'member)                             ,a  ,a)
      (posint  karma      ,(p 'karma)                               t  ,a)
      (num     avg        ,(p 'avg)                                ,a  nil)
      (yesno   ignore     ,(p 'ignore)                             ,e  ,e)
      (num     weight     ,(p 'weight)                             ,a  ,a)
      (mdtext2 about      ,(p 'about)                               t  ,u)
      (string  email      ,(p 'email)                              ,u  ,u)
      (yesno   showdead   ,(p 'showdead)                           ,u  ,u)
      (yesno   noprocrast ,(p 'noprocrast)                         ,u  ,u)
      (string  firstview  ,(p 'firstview)                          ,a   nil)
      (string  lastview   ,(p 'lastview)                           ,a   nil)
      (posint  maxvisit   ,(p 'maxvisit)                           ,u  ,u)
      (posint  minaway    ,(p 'minaway)                            ,u  ,u)
      (sexpr   keys       ,(p 'keys)                               ,a  ,a)
      (hexcol  topcolor   ,(or (p 'topcolor) (hexrep site-color*)) ,k  ,k)
      (choice   theme     ,themes*                                 ,u  ,u)     
      (int font-size      ,(p 'font-size)                          ,u  ,u)
      (int     delay      ,(p 'delay)                              ,u  ,u))))

(def saved-link (user subject)
  (when (or (admin user) (is user subject))
    (let n (if (len> (votes subject) 500)
             "many"
             (len (voted-stories user subject)))
      (if (is n 0)
        ""
        (tostring (underlink n (saved-url subject)))))))

(def resetpw-link ()
  (tostring (underlink "reset password" "resetpw")))

(newsop welcome ()
  (msgpage user (string "Welcome to " this-site* ", " user "!")))


; Main Operators

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
  (listpage user (msec) (topstories user maxend*) nil nil "news"))

(newsop newest () (newestpage user))

; Note: dead/deleted items will persist for the remaining life of the
; cached page.  If this were a prob, could make deletion clear caches.

(newscache newestpage user 40
  (listpage user (msec) (newstories user maxend*) "new" "New Links" "newest"))

(def newstories (user n)
  (retrieve n [cansee user _] stories*))

(newsop best () (bestpage user))

(newscache bestpage user 1000
  (listpage user (msec) (beststories user maxend*) "best" "Top Links"))

; As no of stories gets huge, could test visibility in fn sent to best.

(def beststories (user n)
  (bestn n (compare > realscore) (visible user stories*)))

(newsop noobstories () (noobspage user stories*))
(newsop noobcomments () (noobspage user comments*))

(def noobspage (user source)
  (listpage user (msec) (noobs user maxend* source) "noobs" "New Accounts"))

(def noobs (user n source)
  (retrieve n [and (cansee user _) (bynoob _)] source))

(def bynoob (i)
  (< (- (user-age i!by) (item-age i)) 2880))


(newsop bestcomments () (bestcpage user))

(newscache bestcpage user 1000
  (listpage user (msec) (bestcomments user maxend*)
            "best comments" "Best Comments" "bestcomments" nil))

(def bestcomments (user n)
  (bestn n (compare > realscore) (visible user comments*)))

(def saved-url (user) (+ "saved?id=" user))

(newsop saved (id)
  (if (only.profile id)
    (savedpage user id)
    (pr "No such user.")))

(def savedpage (user subject)
  (if (or (is user subject) (admin user))
    (listpage user (msec)
              (sort (compare < item-age) (voted-stories user subject))
             "saved" "Saved Links" (saved-url subject))
    (pr "Can't display that.")))

(def voted-stories (user subject)
  (keep [and (astory _) (cansee user _)]
        (map item (keys:votes subject))))


; Story Display
; TODO: only display list numbers for newest items page
(def display-items (user items label title whence
                    (o start 0) (o end perpage*) (o number))
    (tag (ol "class" "items-list") 
    (let n start
      (each i (cut items start end)
        (tag li (display-item (and number (++ n)) i user whence t))))
    (when end
      (let newend (+ end perpage*)
        (when (and (<= newend maxend*) (< end (len items)))
          (tag li 
            (morelink display-items
              items label title end newend number)))))))

; This code is inevitably complex because the More fn needs to know
; its own fnid in order to supply a correct whence arg to stuff on
; the page it generates, like logout and delete links.

(def morelink (f items label title . args)
  (tag (a href
          (url-for
            (afnid (fn (req)
                     (prrn)
                     (with (url  (url-for it)     ; it bound by afnid
                            user (get-user req))
                       (newslog req!ip user 'more label)
                       (longpage user (msec) label title url
                         (apply f user items label title url args))))))
          rel 'nofollow)
    (pr "More")))

(def display-story (i s user whence)
  (when (or (cansee user s) (s 'kids))
    (tag (div "class" "itemhead") 
        (votelinks s user whence)
        (titleline s s!url user whence))
        (tag (div "class" "subtext")
          (hook 'itemline s user)
          (itemline s user)
          (when (isnt whence "news") (weblink s!title))
          (when (in s!type 'story 'poll) (commentlink s user))
          (editlink s user)
          (when (apoll s) (addoptlink s user))
          (unless i (flaglink s user whence))
          (locklink s user whence)
          (killlink s user whence)
          (blastlink s user whence)
          (blastlink s user whence t)
          (deletelink s user whence))))

(= follow-threshold* 5)

(def titleline (s url user whence)
  (tag (span "class" "comtitle")
    (if (cansee user s)
      (do (deadmark s user)
          (titlelink s url user)
          (awhen (sitename url)
            (tag (span "class" "comhead")
              (pr " ("
                  (tostring (link it (string "from?site=" (sitename url))))
                  ") ")
                  ; TODO? don't show if archive is unavailable
                  (ia-archivelink s whence)
            )))
      (pr (pseudo-text s)))))

; site archival

; check the Internet Archive for the item (s) url, if it exists, store the data locally
; and return whence, otherwise return a link to IA to archive the url. 
(def set-ia-archive (s whence (o port stdin))
  (let arch (fromstring ((mkreq (string "https://archive.org/wayback/available?url=" s!url)) 1) (read-json (port)))
    (if (len> arch!archived_snapshots 0)
      (do
        (= (s "archive-url") arch!archived_snapshots!closest!url
           (items* s!id)     s)
         whence)
      (string "https://web.archive.org/save/" s!url))))

; display the archival link
(def ia-archivelink (s whence)
  (if (s "archive-url")
    (tag ("a" "href" (s "archive-url")) (pr "archived"))
    (tag ("a" "href" (rflink (fn (req)
      (set-ia-archive s whence)))) 
      (pr "archive"))))

(def titlelink (s url user)
  (let toself (blank url)
    (tag (a href (if toself
                      (item-url s!id)
                     (or (live s) (author user s) (editor user))
                      url)
            rel  (unless (or toself (> (realscore s) follow-threshold*))
                   'nofollow))
      (pr s!title))))

(def pseudo-text (i)
  (if i!deleted "[deleted]" "[dead]"))

(def deadmark (i user)
  (when (and i!dead (seesdead user))
    (pr " [dead] "))
  (when (and i!deleted (admin user))
    (pr " [deleted] ")))

(= downvote-threshold* 200 
   downvote-time* 1440
   votewid* 14
   uparrow-black "&#9650;"
   uparrow-white "&#9651;"
   downarrow-black "&#9660;"
   downarrow-white "&#9660;")

(def votelinks (i user whence (o downtoo))
  (tag (span "class" "votelinks")
    (if (and (cansee user i)
             (or (no user)
                 (no ((votes user) i!id))))
         (do (votelink-up i user whence)
             (when (and downtoo
                        (or (admin user)
                            (< (item-age i) downvote-time*))
                        (canvote user i 'down))
               (votelink-down i user whence)))
        ;(author user i) (do (fontcolor orange (pr "*"))))
)))

; could memoize votelink more, esp for non-logged in users,
; since only uparrow is shown; could straight memoize

(def votelink-up (i user whence)
  (tag (a "class" "votelink uplink" "data-id" (string i!id) "alt" "upvote" "href" (vote-url user i 'up whence))
    (pr uparrow-black)))

(def votelink-down (i user whence)
  (tag (a "class" "votelink downlink" "data-id" (string i!id) "alt" "downvote" "href" (vote-url user i 'up whence))
    (pr downarrow-black)))

(= lowest-score* -4)

; Not much stricter than whether to generate the arrow.  Further tests
; applied in vote-for.

(def canvote (user i dir)
  (and user
       (news-type&live i)
       (or (is dir 'up) (> i!score lowest-score*))
       (no ((votes user) i!id))
       (or (is dir 'up)
           (and (acomment i)
                (> (karma user) downvote-threshold*)
                (no (aand i!parent (author user (item it))))))))

; Need the by argument or someone could trick logged in users into
; voting something up by clicking on a link.  But a bad guy doesn't
; know how to generate an auth arg that matches each user's cookie.

(newsop vote (by for dir auth whence)
  (with (i      (safe-item for)
         dir    (errsafe:read dir)
         whence (if whence (urldecode whence) "news"))
    (if (no i)
         (pr "No such item.")
        (no (in dir 'up 'down))
         (pr "Can't make that vote.")
        (and by (or (isnt by user) (isnt (sym auth) (user->cookie* user))))
         (pr "User mismatch.")
        (no user)
         (news-login-page "You have to be logged in to vote."
                     (list (fn (u ip)
                             (ensure-news-user u)
                             (newslog ip u 'vote-login)
                             (when (canvote u i dir)
                               (vote-for u i dir)
                               (logvote ip u i)))
                           whence))
        (canvote user i dir)
         (do (vote-for by i dir)
             (logvote ip by i)
             ; Redirect w/o JavaScript
             (pr (redirect whence 1))
             (pr "Thank you for voting."))
         (pr "Can't make that vote."))))

(def itemline (i user)
  (when (cansee user i)
    (when
      (or (astory i)
          (apoll i)
          (author user i))
      (itemscore i user))
    (byline i user)))

(def itemscore (i (o user))
  (tag (span "class" "itemscore" 
             "data-score" (string i!score)
             id (+ "score_" i!id))
    (pr (plural (if (is i!type 'pollopt) (realscore i) i!score)
                "point")))
    (if (isnt i!type 'pollopt)
        (pr " by "))
  (hook 'itemscore i user))

(def byline (i user)
  (pr (tostring
        (userlink user i!by) (sp)
        (permalink i user (text-age:item-age i)))))

(def user-url (user) (+ "user?id=" user))

(= show-avg* nil)

(def userlink (user subject (o show-avg t))
  (link  (user-name user subject) (user-url subject))
  (awhen (and show-avg* (admin user) show-avg (uvar subject avg))
    (pr " (@(num it 1 t t))")))

(= noob-color* (color 60 150 60) noob-limit* 1337)

(def user-name (user subject)
  (if (and (editor user) (ignored subject))
       ;(tag ("span" "class" "editor") (pr subject))
       (tostring (spanclass "editor" (pr subject)))
      (and (editor user) (< (user-age subject) noob-limit*))
       ; (tag ("span" "class" "noob") (pr subject))
       (tostring (spanclass "noob" (pr subject)))
      subject))

(= show-threadavg* nil)

(def commentlink (i user)
  (when (cansee user i)
    (pr bar*)
    (tag (a href (item-url i!id))
      (let n (- (visible-family user i) 1)
        (if (> n 0)
          (do (pr (plural n "comment"))
              (awhen (and show-threadavg* (admin user) (threadavg i))
                (pr " (@(num it 1 t t))")))
          (pr "discuss"))))))

(def visible-family (user i)
  (+ (if (cansee user i) 1 0)
     (sum [visible-family user (item _)] i!kids)))

(def threadavg (i)
  (only.avg (map [or (uvar _ avg) 1]
                 (rem admin (dedup (map !by (keep live (family i))))))))

(= user-changetime* 120 editor-changetime* 1440)

(= everchange* (table) noedit* (table))

(def canedit (user i)
  (or (admin user)
      (and (~noedit* i!type)
           (editor user)
           (< (item-age i) editor-changetime*))
      (own-changeable-item user i)))

(def own-changeable-item (user i)
  (and (author user i)
       (~mem 'locked i!keys)
       (no i!deleted)
       (or (everchange* i!type)
           (< (item-age i) user-changetime*))))

(def editlink (i user)
  (when (canedit user i)
    (pr bar*)
    (link  "edit" (edit-url i))))

(def addoptlink (p user)
  (when (or (admin user) (author user p))
    (pr bar*)
    (onlink "add choice" (add-pollopt-page p user))))

; reset later

(= flag-threshold* 30 flag-kill-threshold* 7 many-flags* 1)

; Un-flagging something doesn't unkill it, if it's now no longer
; over flag-kill-threshold.  Ok, since arbitrary threshold anyway.

(def flaglink (i user whence)
  (when (and user
             (isnt user i!by)
             (or (admin user) (> (karma user) flag-threshold*)))
    (pr bar*)
    (w/rlink (do (togglemem user i!flags)
                 (when (and (~mem 'nokill i!keys)
                            (len> i!flags flag-kill-threshold*)
                            (< (realscore i) 10)
                            (~find admin:!2 i!vote))
                   (kill i 'flags))
                 whence)
      (pr "@(if (mem user i!flags) 'un)flag"))
    (when (and (admin user) (len> i!flags many-flags*))
      (pr bar* (plural (len i!flags) "flag") " ")
      (w/rlink (do (togglemem 'nokill i!keys)
                   (save-item i)
                   whence)
        (pr (if (mem 'nokill i!keys) "un-notice" "noted"))))))

(def killlink (i user whence)
  (when (admin user)
    (pr bar*)
    (w/rlink (do (zap no i!dead)
                 (if i!dead
                   (do (pull 'nokill i!keys)
                       (log-kill i user))
                   (pushnew 'nokill i!keys))
                 (save-item i)
                 whence)
      (pr "@(if i!dead 'un)kill"))))

(def locklink (i user whence)
  (when (admin user)
      (pr bar*)
      (w/rlink (do (togglemem 'locked i!keys)
                   (save-item i)
                   whence)
        (prt (if (mem 'locked i!keys) "un") "lock"))))

; Blast kills the submission and bans the user.  Nuke also bans the
; site, so that all future submitters will be ignored.  Does not ban
; the ip address, but that will eventually get banned by maybe-ban-ip.

(def blastlink (i user whence (o nuke))
  (when (and (admin user)
             (or (no nuke) (~empty i!url)))
    (pr bar*)
    (w/rlink (do (toggle-blast i user nuke)
                 whence)
      (prt (if (ignored i!by) "un-") (if nuke "nuke" "blast")))))

(def toggle-blast (i user (o nuke))
  (atomic
    (if (ignored i!by)
      (do (wipe i!dead (ignored i!by))
          (awhen (and nuke (sitename i!url))
            (set-site-ban user it nil)))
      (do (set i!dead)
          (ignore user i!by (if nuke 'nuke 'blast))
          (awhen (and nuke (sitename i!url))
            (set-site-ban user it 'ignore))))
    (if i!dead (log-kill i user))
    (save-item i)
    (save-prof i!by)))

(def candelete (user i)
  (or (admin user) (own-changeable-item user i)))

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
      ; link never used so not testable but think correct
      (display-item nil i user (flink [del-confirm-page (get-user _) i whence]))
      (spacerow 20)
      (tr (td)
          (td (urform user req
                      (do (when (candelete user i)
                            (= i!deleted (is (arg req "b") "Yes"))
                            (save-item i))
                          whence)
                 (prn "Do you want this to @(if i!deleted 'stay 'be) deleted?")
                 (br2)
                 (but "Yes" "b") (sp) (but "No" "b")))))))

(def permalink (story user text)
  (when (cansee user story)
    (link text (item-url story!id))))

(def logvote (ip user story)
  (newslog ip user 'vote (story 'id) (list (story 'title))))

(def text-age (a)
  (tostring
    (if (>= a 1440) (pr (plural (trunc (/ a 1440)) "day")    " ago")
        (>= a   60) (pr (plural (trunc (/ a 60))   "hour")   " ago")
                    (pr (plural (trunc a)          "minute") " ago"))))

; Voting

; A user needs legit-threshold karma for a vote to count if there has
; already been a vote from the same IP address.  A new account below both
; new- thresholds won't affect rankings, though such votes still affect
; scores unless not a legit-user.

(= legit-threshold* 0 new-age-threshold* 0 new-karma-threshold* 2)

(def legit-user (user)
  (or (editor user)
      (> (karma user) legit-threshold*)))

(def possible-sockpuppet (user)
  (or (ignored user)
      (< (uvar user weight) .5)
      (and (< (user-age user) new-age-threshold*)
           (< (karma user) new-karma-threshold*))))

(= downvote-ratio-limit* .65 recent-votes* nil votewindow* 100)

; Note: if vote-for by one user changes (s 'score) while s is being
; edited by another, the save after the edit will overwrite the change.
; Actual votes can't be lost because that field is not editable.  Not a
; big enough problem to drag in locking.

(def vote-for (user i (o dir 'up))
  (unless (or ((votes user) i!id)
              (and (~live i) (isnt user i!by)))
    (withs (ip   (logins* user)
            vote (list (seconds) ip user dir i!score))
      (unless (or (and (or (ignored user) (check-key user 'novote))
                       (isnt user i!by))
                  (and (is dir 'down)
                       (~editor user)
                       (or (check-key user 'nodowns)
                           (> (downvote-ratio user) downvote-ratio-limit*)
                           ; prevention of karma-bombing
                           (just-downvoted user i!by)))
                  (and (~legit-user user)
                       (isnt user i!by)
                       (find [is (cadr _) ip] i!votes))
                  (and (isnt i!type 'pollopt)
                       (biased-voter i vote)))
        (++ i!score (case dir up 1 down -1))
        ; canvote protects against sockpuppet downvote of comments
        (when (and (is dir 'up) (possible-sockpuppet user))
          (++ i!sockvotes))
        (metastory&adjust-rank i)
        (unless (or (author user i)
                    (and (is ip i!ip) (~editor user))
                    (is i!type 'pollopt))
          (++ (karma i!by) (case dir up 1 down -1))
          (save-prof i!by))
        (wipe (comment-cache* i!id)))
      (if (admin user) (pushnew 'nokill i!keys))
      (push vote i!votes)
      (save-item i)
      (push (list (seconds) i!id i!by (sitename i!url) dir)
            (uvar user votes))
      (= ((votes* user) i!id) vote)
      (save-votes user)
      (zap [firstn votewindow* _] (uvar user votes))
      (save-prof user)
      (push (cons i!id vote) recent-votes*))))

(def biased-voter (i vote) nil)

; ugly to access vote fields by position number

(def downvote-ratio (user (o sample 20))
  (ratio [is _.1.3 'down]
         (keep [let by ((item (car _)) 'by)
                 (nor (is by user) (ignored by))]
               (bestn sample (compare > car:cadr) (tablist (votes user))))))

(def just-downvoted (user victim (o n 3))
  (let prev (firstn n (recent-votes-by user))
    (and (is (len prev) n)
         (all (fn ((id sec ip voter dir score))
                (and (author victim (item id)) (is dir 'down)))
              prev))))

; Ugly to pluck out fourth element.  Should read votes into a vote
; template.  They're stored slightly differently in two diff places:
; in one with the voter in the car and the other without.

(def recent-votes-by (user)
  (keep [is _.3 user] recent-votes*))


; Story Submission

(newsop submit ()
  (if user
    (submit-page user "" "" t)
    (submit-login-warning "" "" t)))

(def submit-login-warning ((o url) (o title) (o showtext) (o text)
                           (o req)) ; unused
  (news-login-page "You have to be logged in to submit."
              (fn (user ip)
                (ensure-news-user user)
                (newslog ip user 'submit-login)
                (submit-page user url title showtext text))))

(def submit-page (user (o url) (o title) (o showtext) (o text "") (o msg)
                       (o req)) ; unused
  (minipage "Submit"
    (pagemessage msg)
    (urform user req
            (process-story (get-user req)
                           (clean-url (arg req "u"))
                           (striptags (arg req "t"))
                           showtext
                           (and showtext (md-from-form (arg req "x") t))
                           req!ip)
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
        (row "" submit-instructions*)
        (spacerow 20)
        (row "" submit-via-bookmarklet*)))))

(= submit-instructions*
   "Leave url blank to submit a question for discussion. If there is
    no url, the text (if any) will appear at the top of the comments
    page. If there is a url, the text will be ignored.")

(= submit-via-bookmarklet*
   (tostring
     (pr "You can also submit via ")
     (underlink "bookmarklet")
     (pr ".")))

; Bookmarklet

(newsop bookmarklet ()
  (msgpage user
    (tostring
      (row "When you click on the bookmarklet, it will submit the page you're on. To install, drag the link below to your browser bookmark bar.")
      (row (center (tag (span style "font-size: 2em") (underlink
        (string "Post to " this-site*)
        (string "javascript:window.location='"
                site-url*
                "/submitlink?u='+encodeURIComponent(document.location)+'&t='+encodeURIComponent(document.title)"))))))
    "Bookmarklet"))

; For use by outside code like bookmarklet.
; http://news.domain.com/submitlink?u=http://foo.com&t=Foo
; Added a confirm step to avoid xss hacks.

(newsop submitlink (u t)
  (if user
    (submit-page user u t)
    (submit-login-warning u t)))

(= title-limit* 80
   retry*       "Please try again."
   toolong*     "Please make title < @title-limit* characters."
   bothblank*   "The url and text fields can't both be blank.  Please
                 either supply a url, or if you're asking a question,
                 put it in the text field."
   toofast*     "You're submitting too fast.  Please slow down.  Thanks."
   spammage*    "Stop spamming us.  You're wasting your time.")

; Only for annoyingly high-volume spammers. For ordinary spammers it's
; enough to ban their sites and ip addresses.

(disktable big-spamsites* (+ newsdir* "big-spamsites"))

(def process-story (user url title showtext text ip)
  (aif (and (~blank url) (live-story-w/url url))
    (do (vote-for user it)
        (item-url it!id))
    ; NOTE: Here in Anarki, [...] without _ is nullary, so we're
    ; using (fn (_) (...)) instead.
    (if (no user)
         (flink [submit-login-warning url title showtext text _])
        (no (and (or (blank url) (valid-url url))
                 (~blank title)))
         (flink [submit-page user url title showtext text retry* _])
        (len> title title-limit*)
         (flink [submit-page user url title showtext text toolong* _])
        (and (blank url) (blank text))
         (flink [submit-page user url title showtext text bothblank* _])
        (let site (sitename url)
          (or (big-spamsites* site) (recent-spam site)))
         (flink:fn (_) (msgpage user spammage*))
        (oversubmitting user ip 'story url)
         (flink:fn (_) (msgpage user toofast*))
        (let s (create-story url (process-title title) text user ip)
          (story-ban-test user s ip url)
          (when (ignored user) (kill s 'ignored))
          (submit-item user s)
          (maybe-ban-ip s)
          "newest"))))

(def submit-item (user i)
  (push i!id (uvar user submitted))
  (save-prof user)
  (vote-for user i))

(def recent-spam (site)
  (and (caris (banned-sites* site) 'ignore)
       (recent-items [is (sitename _!url) site] 720)))

(def recent-items (test minutes)
  (let cutoff (- (seconds) (* 60 minutes))
    (latest-items test [< _!time cutoff])))

; Turn this on when spam becomes a problem.

(= enforce-oversubmit* nil)

; New user can't submit more than 2 stories in a 2 hour period.
; Give overeager users the key toofast to make limit permanent.

(def oversubmitting (user ip kind (o url))
  (and enforce-oversubmit*
       (or (check-key user 'toofast)
           (ignored user)
           (< (user-age user) new-age-threshold*)
           (< (karma user) new-karma-threshold*))
       (len> (recent-items [or (author user _) (is _!ip ip)] 180)
             (if (is kind 'story)
               (if (bad-user user) 0 1)
               (if (bad-user user) 1 10)))))

; Note that by deliberate tricks, someone could submit a story with a
; blank title.

(diskvar scrubrules* (+ newsdir* "scrubrules"))

(def process-title (s)
  (let s2 (multisubst scrubrules* s)
    (zap upcase (s2 0))
    s2))

(def live-story-w/url (url)
  (aand (url->story* (canonical-url url)) (check (item it) live)))

(def parse-site (url)
  (rev (tokens (cadr (tokens url [in _ #\/ #\?])) #\.)))

(defmemo sitename (url)
  (and (valid-url url)
       (let toks (parse-site (rem #\space url))
         (if (isa (errsafe:read (car toks)) 'int)
           (tostring (prall toks "" "."))
           (let (t1 t2 t3 . rest) toks
             (if (and (~in t3 nil "www")
                      (or (mem t1 multi-tld-countries*)
                          (mem t2 long-domains*)))
               (+ t3 "." t2 "." t1)
               (and t2 (+ t2 "." t1))))))))

(= multi-tld-countries* '("uk" "jp" "au" "in" "ph" "tr" "za" "my" "nz" "br"
                          "mx" "th" "sg" "id" "pk" "eg" "il" "at" "pl"))

(= long-domains* '("blogspot" "wordpress" "livejournal" "blogs" "typepad"
                   "weebly" "posterous" "blog-city" "supersized" "dreamhosters"
                   ; "sampasite"  "multiply" "wetpaint" ; all spam, just ban
                   "eurekster" "blogsome" "edogo" "blog" "com"))

(def create-story (url title text user ip)
  (newslog ip user 'create url (list title))
  (let s (inst 'item 'type 'story 'id (new-item-id)
                     'url url 'title title 'text text 'by user 'ip ip)
    (save-item s)
    (= (items* s!id) s)
    (unless (blank url) (register-url s url))
    (push s stories*)
    s))


; Bans

(def ignore (user subject cause)
  (set (ignored subject))
  (save-prof subject)
  (log-ignore user subject cause))

(diskvar ignore-log* (+ newsdir* "ignore-log"))

(def log-ignore (user subject cause)
  (todisk ignore-log* (cons (list subject user cause) ignore-log*)))

; Kill means stuff with this substring gets killed. Ignore is stronger,
; means that user will be auto-ignored.  Eventually this info should
; be stored on disk and not in the source code.

(disktable banned-ips*     (+ newsdir* "banned-ips"))   ; was ips
(disktable banned-sites*   (+ newsdir* "banned-sites")) ; was sites

(diskvar  comment-kill*    (+ newsdir* "comment-kill"))
(diskvar  comment-ignore*  (+ newsdir* "comment-ignore"))

(= comment-kill* nil ip-ban-threshold* 3)

(def set-ip-ban (user ip yesno (o info))
  (= (banned-ips* ip) (and yesno (list user (seconds) info)))
  (todisk banned-ips*))

(def set-site-ban (user site ban (o info))
  (= (banned-sites* site) (and ban (list ban user (seconds) info)))
  (todisk banned-sites*))

; Kill submissions from banned ips, but don't auto-ignore users from
; them, because eventually ips will become legit again.

; Note that ban tests are only applied when a link or comment is
; submitted, not each time it's edited.  This will do for now.

(def story-ban-test (user i ip url)
  (site-ban-test user i url)
  (ip-ban-test i ip)
  (hook 'story-ban-test user i ip url))

(def site-ban-test (user i url)
  (whenlet ban (banned-sites* (sitename url))
    (if (caris ban 'ignore) (ignore nil user 'site-ban))
    (kill i 'site-ban)))

(def ip-ban-test (i ip)
  (if (banned-ips* ip) (kill i 'banned-ip)))

(def comment-ban-test (user i ip string kill-list ignore-list)
  (when (some [posmatch _ string] ignore-list)
    (ignore nil user 'comment-ban))
  (when (or (banned-ips* ip) (some [posmatch _ string] kill-list))
    (kill i 'comment-ban)))

; An IP is banned when multiple ignored users have submitted over
; ban-threshold* (currently loaded) dead stories from it.

; Can consider comments too if that later starts to be a problem,
; but the threshold may start to be higher because then you'd be
; dealing with trolls rather than spammers.

(def maybe-ban-ip (s)
  (when (and s!dead (ignored s!by))
    (let bads (loaded-items [and _!dead (astory _) (is _!ip s!ip)])
      (when (and (len> bads ip-ban-threshold*)
                 (some [and (ignored _!by) (isnt _!by s!by)] bads))
        (set-ip-ban nil s!ip t)))))

(def killallby (user)
  (map [kill _ 'all] (submissions user)))

; Only called from repl.

(def kill-whole-thread (c)
  (kill c 'thread)
  (map kill-whole-thread:item c!kids))

; Polls

; a way to add a karma threshold for voting in a poll
;  or better still an arbitrary test fn, or at least pair of name/threshold.
; option to sort the elements of a poll when displaying
; exclusive field? (means only allow one vote per poll)

(= poll-threshold* 20)

(newsop newpoll ()
  (if (and user (> (karma user) poll-threshold*))
    (newpoll-page user)
    (pr "Sorry, you need @poll-threshold* karma to create a poll.")))

(def newpoll-page (user (o title "Poll: ") (o text "") (o opts "") (o msg)
                        (o req)) ; unused
  (minipage "New Poll"
    (pagemessage msg)
    (urform user req
            (process-poll (get-user req)
                          (striptags (arg req "t"))
                          (md-from-form (arg req "x") t)
                          (striptags (arg req "o"))
                          req!ip)
      (tab
        (row "title"   (input "t" title 50))
        (row "text"    (textarea "x" 4 50 (only.pr text)))
        (row ""        "Use blank lines to separate choices:")
        (row "choices" (textarea "o" 7 50 (only.pr opts)))
        (row ""        (submit))))))

(= fewopts* "A poll must have at least two options.")

(def process-poll (user title text opts ip)
  ; NOTE: Here in Anarki, [...] without _ is nullary, so we're using
  ; (fn (_) (...)) instead.
  (if (or (blank title) (blank opts))
       (flink:fn (_) (newpoll-page user title text opts retry*))
      (len> title title-limit*)
       (flink:fn (_) (newpoll-page user title text opts toolong*))
      (len< (paras opts) 2)
       (flink:fn (_) (newpoll-page user title text opts fewopts*))
      (atlet p (create-poll (multisubst scrubrules* title) text opts user ip)
        (ip-ban-test p ip)
        (when (ignored user) (kill p 'ignored))
        (submit-item user p)
        (maybe-ban-ip p)
        "newest")))

(def create-poll (title text opts user ip)
  (newslog ip user 'create-poll title)
  (let p (inst 'item 'type 'poll 'id (new-item-id)
                     'title title 'text text 'by user 'ip ip)
    (= p!parts (map get!id (map [create-pollopt p nil nil _ user ip]
                                (paras opts))))
    (save-item p)
    (= (items* p!id) p)
    (push p stories*)
    p))

(def create-pollopt (p url title text user ip)
  (let o (inst 'item 'type 'pollopt 'id (new-item-id)
                     'url url 'title title 'text text 'parent p!id
                     'by user 'ip ip)
    (save-item o)
    (= (items* o!id) o)
    o))

(def add-pollopt-page (p user)
  (minipage "Add Poll Choice"
    (urform user req
            (do (add-pollopt user p (striptags (arg req "x")) req!ip)
                (item-url p!id))
      (tab
        (row "text" (textarea "x" 4 50))
        (row ""     (submit))))))

(def add-pollopt (user p text ip)
  (unless (blank text)
    (atlet o (create-pollopt p nil nil text user ip)
      (++ p!parts (list o!id))
      (save-item p))))

(def display-pollopts (p user whence)
  (each o (visible user (map item p!parts))
    (display-pollopt nil o user whence)
    (spacerow 7)))

(def display-pollopt (n o user whence)
  (tr 
      (tag (td valign 'top)
      (votelinks o user whence))
      (tag (td class 'comment)
        (tag (div "class" "pollopt")
          (if (~cansee user o) (pr (pseudo-text o))
              (~live o)        (spanclass dead
                                 (pr (if (~blank o!title) o!title o!text)))
                               (if (and (~blank o!title) (~blank o!url))
                                   (link o!title o!url)
                                   ;(fontcolor black (pr o!text))
                                   (pr o!text)
                                   )
                               )))
  )
  (tr (if n (td))
      (td)
      (tag (td class 'default)
        (spanclass comhead
          (itemscore o)
          (editlink o user)
          (killlink o user whence)
          (deletelink o user whence)
          (deadmark o user)))))


; Individual Item Page (= Comments Page of Stories)

(defmemo item-url (id) (+ "item?id=" id))

(newsop item (id)
  (let s (safe-item id)
    (if (news-type s)
      (do (if s!deleted (note-baditem user ip))
          (item-page user s))
      (do (note-baditem user ip)
          (pr "No such item.")))))

(= baditemreqs* (table) baditem-threshold* 1/100)

; Something looking at a lot of deleted items is probably the bad sort
; of crawler.  Throttle it for this server invocation.

(def note-baditem (user ip)
  (unless (admin user)
    (++ (baditemreqs* ip 0))
    (with (r (requests/ip* ip) b (baditemreqs* ip))
       (when (and (> r 500) (> (/ b r) baditem-threshold*))
         (set (throttle-ips* ip))))))

(def news-type (i) (and i (in i!type 'story 'comment 'poll 'pollopt)))

(def item-page (user i)
  (with (title (and (cansee user i)
                    (or i!title (aand i!text (ellipsize (striptags it)))))
         here (item-url i!id))
    (longpage user (msec) nil title here
    (tag (div "class" "item")
      (display-item nil i user here)
      (display-item-text i user)
      (when (apoll i)
        (display-pollopts i user here))
      (when (and (cansee user i) (comments-active i))
        (comment-form i user here)))

      (when (and i!kids (commentable i))
        (display-subcomments i user here))
)))

(def commentable (i) (in i!type 'story 'comment 'poll))

; By default the ability to comment on an item is turned off after
; 45 days, but this can be overriden with commentable key.

(= commentable-threshold* (* 60 24 45))

(def comments-active (i)
  (and (live&commentable i)
       (live (superparent i))
       (or (< (item-age i) commentable-threshold*)
           (mem 'commentable i!keys))))


(= displayfn* (table))

(= (displayfn* 'story)   (fn (n i user here inlist)
                           (display-story n i user here)))

(= (displayfn* 'comment) (fn (n i user here inlist)
                           (display-comment n i user here nil 0 nil inlist)))

(= (displayfn* 'poll)    (displayfn* 'story))

(= (displayfn* 'pollopt) (fn (n i user here inlist)
                           (display-pollopt n i user here)))

(def display-item (n i user here (o inlist))
  ((displayfn* (i 'type)) n i user here inlist))

(def superparent (i)
  (aif i!parent (superparent:item it) i))

(def display-item-text (s user)
  (when (and (cansee user s)
             (in s!type 'story 'poll)
             (blank s!url)
             (~blank s!text))
    (tag ("div" "class" "itemtext") (prn s!text))
    ))

; Edit Item

(def edit-url (i) (+ "edit?id=" i!id))

(newsop edit (id)
  (let i (safe-item id)
    (if (and i
             (cansee user i)
             (editable-type i)
             (or (news-type i) (admin user) (author user i)))
      (edit-item user i)
      (pr "No such item."))))

(def editable-type (i) (fieldfn* i!type))

(= fieldfn* (table))

(= (fieldfn* 'story)
   (fn (user s)
     (with (a (admin user)  e (editor user)  x (canedit user s))
       `((string1 title     ,s!title        t ,x)
         (url     url       ,s!url          t ,e)
         (mdtext2 text      ,s!text         t ,x)
         ,@(standard-item-fields s a e x)))))

(= (fieldfn* 'comment)
   (fn (user c)
     (with (a (admin user)  e (editor user)  x (canedit user c))
       `((mdtext  text      ,c!text         t ,x)
         ,@(standard-item-fields c a e x)))))

(= (fieldfn* 'poll)
   (fn (user p)
     (with (a (admin user)  e (editor user)  x (canedit user p))
       `((string1 title     ,p!title        t ,x)
         (mdtext2 text      ,p!text         t ,x)
         ,@(standard-item-fields p a e x)))))

(= (fieldfn* 'pollopt)
   (fn (user p)
     (with (a (admin user)  e (editor user)  x (canedit user p))
       `((string  title     ,p!title        t ,x)
         (url     url       ,p!url          t ,x)
         (mdtext2 text      ,p!text         t ,x)
         ,@(standard-item-fields p a e x)))))

(def standard-item-fields (i a e x)
       `((int     votes     ,(len i!votes) ,a  nil)
         (int     score     ,i!score        t ,a)
         (int     sockvotes ,i!sockvotes   ,a ,a)
         (yesno   dead      ,i!dead        ,e ,e)
         (yesno   deleted   ,i!deleted     ,a ,a)
         (sexpr   flags     ,i!flags       ,a nil)
         (sexpr   keys      ,i!keys        ,a ,a)
         (string  ip        ,i!ip          ,e  nil)))

; Should check valid-url etc here too.  In fact make a fn that
; does everything that has to happen after submitting a story,
; and call it both there and here.

(def edit-item (user i)
  (let here (edit-url i)
    (shortpage user nil "Edit" here
      (tab (display-item nil i user here)
           (display-item-text i user))
      (br2)
      (vars-form user
                 ((fieldfn* i!type) user i)
                 (fn (name val)
                   (unless (ignore-edit user i name val)
                     (when (and (is name 'dead) val (no i!dead))
                       (log-kill i user))
                     (= (i name) val)))
                 (fn () (if (admin user) (pushnew 'locked i!keys))
                        (save-item i)
                        (metastory&adjust-rank i)
                        (wipe (comment-cache* i!id))
                        (edit-item user i)))
      (hook 'edit user i))))

(def ignore-edit (user i name val)
  (case name title (len> val title-limit*)
             dead  (and (mem 'nokill i!keys) (~admin user))))


; Comment Submission

(def comment-login-warning (parent whence (o text))
  (news-login-page "You have to be logged in to comment."
              (fn (u ip)
                (ensure-news-user u)
                (newslog ip u 'comment-login)
                (addcomment-page parent u whence text))))

(def addcomment-page (parent user whence (o text) (o msg))
  (minipage "Add Comment"
    (pagemessage msg)
    (tab
      (let here (flink [addcomment-page parent (get-user _) whence text msg])
        (display-item nil parent user here))
      (spacerow 10)
      (row "" (comment-form parent user whence text)))))

(= noob-comment-msg* nil
   mismatch-msg*
   "Please confirm that this is your comment by submitting it one more time.
   This is a security measure.
   It usually happens when you log in after the original page was loaded.")

; Comment forms last for 30 min (- cache time)

(def comment-form (parent user whence (o text))
  (tarform 1800
    (fn (req)
      (if (is (get-user req) user)
        (process-comment user parent (arg req "text") req!ip whence)
        (flink [addcomment-page parent (get-user _)
                                whence (arg req "text") mismatch-msg*])))
    (textarea "text" 6 60
      (aif text (prn (unmarkdown it))))
    (when (and noob-comment-msg* (noob user))
      (br2)
      (spanclass subtext (pr noob-comment-msg*)))
    (br2)
    (submit (if (acomment parent) "reply" "add comment"))))

(= comment-threshold* -20)

; Have to remove #\returns because a form gives you back "a\r\nb"
; instead of just "a\nb".   Maybe should just remove returns from
; the vals coming in from any form, e.g. in aform.

(def process-comment (user parent text ip whence)
  ; NOTE: Here in Anarki, [...] without _ is nullary, so we're using
  ; (fn (_) (...)) instead.
  (if (no user)
       (flink:fn (_) (comment-login-warning parent whence text))
      (empty text)
       (flink [addcomment-page parent (get-user _) whence text retry*])
      (oversubmitting user ip 'comment)
       (flink:fn (_) (msgpage user toofast*))
       (atlet c (create-comment parent (md-from-form text) user ip)
         (comment-ban-test user c ip text comment-kill* comment-ignore*)
         (if (bad-user user) (kill c 'ignored/karma))
         (submit-item user c)
         whence)))

(def bad-user (u)
  (or (ignored u) (< (karma u) comment-threshold*)))

(def create-comment (parent text user ip)
  (newslog ip user 'comment (parent 'id))
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
      (tag (li)
        (display-comment nil c user whence t indent initialpar initialpar)
        (display-subcomments c user whence (+ indent 1)))))

(def display-subcomments (c user whence (o indent 0))
  (if c!kids
    (tag (ul "class" "comments-list") 
      (each k (sort (compare > frontpage-rank:item) c!kids)
        (display-comment-tree (item k) user whence indent)))))

(def display-comment (n c user whence (o astree) (o indent 0)
                                      (o showpar) (o showon))
      (votelinks c user whence t)
      (display-comment-body c user whence astree indent showpar showon))

; Comment caching doesn't make generation of comments significantly
; faster, but may speed up everything else by generating less garbage.

; It might solve the same problem more generally to make html code
; more efficient.

(= comment-cache* (table) comment-cache-timeout* (table) cc-window* 10000)

(= comments-printed* 0 cc-hits* 0)

(= comment-caching* t)

; Cache comments generated for nil user that are over an hour old.
; Only try to cache most recent 10k items.  But this window moves,
; so if server is running a long time could have more than that in
; cache.  Probably should actively gc expired cache entries.

(def display-comment-body (c user whence astree indent showpar showon)
  (++ comments-printed*)
  (if (and comment-caching*
           astree (no showpar) (no showon)
           (live c)
           (nor (admin user) (editor user) (author user c))
           (< (- maxid* c!id) cc-window*)
           (> (- (seconds) c!time) 60)) ; was 3600
    (pr (cached-comment-body c user whence indent))
    (gen-comment-body c user whence astree indent showpar showon)))

(def cached-comment-body (c user whence indent)
  (or (and (> (or (comment-cache-timeout* c!id) 0) (seconds))
           (awhen (comment-cache* c!id)
             (++ cc-hits*)
             it))
      (= (comment-cache-timeout* c!id)
          (cc-timeout c!time)
         (comment-cache* c!id)
          (tostring (gen-comment-body c user whence t indent nil nil)))))

; Cache for the remainder of the current minute, hour, or day.

(def cc-timeout (t0)
  (let age (- (seconds) t0)
    (+ t0 (if (< age 3600)
               (* (+ (trunc (/ age    60)) 1)    60)
              (< age 86400)
               (* (+ (trunc (/ age  3600)) 1)  3600)
               (* (+ (trunc (/ age 86400)) 1) 86400)))))

(def gen-comment-body (c user whence astree indent showpar showon)
    (let parent (and (or (no astree) showpar) (c 'parent))
      (tag ("div" "class" "comhead")
         (tag-if (author user c) 
            (span class "opcomment topcol") 
          (itemline c user))
          (when parent
            (when (cansee user c) (pr bar*))
            (link "parent" (item-url ((item parent) 'id))))
          (editlink c user)
          (killlink c user whence)
          (blastlink c user whence)
          (deletelink c user whence)
          ; a hack to check whence but otherwise need an arg just for this
          (unless (or astree (is whence "newcomments"))
            (flaglink c user whence))
          (deadmark c user)
          (when showon
            (pr " | on: ")
            (let s (superparent c)
              (link (ellipsize s!title 50) (item-url s!id)))))
      (when (or parent (cansee user c)))
      (tag ("div" "class"  (if (> c!score 0) "comment" (string "comment comment-" (abs c!score)))
        "data-score" (string c!score) 
        ;"style" (string "color: #" (hexrep (comment-color c)))
        )
        (if (~cansee user c)               (pr (pseudo-text c))
            (nor (live c) (author user c)) (spanclass dead (pr c!text))
                                             (pr c!text)))
      (when (and astree (cansee user c) (live c))
        
        (tag ("div" "class" "comfoot")
          (if (and (~mem 'neutered c!keys)
                   (replyable c indent)
                   (comments-active c))
            (replylink c whence)
)))))

; For really deeply nested comments, caching could add another reply
; delay, but that's ok.

; People could beat this by going to the link url or manually entering
; the reply url, but deal with that if they do.

(= reply-decay* 1.8)   ; delays: (0 0 1 3 7 12 18 25 33 42 52 63)

(def replyable (c indent)
  (or (< indent 2)
      (> (item-age c) (expt (- indent 1) reply-decay*))))

(def replylink (i whence (o title 'reply))
  (link title (+ "reply?id=" i!id "&whence=" (urlencode whence))))

(newsop reply (id whence)
  (with (i      (safe-item id)
         whence (or (only.urldecode whence) "news"))
    (if (only.comments-active i)
      (if user
          (addcomment-page i user whence)
          (news-login-page "You have to be logged in to comment."
                      (fn (u ip)
                        (ensure-news-user u)
                        (newslog ip u 'comment-login)
                        (addcomment-page i u whence))))
      (pr "No such item."))))


; Threads

(def threads-url (user) (+ "threads?id=" user))

(newsop threads (id)
  (if id
    (threads-page user id)
    (pr "No user specified.")))

(def threads-page (user subject)
  (if (profile subject)
    (withs (title (+ subject "'s comments")
            label (if (is user subject) "threads" title)
            here  (threads-url subject))
      (longpage user (msec) label title here
        (awhen (keep [and (cansee user _) (~subcomment _)]
                     (comments subject maxend*))
          (display-threads user it label title here))))
    (prn "No such user.")))

(def display-threads (user comments label title whence
                      (o start 0) (o end threads-perpage*))
  (tag (ul "class" "threads-list")
    (each c (cut comments start end)
      (display-comment-tree c user whence 0 t))
    (when end
      (let newend (+ end threads-perpage*)
        (when (and (<= newend maxend*) (< end (len comments)))
          (tag (li)
            (morelink display-threads
              comments label title end newend)))))))

(def submissions (user (o limit))
  (map item (firstn limit (uvar user submitted))))

(def comments (user (o limit))
  (map item (retrieve limit acomment:item (uvar user submitted))))

(def subcomment (c)
  (some [and (acomment _) (is _!by c!by) (no _!deleted)]
        (ancestors c)))

(def ancestors (i)
  (accum a (trav i!parent a:item self:!parent:item)))


; Submitted

(def submitted-url (user) (+ "submitted?id=" user))

(newsop submitted (id)
  (if id
    (submitted-page user id)
    (pr "No user specified.")))

(def submitted-page (user subject)
  (if (profile subject)
    (with (label (+ subject "'s submissions")
           here  (submitted-url subject))
      (longpage user (msec) label label here
        (if (or (no (ignored subject))
                (is user subject)
                (seesdead user))
            (aif (keep [and (metastory _) (cansee user _)]
                       (submissions subject))
                 (display-items user it label label here 0 perpage* t)))))
    (pr "No such user.")))


; list stories from one domain

(newsop from (site)
  (listpage user (msec) (keep [is (sitename (_ 'url)) site] stories*) "from" (string "Submissions from " site)))


; list stories that don't link anywhere

(newsop ask ()
  (listpage user (msec) (keep [empty (_ 'url)] stories*) "ask" "Ask"))

; RSS
(newsop rss () (rsspage nil))

(newscache rsspage user 90
  (rss-stories (retrieve perpage* live ranked-stories*)))

(def rss-stories (stories (o title this-site*) (o url site-url*) (o desc site-desc*))
    (tag (rss version "2.0")
      (tag channel
        (tag title (pr title))
        (tag link (pr url))
        (tag description (pr desc))
        (each s stories
          (tag item
            (let comurl (+ site-url* (item-url s!id))
              (tag title    (pr (eschtml s!title)
                                (aif (sitename s!url) (+ " (" it ")") "")))
              (tag link     (pr (if (blank s!url) comurl (eschtml s!url))))
              (tag comments (pr comurl))
              (tag description
                (cdata (link "Comments" comurl)))))))))

; RSS feed of user
(newsop follow (subject)
  (rss-stories
     (retrieve perpage* [is (_ 'by) subject] stories*)
     subject
     (+ site-url* "?user=" subject)
     profile.subject!about))

; User Stats

(newsop leaders () (leaderspage user))

(= nleaders* 20)

(newscache leaderspage user 1000
  (longpage user (msec) "leaders" "Leaders" "leaders"
    (sptab
      (let i 0
        (each u (firstn nleaders* (leading-users))
          (tr (tdr:pr (++ i) ".")
              (td (userlink user u nil))
              (if showkarma* (tdr:pr (karma u)))
              (when (admin user)
                (tdr:prt (only.num (uvar u avg) 2 t t))))
          (if (is i 10) (spacerow 30)))))))

(= leader-threshold* 1)

(def leading-users ()
  (sort (compare > [karma _])
        (users [and (> (karma _) leader-threshold*) (~admin _)])))

(adop editors ()
  (tab (each u (users [is (uvar _ auth) 1])
         (row (userlink user u)))))


(= update-avg-threshold* 0)

(defbg update-avg 45
  (unless (or (empty profs*) (no stories*))
    (update-avg (rand-user [and (only.> (car (uvar _ submitted))
                                        (- maxid* initload*))
                                (len> (uvar _ submitted)
                                      update-avg-threshold*)]))))

(def update-avg (user)
  (= (uvar user avg) (comment-score user))
  (save-prof user))

(def rand-user ((o test idfn))
  (evtil (rand-key profs*) test))

; Ignore the most recent 5 comments since they may still be gaining votes.
; Also ignore the highest-scoring comment, since possibly a fluff outlier.

(def comment-score (user)
  (aif (check (nthcdr 5 (comments user 50)) [len> _ 10])
    (avg (cdr (sort > (map !score (rem !deleted it)))))
    nil))


; Comment Analysis

; Instead of a separate active op, should probably display this info
; implicitly by e.g. changing color of commentlink or by showing the
; no of comments since that user last looked.

(newsop active () (active-page user))

(newscache active-page user 600
  (listpage user (msec) (actives user) "active" "Active Threads"))

(def actives (user (o n maxend*) (o consider 2000))
  (visible user (rank-stories n consider (memo active-rank))))

(= active-threshold* 1500)

(def active-rank (s)
  (sum [max 0 (- active-threshold* (item-age _))]
       (cdr (family s))))

(def family (i) (cons i (mappend family:item i!kids)))


(newsop newcomments () (newcomments-page user))

(newscache newcomments-page user 60
  (listpage user (msec) (visible user (firstn maxend* comments*))
            "comments" "New Comments" "newcomments" nil))

; Noprocrast

(def check-procrast (user)
  (or (no user)
      (no (uvar user noprocrast))
      (let now (seconds)
        (unless (uvar user firstview)
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
                        (minutes-since (uvar user lastview)))))
    (pr "<b>Get back to work!</b>")
    (para "Sorry, you can't see this page.  Based on the anti-procrastination
           parameters you set in your profile, you'll be able to use the site
           again in " (plural m "minute") ".")
    (para "(If you got this message after submitting something, don't worry,
           the submission was processed.)")
    (para "To change your anti-procrastination settings, go to your profile
           by clicking on your username.  If <tt>noprocrast</tt> is set to
           <tt>yes</tt>, you'll be limited to sessions of <tt>maxvisit</tt>
           minutes, with <tt>minaway</tt> minutes between them.")
    (para)
    (w/rlink whence (underline (pr "retry")))
    ; (hspace 20)
    ; (w/rlink (do (reset-procrast user) whence) (underline (pr "override")))
    (br2)))


; Reset PW

(defopg resetpw req (resetpw-page (get-user req)))

(def resetpw-page (user (o msg))
  (minipage "Reset Password"
    (if msg
         (pr msg)
        (blank (uvar user email))
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


; Scrubrules

(defopa scrubrules req
  (scrub-page (get-user req) scrubrules*))

; If have other global alists, generalize an alist edit page.
; Or better still generalize vars-form.

(def scrub-page (user rules (o msg nil))
  (minipage "Scrubrules"
    (when msg (pr msg) (br2))
    (uform user req
           (with (froms (lines (arg req "from"))
                  tos   (lines (arg req "to")))
             (if (is (len froms) (len tos))
               (do (todisk scrubrules* (map list froms tos))
                   (scrub-page user scrubrules* "Changes saved."))
               (scrub-page user rules "To and from should be same length.")))
      (pr "From: ")
      (tag (textarea name 'from
                     cols (apply max 20 (map len (map car rules)))
                     rows (+ (len rules) 3))
        (apply pr #\newline (intersperse #\newline (map car rules))))
      (pr " To: ")
      (tag (textarea name 'to
                     cols (apply max 20 (map len (map cadr rules)))
                     rows (+ (len rules) 3))
        (apply pr #\newline (intersperse #\newline (map cadr rules))))
      (br2)
      (submit "update"))))


; Abuse Analysis

(adop badsites ()
  (sptab
    (row "Dead" "Days" "Site" "O" "K" "I" "Users")
    (each (site deads) (with (banned (banned-site-items)
                              pairs  (killedsites))
                         (+ pairs (map [list _ (banned _)]
                                       (rem (fn (d)
                                              (some [caris _ d] pairs))
                                            (keys banned-sites*)))))
      (let ban (car (banned-sites* site))
        (tr (tdr (when deads
                   (onlink (len deads)
                           (listpage user (msec) deads
                                     nil (+ "killed at " site) "badsites"))))
            (tdr (when deads (pr (round (days-since ((car deads) 'time))))))
            (td site)
            (td (w/rlink (do (set-site-ban user site nil) "badsites")
                  ;(fontcolor (if ban gray.220 black) (pr "x"))
              (tag-if ban (span class "banned") (pr "x"))
            ))
            (td (w/rlink (do (set-site-ban user site 'kill) "badsites")
              (spanclass (case ban kill "killed") (pr "x"))
                  ;(fontcolor (case ban kill darkred gray.220) (pr "x"))
            ))
            (td (w/rlink (do (set-site-ban user site 'ignore) "badsites")
              (spanclass (case ban ignore "ignored") (pr "x"))
                  ;(fontcolor (case ban ignore darkred gray.220) (pr "x"))
            ))
            (td (each u (dedup (map !by deads))
                  (userlink user u nil)
                  (pr " "))))))))

(defcache killedsites 300
  (let bads (table [each-loaded-item i
                     (awhen (and i!dead (sitename i!url))
                       (push i (_ it)))])
    (with (acc nil deadcount (table))
      (each (site items) bads
        (let n (len items)
          (when (> n 2)
            (= (deadcount site) n)
            (insort (compare > deadcount:car)
                    (list site (rev items))
                    acc))))
      acc)))

(defcache banned-site-items 300
  (table [each-loaded-item i
           (awhen (and i!dead (check (sitename i!url) banned-sites*))
             (push i (_ it)))]))

; Would be nice to auto unban ips whose most recent submission is > n
; days old, but hard to do because of lazy loading.  Would have to keep
; a table of most recent submission per ip, and only enforce bannnedness
; if < n days ago.

(adop badips ()
  (withs ((bads goods) (badips)
          (subs ips)   (sorted-badips bads goods))
    (sptab
      (row "IP" "Days" "Dead" "Live" "Users")
      (each ip ips
        (tr (td (let banned (banned-ips* ip)
                  (w/rlink (do (set-ip-ban user ip (no banned))
                               "badips")

                    ;(fontcolor (if banned darkred) (pr ip))
                    (tag-if banned (span class "banned") (pr ip))
                    )))
            (tdr (when (or (goods ip) (bads ip))
                   (pr (round (days-since
                                (max (aif (car (goods ip)) it!time 0)
                                     (aif (car (bads  ip)) it!time 0)))))))
            (tdr (onlink (len (bads ip))
                         (listpage user (msec) (bads ip)
                                   nil (+ "dead from " ip) "badips")))
            (tdr (onlink (len (goods ip))
                         (listpage user (msec) (goods ip)
                                   nil (+ "live from " ip) "badips")))
            (td (each u (subs ip)
                  (userlink user u nil)
                  (pr " "))))))))

(defcache badips 300
  (with (bads (table) goods (table))
    (each-loaded-item s
      (if (and s!dead (commentable s))
        (push s (bads  s!ip))
        (push s (goods s!ip))))
    (each (k v) bads  (zap rev (bads  k)))
    (each (k v) goods (zap rev (goods k)))
    (list bads goods)))

(def sorted-badips (bads goods)
  (withs (ips  (let ips (rem [len< (bads _) 2] (keys bads))
                (+ ips (rem [mem _ ips] (keys banned-ips*))))
          subs (table
                 [each ip ips
                   (= (_ ip) (dedup (map !by (+ (bads ip) (goods ip)))))]))
    (list subs
          (sort (compare > (memo [badness (subs _) (bads _) (goods _)]))
                ips))))

(def badness (subs bads goods)
  (* (/ (len bads)
        (max .9 (expt (len goods) 2))
        (expt (+ (days-since (aif (car bads) it!time 0))
                 1)
              2))
     (if (len> subs 1) 20 1)))


(edop flagged ()
  (display-selected-items user [retrieve maxend* flagged _] "flagged"))

(def flagged (i)
  (and (live i)
       (~mem 'nokill i!keys)
       (len> i!flags many-flags*)))


(edop killed ()
  (display-selected-items user [retrieve maxend* !dead _] "killed"))

(def display-selected-items (user f whence)
  (display-items user (f stories*) nil nil whence)
  (vspace 35)
  ;(color-stripe textgray)
  (vspace 35)
  (display-items user (f comments*) nil nil whence))


; Rather useless thus; should add more data.

(adop badguys ()
  (tab (each u (sort (compare > [uvar _ created])
                     (users [ignored _]))
         (row (userlink user u nil)))))

(adop badlogins ()  (logins-page bad-logins*))

(adop goodlogins () (logins-page good-logins*))

(def logins-page (source)
  (sptab (each (time ip user) (firstn 100 (rev (as cons source)))
           (row time ip user))))

; personal data export in machine readable format as required by GDPR
; TODO: should include logs (and export as JSON)
(newsop personal-data ()
    (pr (tojson (obj
           votes     (votes* user)
           comments  (keep [author user _] comments*)
           stories   (keep [author user _] stories*)))))

(def personal-data-link ()
  (tostring (underlink "download personal data" "personal-data")))

; Stats

(adop optimes ()
  (sptab
    (tr (td "op") (tdr "avg") (tdr "med") (tdr "req") (tdr "total"))
    (spacerow 10)
    (each name (sort < newsop-names*)
      (tr (td name)
          (let ms (only.avg (as cons (optimes* name)))
            (tdr:prt (only.round ms))
            (tdr:prt (only.med (as cons (optimes* name))))
            (let n (opcounts* name)
              (tdr:prt n)
              (tdr:prt (and n (round (/ (* n ms) 1000))))))))))

; since Arc has no modules we have to turn off global settings turned on just
; in this file
(declare 'atstrings nil)
