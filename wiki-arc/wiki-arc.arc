; Arkani
; by AmkG
; A wiki, in Arc.  The goal is to have a similar
; feature set to mediawiki (discussion pages, history,
; similar markup rules, templates, "What links here").

(require "whtml.arc")
(require "lib/settable-fn.arc")
(require "lib/file-table.arc")
(require "lib/scanner.arc")
(require "lib/m_treeparse.arc")

(require "wiki-arc/wikiconf.arc")
(require "wiki-arc/diff.arc")
(require "wiki-arc/ampersands.arc")

(attribute a class opstring)
(attribute a name opstring)

; macro for easily destructuring web arguments
(mac *wiki-args (vars args . body)
  `(with
       ,(mappend
         [list _ `(alref ,args ,(string _))] vars)
     ,@body))

; macro for displaying a page
(mac *wiki-page (title css . body)
  (w/uniq t1
    `(w/html
       (let ,t1 (msec)
         ('head
           ('title (pr ,title))
           (if (and css (isnt "" css) (file-exists css))
               ('(link rel "stylesheet"
                       type "text/css"
                       href (flink (fn args
                                     (w/infile s css
                                       (whilet c (readc s) (pr c)))))))))
         ('body ,@body
                ('hr)
                ('.rendertime (pr "(" (- (msec) ,t1) " msec)"))
                ('.footnote (pr "Powered by Arkani, a wiki in Anarki.")))))))

; wiki-arc module
(= Arkani
  (let (help* sig source-file*
        wiki add-wiki wikis
        _->space space->_ isdigit pr-esc capitalize
        scan-words scan-logs scan-paras
        serialize
        new-log head-rv get-rv save-page
        urlencode
        header-p header-display
        enformat-base) nil
    ; protect against arc-wiki 'def bashing the
    ; global docstrings tables
    (= help* (table) sig (table) source-file* (table))
    ; the set of wikis defined
    (= wikis (table))
    ; conversion from wiki-style names to ordinary ones
    (def _->space (s)
      (tostring:each c s
        (if (is c #\_) (pr #\space) (pr c))))
    (def space->_ (s)
      (tostring:each c s
        (if (is c #\space) (pr #\_) (pr c))))
    ; determine if char is digit
    (def isdigit (c)
      (in c #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
    ; protect html characters
    (def pr-esc (c)
      (if (isa c 'char)
          (case c
            #\< (pr "&lt;")
            #\> (pr "&gt;")
            #\& (pr "&amp;")
            #\" (pr "&quot;")
                (pr c))
         (each rc c (pr-esc rc))))
    ; capitalizes the first character of the input string
    (def capitalize (s)
      (string (upcase (s 0)) (cut s 1)))
    ; creates a scanner for words
    (def scan-words (s (o start 0) (o end (len s)))
      " Creates a scanner which traverses the words
        of the given string. "
      (when (< start end)
        (let w-end start
          (while (and (< w-end end) (whitec (s w-end)))   (++ w-end))
          (while (and (< w-end end) (nonwhite (s w-end))) (++ w-end))
          (while (and (< w-end end) (whitec (s w-end)))   (++ w-end))
          (scanner
            'car (scanner-string s start w-end)
            'cdr (scan-words s w-end end)))))
    ; creates a scanner for a text history log
    (def scan-logs (s (o start 0) (o end (len s)))
      (when (< start end)
        (withs (i start
                read-num-base
                (fn ()
                  (coerce
                    (tostring:do
                      (pr #\0)
                      (while (isdigit (s i))
                        (pr (s i))
                        (++ i)))
                    'int))
                read-num
                (fn () (do1 (read-num-base) (++ i)))
                read-string
                (fn (len)
                  (do1
                    (tostring:repeat len
                      (pr (s i))
                      (++ i))
                    (++ i)))
                rv (read-num)
                len_editby (read-num)
                editby (read-string len_editby)
                tm (read-num)
                len_log (read-num)
                log (read-string len_log)
                numerify
                (fn (num e)
                  (if num `(,num ,e) e))
                diff-parse
                (afn ()
                  (let num (if (isdigit (s i)) (read-num-base))
                    (case (s i)
                      #\d (do
                            (++ i)
                            (cons (numerify num 'delete) (self)))
                      #\s (do
                            (++ i)
                            (cons (numerify num 'skip) (self)))
                      #\i (do
                            (++ i)
                            (let w
                                 (tostring
                                   (repeat num
                                     (pr (s i))
                                     (++ i)))
                              (cons `(insert ,w) (self))))
                      #\@ nil)))
                diff (diff-parse))
          ; skip the trailing @
          (++ i)
          (scanner
            'car (table 'rv rv
                        'editby editby
                        'tm tm
                        'log log
                        'diff diff)
            'cdr (scan-logs s i end)))))
    ; creates a scanner for paragraphs
    (def scan-paras (s (o start 0) (o end (len s)))
      (when (< start end)
        (with (i start
               j start
               l start
               allwhite t)
          ; NOTE!  The *proper* EOL is \r\n, because
          ; that's the web standard.
          ; to simplify, we just define paragraphs
          ; as being separated by lines consisting
          ; only of whitespace
          ; first skip over whitespace-only lines
          (while (and (< j end) (whitec (s j)))
            (when (is (s j) #\newline) (= i (+ 1 j)))
            (++ j))
          ; now start searching for a whitespace-only line
          (= l j)
          (breakable:while (< j end)
            (if
              (is (s j) #\newline)
                (if allwhite
                    (break nil)
                    (= allwhite t))
              (nonwhite (s j))
                (= l j
                   allwhite nil))
            (++ j))
          (++ l)
          (scanner
            'car (scanner-string s i l)
            'cdr (scan-paras s l end)))))
    ; serializes a diff
    (def serialize (diff)
      (each e diff
        (when (and (acons e) (or (isa (car e) 'int) (isa (car e) 'num)))
          (pr (car e))
          (zap cadr e))
        (case e
          skip   (pr "s")
          delete (pr "d")
                 (do (pr (len (cadr e)))
                     (pr "i")
                     (each c (cadr e)
                       (pr c))))))
    ; create a new log
    (def new-log (rv editby tm log diff)
      (tostring
        (pr rv " " (len editby) " " editby " " tm " "
            (len log) " " log " ")
        (serialize diff)
        (pr "@")))
    ; gets the specific revision
    (def get-rv (data meta p rv)
      (if rv
        (with (cur-ct (scan-words data.p)
               ht     (scan-logs meta.p))
          (breakable:each l ht
            (if (<= l!rv rv) (break nil))
            (zap *wiki-undiff cur-ct l!diff))
          (tostring:each w cur-ct
            ; fortunately each works on both
            ; strings and scanners
            (each c w
              (writec c))))
        data.p))
    ; determine the revision number of the head of a
    ; (text) history
    (def head-rv (ht)
      (if (isnt ht "")
          ; WARNING this assumes that the history file is not
          ; corrupted
          (coerce
            (tostring:let ht (if (isa ht 'string) (scanner-string ht) ht)
              (while (isdigit:car ht)
                (pr:car ht)
                (zap cdr ht)))
            'int)
          0))
    ; save the page, including history
    (def save-page (data meta title ct log req)
      (withs (editby (or (get-user req) req!ip)
              old-ct (or data.title "")
              old-ht (or meta.title "")
              diff   (*wiki-diff (scan-words ct) (scan-words old-ct) iso))
      (= meta.title (+ (new-log (+ 1 (head-rv old-ht))
                                editby
                                (seconds)
                                log
                                diff)
                       old-ht))
      (= data.title ct)))
    ; determines if the given paragraph scanner
    ; is a header.
    (def header-p (p)
      (when (is (car p) #\=)
        (with (sp (cdr p)
               nums 1
               nume 0
               prespace nil)
          ; determine the number of ='s at the start
          (while (is (car sp) #\=)
            (zap cdr sp)
            (++ nums))
          ; make sure it's a space char that
          ; stopped us, and that there's a
          ; nonwhitespace char after it
          (when (and (is #\space (car sp)) (nonwhite (cadr sp)))
            ; check that we have a similar sequence at
            ; the end
            ((afn (sp)
               (if
                 (is (car sp) #\=)
                   (do (if prespace (++ nume))
                       (self (cdr sp)))
                 (is (car sp) #\newline)
                   nil
                 (is (car sp) #\space)
                   (do (= prespace t)
                       (self (cdr sp)))
                 (no sp)
                   (is nums nume)
                   (do (= nume 0
                          prespace nil)
                       (self (cdr sp)))))
             (cdr sp))))))
    ; display a header
    (def header-display (enformat p)
      (with (num 0
             sp p)
        ; count the number of ='s
        (while (is (car sp) #\=)
          (zap cdr sp)
          (++ num))
        (zap cdr sp)
        (pr "<h" num ">")
        (enformat (cut sp 0 (- (+ 1 num))))
        (pr "</h" num ">")))
    ; format a single paragraph
    ; ~!TODO: Change this to handle formatting
    (def enformat-base (link-to)
      (let (; extensions to treeparse
            enclose-sem seq-str
            ; actions
            on-plain-wiki-link in-italics in-bold
            article text
            on-article-wiki-link on-text-wiki-link
            ; parsers
            open-br close-br p-nonwhite italics bold
            nowiki nowiki-e ampersand-codes
            ampersand-coded-text
            nowiki-text italicized-text bolded-text
            plain-wiki-link joined-wiki-link formatting) nil
        ; extensions to treeparse
        (= enclose-sem
           (fn (f p)
             " Adds semantics like 'sem, but encloses the semantics
               defined by inner parsers instead of just adding to
               them. "
             (fn (r)
               (iflet enc (parse p r)
                      (return (enc 0) (enc 1)
                              (list
                                (fn () (f enc))))))))
        (= seq-str
           (fn (s)
             (zap scanner-string s)
             (fn (remaining)
               (seq-r s remaining nil nil))))
        ; actions
        (= on-plain-wiki-link
           [let s (string _)
             (= article s text s)])
        (= on-article-wiki-link
           [= article (string _)])
        (= on-text-wiki-link
           [= text (string _)])
        (= on-wiki-link-completed
           [link-to article (string text _)])
        (w/html-tags
          (= in-italics
               ['i (carry-out _)])
          (= in-bold
               ['b (carry-out _)]))
        ; parsers
        (= open-br
          (seq-str "[["))
        (= close-br
          (seq-str "]]"))
        (= p-alphadig
          (pred alphadig:car anything))
        (= italics
          (seq-str "''"))
        (= bold
          (seq-str "'''"))
        (= nowiki
          (seq-str "<nowiki>"))
        (= nowiki-e
          (seq-str "</nowiki>"))
        (= ampersand-codes
          (apply alt
            (map seq-str *wiki-ampersand-codes)))
        (= ampersand-coded-text
          (sem [map pr _] (seq #\& ampersand-codes #\;)))
        (= italicized-text
          (seq italics
               (enclose-sem in-italics (many (seq (cant-see italics) (delay-parser formatting))))
               italics))
        (= bolded-text
          (seq bold
               (enclose-sem in-bold (many (seq (cant-see bold) (delay-parser formatting))))
               bold))
        (= nowiki-text
          (seq nowiki
               (sem pr-esc (many (anything-but nowiki-e)))
               nowiki-e))
        (= plain-wiki-link
          (seq open-br
               ; should really be (many anything), however treeparse.arc
               ; currently does not do backtracking on 'many
               (sem on-plain-wiki-link (many (anything-but #\| close-br)))
               close-br
               (sem on-wiki-link-completed (many p-alphadig))))
        (= joined-wiki-link
          (seq open-br
               (sem on-article-wiki-link (many (anything-but #\|)))
               #\|
               (sem on-text-wiki-link (many (anything-but close-br)))
               close-br
               (sem on-wiki-link-completed (many p-alphadig))))
        (= formatting
          (alt
            plain-wiki-link
            joined-wiki-link
            bolded-text
            italicized-text
            ampersand-coded-text
            nowiki-text
            (sem pr-esc anything)))
        (fn (p)
          (carry-out (parse (many formatting) p)))))
    ; use our own urlencode -
    ; arc-wiki version may suddenly change in the future, breaking
    ; existing filebases
    (= urlencode
      (fn (s)
        " Converts to urlencoding.
          Also used for generating a filename for use by
          file-table "
        (tostring:each c s
          (if
            (is #\space c)
              (writec #\+)
            ((andf ~alphadig [no (in _ #\_ #\.)]) c)
              (let code (coerce (coerce c 'int) 'string 16)
                (writec #\%)
                (each p code (writec p)))
              (writec c)))))
    ; generate wiki content
    (def wiki (op name data meta css   args req)
      " Creates the output of the wiki system. "
      ; filter keys via urlencode
      (with (data data:urlencode
             meta meta:urlencode)
        ; TODO: put these inside *wiki-args form
        (*wiki-args (action title rv) args
          ; letrec form, because the subfunctions might
          ; end up calling one another.
          (let (p
                talk-page-p talk-page article-page
                link-to this-page
                add-ons cant-edit edit edit-target
                hist display empty-page display-content) nil
            (=
               ; determine if this is a talk page
               talk-page-p
               (fn ()
                 (prefix "Talk:" p))
               ; talk-page for current page, or current page if
               ; talk-page
               talk-page
               (fn ()
                 (if (talk-page-p)
                     p
                     (+ "Talk:" p)))
               ; article for current page, or current page if
               ; article
               article-page
               (fn ()
                 (if (talk-page-p)
                     (cut p 5)
                     p))
               ; recomposes the address for this page
               this-page
               (+ (string op)
                  (tostring:prall
                    (map
                      [+ (car _) "=" (cadr _)]
                      args) "?" "&"))
               ; topbar and side bar
               add-ons
               (fn ()
                 (w/html-tags
                   ; maybe float: right
                   ('.topinfo
                     (aif (get-user req)
                       (do
                         ('span.username
                           (link-to (+ "User:" it) it))
                         ('span.logout
                           (w/rlink (do (logout-user it) this-page)
                             (pr "logout"))))
                       ('span.username
                         (w/link (login-page 'both (+ "Log into " name)
                                   (list nilfn
                                     this-page))
                           (pr "login")))))
                   ('.topbar
                     ; perhaps remove the 'a if already on that page?
                     ('(span.article
                         id (if (~talk-page-p) 'selected))
                       (tag-if (~talk-page-p) b
                         (link-to (article-page) "article")))
                     ('(span.discussion
                         id (if (talk-page-p) 'selected))
                       (tag-if (talk-page-p) b
                         (link-to (talk-page) "discussion")))
                     ('(span.edit
                         id (if (is action 'edit) 'selected))
                       (tag-if (is action 'edit) b
                         (link-to p "edit" 'edit rv)))
                     ('(span.history
                         id (if (is action 'hist) 'selected))
                       (tag-if (is action 'hist) b
                         (link-to p "history" 'hist))))
                   ('.sidebar
                     ('div
                       (link-to "Main Page" "Main Page"))
                     ('div
                       (let articles-only [rem [some #\: (urldecode _)] _]
                         (w/rlink (+ (string op) "?title=" (random-elt:articles-only:keys data))
                           (pr "Random Article")))))))
               link-to
               ; creates a link to the specified article
               (fn (l-p text (o l-action) (o l-rv))
                 (zap space->_:capitalize l-p)
                 (withs (rp (urlencode l-p)
                         href (+ "?title=" rp
                                 (if l-action
                                     (+ "&action=" (urlencode:string l-action))
                                     "")
                                 (if l-rv
                                     (+ "&rv=" (string l-rv))
                                     "")))
                   (w/html-tags
                     (if
                       (and (is p l-p) (is action l-action) (is l-rv rv))
                         ('b (pr-esc text))
                       (some rp (keys data))
                         ('(a href href)
                           (pr-esc text))
                       ; else
                         ('(a.deadlink href href)
                           (pr-esc text))))))
               ; returns nil if editing allowed,
               ; a string detailing the reason why not otherwise
               cant-edit
               (fn () nil)
               edit
               (fn ()
                 (*wiki-page (+ "Editing " (_->space p) " - " name ) css
                   (add-ons)
                   ('.main
                     (aif (cant-edit)
                          (do
                            ('p (pr-esc it)))
                          (let ct (get-rv data meta p rv)
                            ('h1
                              (pr-esc:+ "Editing " (_->space p)))
                            (when (and rv (isnt rv (head-rv meta.p)))
                               ('.warning
                                 (pr-esc "This is an old revision of this page")
                                 #|
                                 (pr-esc ", as edited by ")
                                 ('b (pr-esc (editor-rv meta p rv)))
                                 (pr-esc " on ")
                                 ('b (pr-esc (date (time-rv meta p rv))))
                                 (pr-esc ".")|#))
                            (arform [edit-target req!ip _]
                              ; should have id/class?
                              ('(textarea name 'ct rows 25 cols 80)
                                (when ct (pr-esc ct)))
                              ('div ('p
                                      (pr "Edit summary")
                                      ('span.small
                                        (pr "(Briefly describe the "
                                            "changes you have made)")))
                                    ('(textarea name 'log rows 1 cols 80)))
                              ('div
                                ('(input
                                    type 'submit
                                    name 'st
                                    value "Save Page")))))))))
               edit-target
               (fn (old-ip req)
                 (*wiki-args (ct log) req!args
                   ; feels wrong.  Should be as short as (= data.p ct)
                   (if (is req!ip old-ip) (save-page data meta p ct log req))
                   (+ (string op) "?title=" (urlencode p))))
               ; show history
               hist
               (fn ()
                 (*wiki-page (+ "Revision history of "
                                (_->space p) " - " name) css
                   (add-ons)
                   ('.main
                     ('h1
                       (pr-esc:+ "Revision history of " (_->space p)))
                     (if meta.p
                         ('ul
                           (let ht (scan-logs meta.p)
                             (each l ht
                               ('li
                                 ('(a href (+ "?title=" (urlencode p)
                                              "&rv=" (string l!rv)))
                                   (pr-esc (date l!tm)))
                                 (pr " ")
                                 ('span
                                   (pr-esc l!editby))
                                 (when (and l!log (isnt l!log ""))
                                   (pr " ")
                                   ('i (pr-esc:+ "(" l!log ")")))))))
                         ('p
                           (pr
                            "There is no revision history for this page."))))))
               ; should be changed in future to displaying the content of a
               ; template instead.
               empty-page
               (fn ()
                 (w/html-tags
                   ('p
                     ('b
                       (pr-esc:+ name
                           " does not have an article with this exact name. ")))
                   ('ul
                     ('li
                       ('(a href (+ "?title=" (urlencode p) "&action=edit"))
                          ('b (pr "Start the ")
                              ('i (pr-esc (_->space p)))
                              (pr " article")))
                          (pr ".")))))
               display-content
               (fn (ct)
                 (let enformat (enformat-base link-to)
                   (each p (scan-paras ct)
                     (if
                       (header-p p)
                         (header-display enformat p)
                       ; else
                         (tag p (enformat p))))))
               display
               (fn ()
                 (*wiki-page (+ (_->space p) " - " name) css
                   (add-ons)
                   ('.main
                     (let ct (get-rv data meta p rv)
                       (if (no ct)
                           (empty-page)
                           (do
                             (when (and rv (isnt rv (head-rv meta.p)))
                               ('.warning
                                 (pr-esc "This is an old revision of this page")
                                 #|
                                 (pr-esc ", as edited by ")
                                 ('b (pr-esc (editor-rv meta p rv)))
                                 (pr-esc " on ")
                                 ('b (pr-esc (date (time-rv meta p rv))))
                                 (pr-esc ".")|#))
                             ('h1 (pr-esc (_->space p)))
                             (display-content ct))))))))
            ; body
            (if (or (no title) (is title "")) (= title "Main Page"))
            (zap space->_:capitalize title)
            (= p title)
            (if action (zap sym action))
            (= rv (errsafe (coerce rv 'int)))
            (if (and rv (< rv 1)) (wipe rv))
            (case action
              edit   (edit)
              hist   (hist)
                     (display))))))
    (def add-wiki (op name data meta css)
      (= wikis.op (list name data meta css)))
    (add-attachments
      '= (fn (v s)
           (case s
               (err "Invalid key assignment")))
      'keys (fn () (list 'wiki 'add-wiki 'wikis))
      (annotate 'table
        (fn (s)
          (case s
            wiki wiki
            add-wiki add-wiki
            wikis (tablist wikis)))))))

(mac *wiki-def (op name data meta css)
  (w/uniq (d-v m-v n-v c-v req)
    `(with (,d-v ,data
            ,m-v ,meta
            ,n-v ,name
            ,c-v ,css)
       (Arkani!add-wiki ',op ',name ',data ',meta ',css)
       (defop ,op ,req
         (Arkani!wiki ',op ,n-v ,d-v ,m-v ,c-v  (,req 'args) ,req)))))

(ensure-dir "arc/")
(if (file-exists "arc/wiki-arc.conf") (load "arc/wiki-arc.conf"))

; test
(*wiki-def wikitest "Wiki Test"
           (file-table "arc/wiki-test")
           (file-table "arc/wiki-test-meta")
           "wiki-arc/wiki-arc-default.css")


