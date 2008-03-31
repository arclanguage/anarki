; Arki
; by AmkG
; A wiki, in Arc.  The goal is to have a similar
; feature set to mediawiki (discussion pages, history,
; similar markup rules, templates, "What links here").

(require "whtml.arc")
(require "lib/settable-fn.arc")
(require "lib/file-table.arc")
(require "lib/cached-table.arc")
(require "lib/scanner.arc")
(require "lib/cps_treeparse.arc")
(require "lib/arcformat.arc")

(require "wiki-arc/wikiconf.arc")
(require "wiki-arc/diff.arc")
(require "wiki-arc/ampersands.arc")

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
                ('.footnote
                  ('.right (pr "(" (- (msec) ,t1) " msec)"))
                  ('.left (pr ""))
                  ('.center (pr "Powered by Arki, a wiki in Anarki."))))))))

(= *wiki-profiling-on nil)
; macro for profiling parser
(when *wiki-profiling-on
  (require "lib/profiler.arc")
  (profiler-reset))
(mac *wiki-pp (parser fun)
  (if *wiki-profiling-on
      `(do
         (= ,parser ,fun)
         (profile-function ,parser))
      `(= ,parser ,fun)))

(when *wiki-profiling-on
  (profile seq-r alt-r many-r))

; wiki-arc module
(= Arki
  (let (help* sig source-file*
        wiki add-wiki wikis
        _->space space->_ isdigit
        pr-esc escape capitalize
        coerce-into
        in-paren
        scan-words scan-logs scan-paras
        serialize
        new-log head-rv get-rv save-page
        urlencode
        header-p header-display
        code-block-p code-block-display
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
          (pr (escape c))
          (each rc c (pr-esc rc))))
    (def escape (c)
      (case c
        #\< "&lt;"
        #\> "&gt;"
        #\& "&amp;"
        #\" "&quot;"
            c))
    ; capitalizes the first character of the input string
    (def capitalize (s)
      (string (upcase (s 0)) (cut s 1)))
    ; coerce into an integer within the range
    (def coerce-into (n (o l) (o h))
      (let n (coerce n 'int)
        (if l (zap max n l))
        (if h (zap min n h))
        n))
    ; prints in parentheses; separated because
    ; editors might get confused with the parens
    (def in-paren (s)
      (pr "(" s ")"))
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
    ; determines if the given paragraph scanner is
    ; a code block
    (def code-block-p (p)
      (and (is (car p) #\space) (is (cadr p) #\space)))
    ; display a code block
    (def code-block-display (enformat p)
      (tag pre
        (enformat p)))
    ; format a single paragraph
    (def enformat-base (link-to)
      (let (; extensions to treeparse
            seq-str nil-seq-str
            ; actions
            in-italics in-bold
            in-wiki-link
            ; parsers
            open-br close-br p-alphadig italics bold
            nowiki nowiki-e arc-tag arc-tag-e
            ampersand-codes elided-white
            arc-code ampersand-coded-text
            nowiki-text italicized-text bolded-text
            wiki-link formatting
            many-format
            ; output
            print-out) nil
        ; extensions to treeparse
        (= seq-str
           (fn (s)
             (seq-l (scanner-string s))))
        (= nil-seq-str
           (fn (s)
             (nil-seq-l (scanner-string s))))
        ; actions
        (= in-wiki-link
           (fn ((article text . addtext))
             (tostring:link-to (string article)
                               (if addtext
                                   (string text (car addtext))
                                   (string article text)))))
        (= in-italics
           [list "<i>" _ "</i>"])
        (= in-bold
           [list "<b>" _ "</b>"])
        ; parsers
        (*wiki-pp open-br
          (nil-seq-str "[["))
        (*wiki-pp close-br
          (nil-seq-str "]]"))
        (*wiki-pp p-alphadig
          (pred alphadig:car anything))
        (*wiki-pp italics
          (nil-seq-str "''"))
        (*wiki-pp bold
          (nil-seq-str "'''"))
        (*wiki-pp nowiki
          (nil-seq-str "<nowiki>"))
        (*wiki-pp nowiki-e
          (nil-seq-str "</nowiki>"))
        (*wiki-pp arc-tag
          (nil-seq-str "<arc>"))
        (*wiki-pp arc-tag-e
          (nil-seq-str "</arc>"))
        (*wiki-pp ampersand-codes
          (apply alt
            (map seq-str *wiki-ampersand-codes)))
        (*wiki-pp elided-white
          (filt [list #\space] (nil-many1 (pred whitec:car anything))))
        (*wiki-pp arc-code
          (seq arc-tag
               (filt [list (tostring (arc-format-l _))]
                 (many (anything-but arc-tag-e)))
               arc-tag-e))
        (*wiki-pp ampersand-coded-text
          (seq #\& ampersand-codes #\;))
        (*wiki-pp italicized-text
          (seq italics
               (filt in-italics
                     (many:seq (cant-see italics) (delay-parser formatting)))
               italics))
        (*wiki-pp bolded-text
          (seq bold
               (filt in-bold
                     (many:seq (cant-see bold) (delay-parser formatting)))
               bold))
        (*wiki-pp nowiki-text
          (seq nowiki
               (filt-map escape (many:anything-but nowiki-e))
               nowiki-e))
        (*wiki-pp wiki-link
          (filt list:in-wiki-link
            (seq open-br
                 ; article name: elide whitespaces.
                 (filt [list _]
                       (many:seq (cant-see:alt #\| close-br)
                                 (alt elided-white anything)))
                 (maybe:seq
                   (nil-lit #\|)
                   (filt [list _] (many:anything-but close-br)))
                 close-br
                 (filt [list _] (many p-alphadig)))))
        (*wiki-pp formatting
          (alt
            (many1
              (pred [let c (car _)
                      (or (alphadig c)
                          (whitec c)
                          (in c #\, #\.))]
                    anything))
            wiki-link
            bolded-text
            italicized-text
            ampersand-coded-text
            nowiki-text
            arc-code
            (filt-map escape anything)))
        (*wiki-pp many-format
          (many formatting))
        (= print-out
          (fn (s)
            (if (alist s)
                (each c s (print-out c))
                (pr s))))
        (fn (p)
          (print-out:car:parse many-format p))))
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
    (def wiki (op name data meta css scratch   args req)
      " Creates the output of the wiki system. "
      ; filter keys via urlencode
      (with (data data:urlencode
             meta meta:urlencode)
        ; TODO: put these inside *wiki-args form
        (*wiki-args (action title rv start size) args
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
                   ('.topinfo
                     (aif (get-user req)
                       (do
                         ('span.username
                           (link-to (+ "User:" it) it))
                         ('span.loginout
                           (w/rlink (do (logout-user it) this-page)
                             (pr "logout"))))
                       ('span.loginout
                         (w/link (*wiki-page "Login" css
                                   (add-ons)
                                   ('.main
                                     (login-page 'both (+ "Log into " name)
                                       (list nilfn
                                         this-page))))
                           (pr "login")))))
                   ('.topbar
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
                         (w/rlink (+ (string op)
                                     "?title="
                                     (random-elt:articles-only:keys data))
                           (pr "Random Article")))))))
               link-to
               ; creates a link to the specified article
               (fn (l-p text (o l-action) (o l-rv))
                 (zap space->_:capitalize l-p)
                 (withs (rp (urlencode l-p)
                         href (+ (string op) "?title=" rp
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
                 (or= start 0)
                 (or= size 50)
                 (*wiki-page (+ "Revision history of "
                                (_->space p) " - " name) css
                   (add-ons)
                   ('.main
                     ('h1
                       (pr-esc:+ "Revision history of " (_->space p)))
                     (if meta.p
                         (withs (rp (urlencode p)
                                 ht (cut (scan-logs meta.p)
                                        start (+ start size))
                                 hist-link-to
                                 (fn (start size)
                                   (+ "?title=" rp
                                      "&action=hist"
                                      "&start=" (string start)
                                      "&size=" (string size)))
                                 navigation
                                 (fn ()
                                   ('.small
                                     (in-paren
                                       (tostring
                                         (tag-if (> start 0)
                                                 (a href
                                                    (hist-link-to
                                                      (coerce-into
                                                        (- start size) 0)
                                                      size))
                                           (pr "newer"))))
                                     (pr " ")
                                     (in-paren
                                       (tostring
                                         (tag-if (>= (len ht) size)
                                                 (a href
                                                    (hist-link-to
                                                      (+ start size)
                                                      size))
                                           (pr "older"))))
                                     (pr " ")
                                     (in-paren
                                       (tostring
                                         (tag-if (isnt size 50)
                                                 (a href
                                                    (hist-link-to
                                                      start 50))
                                           (pr "50"))))
                                     (pr " ")
                                     (in-paren
                                       (tostring
                                         (tag-if (isnt size 100)
                                                 (a href
                                                    (hist-link-to
                                                      start 100))
                                           (pr "100")))))))
                           (navigation)
                           ('ul
                             (each l ht
                               ('li
                                 ('(a href (+ "?title=" rp
                                              "&rv=" (string l!rv)))
                                   (pr-esc (date l!tm)))
                                 (pr " ")
                                 ('span
                                   (pr-esc l!editby))
                                 (when (and l!log (isnt l!log ""))
                                   (pr " ")
                                   ('i (pr-esc:+ "(" l!log ")"))))))
                           (navigation))
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
                 (with (enformat (enformat-base link-to)
                        formatted scratch!formatted
                        k (list p (or rv (head-rv meta.p))))
                   (pr
                     (or (formatted k)
                         (= (formatted k)
                           (tostring
                             (each p (scan-paras ct)
                               (if
                                 (header-p p)
                                   (header-display enformat p)
                                 (code-block-p p)
                                   (code-block-display enformat p)
                                 ; else
                                   (tag p (enformat p))))))))))
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
            (if start (zap coerce-into start 0))
            (if size (zap coerce-into size 1))
            (if rv (zap coerce-into rv 1))
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
  (w/uniq (d-v m-v n-v c-v s-v req)
    `(with (,d-v ,data
            ,m-v ,meta
            ,n-v ,name
            ,c-v ,css
            ,s-v (table))
       (= (,s-v 'formatted) (cached-table))
       (Arki!add-wiki ',op ',name ',data ',meta ',css)
       (defop ,op ,req
         (Arki!wiki ',op ,n-v ,d-v ,m-v ,c-v ,s-v  (,req 'args) ,req)))))

(ensure-dir "arc/")
(if (file-exists "arc/wiki-arc.conf") (load "arc/wiki-arc.conf"))

; test
(*wiki-def wikitest "Wiki Test"
           (file-table "arc/wiki-test")
           (file-table "arc/wiki-test-meta")
           "wiki-arc/wiki-arc-default.css")


