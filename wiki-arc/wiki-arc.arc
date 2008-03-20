; Arkani
; by AmkG
; A wiki, in Arc.  The goal is to have a similar
; feature set to mediawiki (discussion pages, history,
; similar markup rules, templates, "What links here").

(require "whtml.arc")
(require "lib/settable-fn.arc")
(require "wiki-arc/wikiconf.arc")
(require "wiki-arc/diff.arc")
(require "lib/file-table.arc")
(require "lib/scanner.arc")
(require "lib/treeparse.arc")

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
  `(w/html
     ('head
       ('title (pr ,title))
       (if (and css (isnt "" css) (file-exists css))
           ('(link rel "stylesheet"
                   type "text/css"
                   href (flink (fn args
                                 (w/infile s css
                                   (whilet c (readc s) (pr c)))))))))
     ('body ,@body)))

; wiki-arc module
(= Arkani
  (let (help* sig source-file*
        wiki add-wiki wikis
        _->space space->_ isdigit pr-esc
        scan-words scan-logs scan-paras
        serialize
        new-log head-rv get-rv save-page
        urlencode
        header-p header-display
        enformat) nil
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
      (case c
        #\< (pr "&lt;")
        #\> (pr "&gt;")
        #\& (pr "&amp;")
        #\" (pr "&quot;")
            (pr c)))
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
    (def header-display (p link-to)
      (with (num 0
             sp p)
        ; count the number of ='s
        (while (is (car sp) #\=)
          (zap cdr sp)
          (++ num))
        (zap cdr sp)
        (pr "<h" num ">")
        (enformat (cut sp 0 (- (+ 1 num))) link-to)
        (pr "</h" num ">")))
    ; format a single paragraph
    ; ~!TODO: Change this to handle formatting
    (def enformat (p link-to)
      (let (; extensions to treeparse
            enclose-sem
            ; actions
            on-plain-wiki-link in-italics in-bold
            ; parsers
            open-br close-br italics bold italicized-text bolded-text
            plain-wiki-link formatting) nil
        ; extensions to treeparse
        (= enclose-sem
           (fn (f p)
             " Adds semantics like 'sem, but encloses the semantics
               defined by inner parsers "
             (fn (r)
               (iflet enc (parse p r)
                      (return (enc 0) (enc 1)
                              (list
                                (fn () (f enc))))))))
        ; actions
        (= on-plain-wiki-link
           [let s (string _)
               (link-to s s)])
        (w/html-tags
          (= in-italics
               ['i (carry-out _)])
          (= in-bold
               ['b (carry-out _)]))
        ; parsers
        (= open-br
          (seq #\[ #\[))
        (= close-br
          (seq #\] #\]))
        (= italics
          (seq #\' #\' #\'))
        (= bold
          (seq #\' #\'))
        (= italicized-text
          (seq italics (enclose-sem in-italics (many (seq (cant-see italics) (delay-parser formatting)))) italics))
        (= bolded-text
          (seq bold (enclose-sem in-bold (many (seq (cant-see bold) (delay-parser formatting)))) bold))
        (= plain-wiki-link
          ; should really be (many anything), however parsecomb.arc
          ; currently does not do backtracking on 'many
          (seq open-br (sem on-plain-wiki-link (many (anything-but #\]))) close-br))
        (= formatting
          (alt
            plain-wiki-link
            italicized-text
            bolded-text
            (sem pr-esc:car anything)))
        (carry-out (parse (many formatting) p))))
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
                link-to
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
               add-ons
               (fn ()
                 (w/html-tags
                   ; maybe float: right
                   ('.topinfo
                     (aif (get-user req)
                       ('span.username (pr it))
                       ('span (pr "not logged in"))))
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
                   ('.sidebar ('(a href "?title=Main_Page") (pr "Main Page")))))
               link-to
               ; creates a link to the specified article
               (fn (p text (o action) (o rv))
                 (withs (rp (urlencode:space->_ p)
                         href (+ "?title=" rp
                                 (if action
                                     (+ "&action=" (urlencode:string action))
                                     "")
                                 (if rv
                                     (+ "&rv=" (string rv))
                                     "")))
                   (w/html-tags
                     (if (some rp (keys data))
                       ('(a href href)
                         (pr text))
                       ('(a.deadlink href href)
                         (pr text))))))
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
                            ('p (pr it)))
                          (let ct (get-rv data meta p rv)
                            ('h1
                              (pr "Editing " (_->space p)))
                            (when (and rv (isnt rv (head-rv meta.p)))
                               ('.warning
                                 (pr "This is an old revision of this page")
                                 #|
                                 (pr ", as edited by ")
                                 ('b (pr (editor-rv meta p rv)))
                                 (pr " on ")
                                 ('b (pr (date (time-rv meta p rv))))
                                 (pr ".")|#))
                            (arform [edit-target req!ip _]
                              ; should have id/class?
                              ('(textarea name 'ct rows 25 cols 80)
                                (when ct (pr ct)))
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
                       (pr "Revision history of " (_->space p)))
                     (if meta.p
                         ('ul
                           (let ht (scan-logs meta.p)
                             (each l ht
                               ('li
                                 ('(a href (+ "?title=" (urlencode p)
                                              "&rv=" (string l!rv)))
                                   (pr (date l!tm)))
                                 (pr " ")
                                 ('span
                                   (pr l!editby))
                                 (when (and l!log (isnt l!log ""))
                                   (pr " ")
                                   ('i (pr "(" l!log ")")))))))
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
                       (pr name
                           " does not have an article with this exact name. ")))
                   ('ul
                     ('li
                       ('(a href (+ "?title=" (urlencode p) "&action=edit"))
                          ('b (pr "Start the ")
                              ('i (pr (eschtml p)))
                              (pr " article")))
                          (pr ".")))))
               display-content
               (fn (ct)
                 (each p (scan-paras ct)
                   (if
                     (header-p p)
                       (header-display p link-to)
                     ; else
                       (tag p (enformat p link-to)))))
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
                                 (pr "This is an old revision of this page")
                                 #|
                                 (pr ", as edited by ")
                                 ('b (pr (editor-rv meta p rv)))
                                 (pr " on ")
                                 ('b (pr (date (time-rv meta p rv))))
                                 (pr ".")|#))
                             ('h1 (pr (_->space p)))
                             (display-content ct))))))))
            ; body
            (if (or (no title) (is title "")) (= title "Main Page"))
            (zap space->_ title)
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


