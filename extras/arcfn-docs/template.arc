(load "ppr.arc")

(= hpasswords* (table))

(def start-table (hdr) (prn "<p><table class='arc'>")
;(prn "<tr><th class='arc' colspan=2>" hdr "</th></tr>")
)

(def codelinkencode (str)
   (subst "%2f" "/"
     (subst "%2b" "+"
       (subst "%3c" "<"
         (subst "%3e" ">"
           (subst "%2a" "*"
	     (subst "%3f" "?"
	       (subst "%2f" "%"
	   str))))))))

(def end-table () (prn "</table>"))

(def errproc (ex) (prn "Error: " (details ex)) 'err)

(def html-esc (str) (subst "&gt;" ">" (subst "&lt;" "<" (subst "&amp;" "&" str))))
(def url-esc (str) (subst "%20" " " (subst "%2b" "+" (subst "%3d" "=" (coerce str 'string)))))
(def anchor-esc (str) (url-esc (subst "_" " " (coerce str 'string))))


(def copy-file (filename)
  (let inf (infile filename)
    (while (= line (readline inf))
      (if (posmatch "%%INDEX%%" line)
        (disp (subst (getlinkindexhtml) "%%INDEX%%" line))
        (disp (subst links* "%%LINKS%%" (subst (string "Arc: " page*) "%%TITLE%%" line))))
      (prn)
      )
    (close inf)))

(def faketest (input result)
  (prn "<pre>")
  (pr (html-esc ">"))
  (prn (html-esc input))
  (prn (html-esc result))
  (prn "</pre>"))

(def dotest (testcode) (dotestint 'normal testcode))
(def dohtmltest (testcode) (dotestint 'html testcode))

(def dotestint (testtype testcode)
 (let show nil
  (prn "<pre>")
  (pr (html-esc ">"))

  (when (caris testcode 'show)
    (= show t)
    (= testcode (cdr testcode))
    (if (is (type (car testcode)) 'string) (= testcode (car testcode))))
  ;If a string, disp and get obj, else write
  (if (is (type testcode) 'string)
    (do (disp testcode) (= testcode (sread (instring testcode) 'eof)))
    (pr (html-esc (tostring (pprint testcode)))))

  (prn)

  (= sop (outstring))
  (call-w/stdout sop (fn ()
    (= result (on-err errproc (fn () (eval testcode))))))
  (= stdout-val (inside sop))

  ; Hack around bad return value
  (= sop (outstring))
  (write result sop)
  (= result-string (inside sop))
  (if (or (is result-string "#t")
          (is result-string "#f")
	  (is result-string "#<thread>"))
        (= result result-string))

  (if (no (is "" stdout-val)) (spanclass "stdout" (prn (html-esc (splitstring stdout-val 60)))))
  (when (and (isnt 'html testtype) (isnt 'err result))
    (spanclass "return" (prn (html-esc result-string))))
  (prn "</pre>")
  (when show
    (prn "<hr/>")
    (prn stdout-val))
  ))

(= desc "")

(def intags (tag tags) (if (no tags) nil
                           (is tag (car tags)) t
                           (intags tag (cdr tags))))


(def op (args)
  (with (tag nil tags nil operation nil arglist nil desc nil testlist nil)
    (push (pop args) tags)
    (= tag (pop args))
    (while (in tag 'destructive 'predicate)
       (push tag tags)
       (= tag (pop args)))
    (= operation tag)
    (push operation tags)
    (= arglist (pop args))
    (= desc (pop args))
    (= testlist args)
    (prn "  <tr>")
    (pr "    <td class='arc'>")
    (add-anchor2 (coerce operation 'string))
    (if (no (intags 'nolink tags))
      (prn "<a target='CODE' href='/src/"  (subst "%2f" "%" (codelinkencode (string operation))) ".html'><img src='code.gif' title='code'/></a>"))
    (if (intags 'mac tags) (prn "<img src='macro.gif' title='Macro'/>"))
    (if (intags 'op tags) (prn "<img src='foundation.gif' title='Foundation'/>"))
    (if (intags 'def tags) (prn "<img src='proc.gif' title='Procedure'/>"))
    (if (intags 'var tags) (prn "<img src='var.gif' title='Variable'/>"))
    (if (intags 'destructive tags) (prn "<img src='destructive.gif' title='Destructive'/>"))
    (if (intags 'predicate tags) (prn "<img src='predicate.gif' title='Predicate'/>"))

    (if (intags 'nolink tags) (pr "<span class='op'>" operation "</span> ")
                          (pr "<a class='op' href='http://practical-scheme.net/wiliki/arcxref?" (url-esc operation) "'>" operation "</a> "))
    (prn "<span class='args'>" arglist "</span>")
    (prn "    <div class='desc'>" desc "</div>")
    (prn "    </td>")
    (pr "    <td class='arc'>")
    (if (no testlist) nil
	(no (testlist 0)) nil
	(is 'faketest ((testlist 0) 0)) (faketest ((testlist 0) 1) ((testlist
	0) 2))
	(is 'tests ((testlist 0) 0)) (map dotest (cdr:car testlist))
	(is 'htmltests ((testlist 0) 0)) (map dohtmltest (cdr:car testlist))
        (err "Expected tests" operation (car testlist) ))
    (prn "  </td></tr>")
)
    )

(def pair? (x) (is (type x) 'cons))

; all-links* = ((foo.tem item1 item2) (bar.tem item1 ...))
(= all-links* '())
(= update-links* nil)
(def add-index1 (link)
     (if update-links*
       (= all-links* (+ all-links* (list (list link))))))

(def add-index2 (link)
    (if update-links*
     (let lastelt (- (len all-links*) 1)
       (= (all-links* lastelt) (+ (all-links* lastelt) (list link))))))

(def add-anchor1 (link)
  (add-index1 current-file*)
  ;(prn "<a name='" (anchor-esc link) "'></a>")
)

(def add-anchor2 (title)
  (add-index2 title)
  (prn "<a name='" (anchor-esc title) "'></a>")
)

(def newtable (title . contents) 
  (hdr title)
  (when (and (pair? contents)
           (pair? (car contents)))
	   (if (is ((car contents) 0) 'text)
	     (do (text ((car contents) 1)) (= contents (cdr contents))))
	   (if (is ((car contents) 0) 'import)
	     (do (copy-file ((car contents) 1)) (= contents (cdr contents)))))
  (start-table title)
  (map doit contents)
  (end-table)
  )

(def page (title . contents) 
  (= page* title)
  (copy-file "docs/hdr2.html")
  (add-anchor1 (coerce title 'string))
  (map doit contents)
  (copy-file "footer.txt")
  )

(def top-page (title . contents) 
  (= page* title)
  (= links* "")
  (copy-file "docs/hdr2.html")
  (map doit contents)
  (copy-file "footer.txt")
  )

(def hdr (msg) (prn "<h2>" msg "</h2>"))
(def text (msg) (prn msg))

; Make the index
(def index () 
  (prn "<h2>Index</h2>")
  (on links all-links*
    (with (filename (htmlname (links 0)) title ((index* (links 0)) 0))
      (prn "<div class=\"toplink\">")
      (prn "<a class=\"toplink\" href=\"" filename "\">" title "</a>")
      (prn "</div>")
      (prn "<div class=\"sublink\">")
      (on link (cdr links)
        (prn "<a class=\"sublink\" href=\"" filename "#" (anchor-esc link)  "\">" link "</a>"))
      (prn "</div>")
	)))

(= out-file-name* "")
(= out-file* (stdout))
(def doit (arg)
 (with (cmd (car arg) args (cdr arg))
  (w/stdout out-file*
    (if (is 'newtable cmd) (apply newtable args)
        (is 'page cmd) (apply page args)
        (is 'top-page cmd) (apply top-page args)
        (is 'text cmd) (apply text args)
        (is 'op cmd) (op (cons 'op args ))
        (is 'mac cmd) (op (cons 'mac args))
        (is 'def cmd) (op (cons 'def args))
        (is 'var cmd) (op (cons 'var args))
        (is 'op-nolink cmd) (op (join '(op nolink) args))
	(is 'index cmd) (index)
        (is 'import cmd) (copy-file (args 0))
        (is 'template) nil
        (is 'file cmd) (do (= out-file-name* (args 0))
			 (= out-file* (outfile (arg 1))))
        (err "Expected a template operation" arg)
      ))))


(def triples (xs (o n 3))
     (if (or (no xs) (< (len xs) n))
       nil
       (cons (firstn n xs)
	     (triples (cdr xs) n))))

; Return just the templates from filename
(def gettemplates (filename)
  (keep (fn (x) (and (acons x) (is (x 0) 'template))) (sread (infile filename) 'eof)))

(def safematch (pat str (o start 0))
  (catch
    (for i start (- (len str) (len pat))
       (if (is (cut str i (+ i (len pat))) pat) (throw i)))
    nil))

     
; Create list of (text link):
; Extract <a href="foo.html">text</a> and convert to (text "foo.tem")
(def gettemlinks (filename)
 (let result nil
  (w/infile inf filename (w/stdin inf (whilet line (readline)
     (let m (safematch "href=\"" line)
        (when m
          (++ m 6)
          (withs (n (safematch "\">" line (+ m 1))
                  o (safematch "<" line (+ m 1))
                  link (cut line m n)
                  link2 (subst ".tem" ".html" link))
	    (when (and n o (< n o))
	      (let text (cut line (+ n 2) o)
	        (when (and (isnt link link2) (file-exists link2))
		  (= result (+ result (list (list text link2)))))))))))))
result))

(def readtop (filename)
     ; (index* foo.tem) == (prev.tem this.tem next.tem)
     ; pagelist* == ("page1.tem" "page2.tem" ...)
     (= index* (table))
     (let temlinks (gettemlinks filename)
       (= pagelist* (map [_ 1] temlinks))
       (each (title link) temlinks
          (= (index* link) title)) ; (index* foo.tem) = title
       (each trip (triples (+ '(nil) pagelist* '(nil)))
         (= (index* (trip 1)) (cons (index* (trip 1)) trip))
       )))

(def htmllink (href text) (prn "<a href=\"" href "\">" text "</a>"))

; Convert foo.tem to foo.html
(def htmlname(x) (subst ".html" ".tem" x))

(def getlinks (tem)
     (let info (index* tem)
      (if info
       (with (text (info 0) prev (info 1) this (info 2) next (info 3))
     (tostring (pr "<div class=\"links\">")
	       (when prev
		 (pr "Previous: ")
		 (htmllink (htmlname prev) ((index* prev) 0)))
	       (pr "Up: ")
	       (htmllink "index.html" "Contents")
	       (when next
		 (pr "Next: ")
		 (htmllink (htmlname next) ((index* next) 0)))
	       (pr "</div>")
     )
     ))))

(= index* nil)

; Generate html from a template
(def run (filename)
        (= current-file* filename)
        (= out-file-name* (+ "html/" (subst "" ".tem" filename) ".html"))
	(= page* "")
        (= out-file* (outfile out-file-name*))
	(when (no index*)
	  (prn "Reading top.tem")
	  (readtop "top.tem"))
	(= links* (getlinks filename))
	(doit (sread (infile filename) 'eof))
        out-file-name*
)

; Run through all the templates listed in the top file.
(def runall ((o topname "docs/index.html"))
     (= all-links* '())
     (readtop topname)
     (= update-links* t)
     (on filename pagelist*
	 (prn "Doing file " filename)
	 (run filename))
     (= update-links* nil)
	 )

(def tem () (load "template.arc"))

(def alpha (c) (<= #\a c #\z))

(def getlinkindexhtml ()
 (tostring
  (with (idx (getlinkindex) first nil)
   (each (name link) idx
    (let newfirst (name 0)
     (when (and (alpha newfirst) (isnt first newfirst))
       (= first newfirst)
       (prn "<h2>" first "</h2>"))
     (prn "<a href='" link "'>" name "</a>"))))))

(def getlinkindex ()
 (mergesort (fn (x y) (< (car x) (car y)))
  (accum accfn
   (each links all-links*
    (let file (+ (subst ".html" ".tem" (car links)) "#")
     (each link (cdr links)
      (accfn (list link (+ file (codelinkencode link))))))))))

(def dumplinks ()
 (w/outfile of "dumplinks" (w/stdout of
  (each links all-links*
    (each link (cdr links)
       (prn (codelinkencode link)))))))

; Find index of last location <= pos where chr appears in str, or nil
(def rmatch (str chr (o pos 99999))
  (let minpos (min pos (- (len str) 1))
    (if (< minpos 0) nil
        (is (str minpos) chr) minpos
	 (rmatch str chr (- minpos 1)))))

; Split string at length n or earlier, splitting at a <
(def splitstring (str (o maxlen 60) (o splitchar #\<))
  (if (len< str maxlen) str
      (let pos (aif (rmatch str splitchar maxlen) (if (> it 0) it maxlen) maxlen)
       (+ (cut str 0 pos) "\n" (splitstring (cut str pos) maxlen splitchar)))))
