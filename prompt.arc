; Prompt: Web-based programming application.  4 Aug 06.

(= appdir* "arc/apps/")

(defop prompt req 
  (let user (get-user req)
    (if (admin user)
        (prompt-page user)
        (pr "Sorry."))))

(def prompt-page (user . msg)
  (ensure-dir appdir*)
  (ensure-dir (string appdir* user))
  (whitepage
    (prbold "Prompt")
    (hspace 20)
    (pr user " | ")
    (link "logout")
    (when msg (hspace 10) (apply pr msg))
    (br2)
    (tag (table border 0 cellspacing 10)
      (each app (dir (+ appdir* user))
        (tr (td app)
            (td (userlink user 'edit   (edit-app user app)))
            (td (userlink user 'run    (run-app  user app)))
            (td (hspace 40)
                (userlink user 'delete (rem-app  user app))))))
    (br2)
    (aform (fn (req)
             (when-umatch user req
               (aif (goodname (arg req "app"))
                    (edit-app user it)
                    (prompt-page user "Bad name."))))
       (tab (row "name:" (input "app") (submit "create app"))))))

(def app-path (user app) 
  (and user app (+ appdir* user "/" app)))

(def read-app (user app)
  (aand (app-path user app) 
        (file-exists it)
        (w/infile i it (readall i))))

(def write-app (user app exprs)
  (awhen (app-path user app)
    (w/outfile o it 
      (each e exprs (write e o)))))

(def rem-app (user app)
  (let file (app-path user app)
    (if (file-exists file)
        (do (rmfile (app-path user app))
            (prompt-page user "Program " app " deleted."))
        (prompt-page user "No such app."))))

(def edit-app (user app)
  (whitepage
    (pr "user: " user " app: " app)
    (br2)
    (aform (fn (req)
             (let u2 (get-user req)
               (if (is u2 user)
                   (do (when (is (arg req "cmd") "save")
                         (write-app user app (readall (arg req "exprs"))))
                       (prompt-page user))
                   (login-page 'both nil
                               (fn (u ip) (prompt-page u))))))
      (textarea "exprs" 10 82
        (pprcode (read-app user app)))
      (br2)
      (buts 'cmd "save" "cancel"))))

(def pprcode (exprs) 
  (each e exprs
    (ppr e) 
    (pr "\n\n")))

(def view-app (user app)
  (whitepage
    (pr "user: " user " app: " app)
    (br2)
    (tag xmp (pprcode (read-app user app)))))

(def run-app (user app)
  (let exprs (read-app user app)
    (if exprs 
        (on-err (fn (c) (pr "Error: " (details c)))
          (fn () (map eval exprs)))
        (prompt-page user "Error: No application " app " for user " user))))

(wipe repl-history*)

(defop repl req
  (if (admin (get-user req))
      (replpage req)
      (pr "Sorry.")))

(def replpage (req)
  (whitepage
    (repl (readall (or (arg req "expr") "")) "repl")))

(def repl (exprs url)
    (each expr exprs 
      (on-err (fn (c) (push (list expr c t) repl-history*))
              (fn () 
                (= that (eval expr) thatexpr expr)
                (push (list expr that) repl-history*))))
    (form url
      (textarea "expr" 8 60)
      (sp) 
      (submit))
    (tag xmp
      (each (expr val err) (firstn 20 repl-history*)
        (pr "> ")
        (ppr expr)
        (prn)
        (prn (if err "Error: " "")
             (ellipsize (tostring (write val)) 800)))))

(defop help req
  (withs
    (
      rawargs (listtab:req 'args)
      args [coerce (rawargs _) 'string]
      nonempty [if (isnt "" _) _]
      enempty [if (no _) "" _]
      ;not yet implemented in base libs as of this writing.
      urlencode
      (fn (s)
        (let code [coerce _ 'int 16]
          (tostring
            (forlen i s
              (if (some (s i)
"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890")
                  (pr (s i))
                (is (s i) #\space)
                  (pr "+")
                  (pr "%" (coerce (code:s i) 'string 16)))))))
      enformat
      (fn (s)
        ;inter-recursion needed, so just assign nothing
        (let (get normal one-bracket inlink one-close
            emit-char out-char build build2
            bldg emit-link) nil
          (=
            get
            (with (i 0 l (len s))
              (fn () (if (< i l) (do1 (cut s i (+ 1 i)) (++ i)))))
            normal
            (fn ()
              (caselet c (get)
                nil nil
                "[" (emit-char c one-bracket)
                  (emit-char c normal)))
            one-bracket
            (fn ()
              (caselet c (get)
                nil nil
                "[" (emit-char c inlink)
                  (emit-char c normal)))
            inlink
            (fn ()
              (caselet c (get)
                nil nil
                "]" (one-close)
                  (build c inlink)))
            one-close
            (fn ()
              (caselet c (get)
                nil nil
                "]" (emit-link (fn () (pr "]]") (normal)))
                  (build2 "]" c inlink)))
            emit-char
            (fn (c next)
              (out-char c)
              (next))
            out-char
            (fn (c)
              (case c
                "<"   (pr "&lt;")
                ">"   (pr "&gt;")
                "&"   (pr "&amp;")
                "\""  (pr "&quot;")
                "/"   (pr "&frasl;")
                " "   (nbsp)
                "\n"  (br)
                (pr c)))
            build
            (fn (c next) (push c bldg) (next))
            build2
            (fn (c1 c2 next) (push c1 bldg) (push c2 bldg) (next))
            emit-link
            (fn (next)
              (let ln (apply + (rev bldg))
                (link ln
                  (if (help* (coerce ln 'sym))
                    (+ "?sym=" (urlencode ln))
                    (+ "?str=" (urlencode ln)))))
              (wipe bldg)
              (next)))
          (tag (code) (normal)) )))
    (tag (html)
      (tag (head)
        (tag (title)
          (aif (nonempty:args "sym") (pr it " - ")
            (nonempty:args "str") (pr "\"" it "\" - "))
          (pr "Online help")))
      (tag (body)
        (aif
          (nonempty:args "sym")
            (enformat (eval `(tostring:help ,(coerce it 'sym))))
          (nonempty:args "str")
            (prall
              ;mysteriously, at the time of this writing, (+ "?sym=" _) fails
              (map
                (compose
                  [tostring
                    (link (eschtml _)
                      (tostring (pr "?sym=" (urlencode _))))]
                  string)
                (helpsearch-core (coerce it 'string)))
              "Related symbols:<br>" "<br>")
            (pr "Welcome to online help. "
              "Enter your search below.") )
        ;Use 'tag because we want to use get method for
        ;this form, in order to allow the user to bookmark
        ;search results.
        (tag (form method 'get action 'help)
          (inputs "sym" "Search Function "
            20 (enempty:args "sym")
            "str" "Search Docstrings "
            20 (enempty:args "str") )
          (br)
          (submit "Search"))
        ;dummy form to force reload of this operation
        (tag (form method 'get action 'help)
          (submit "Clear") )))))


