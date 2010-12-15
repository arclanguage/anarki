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
            (td (ulink user 'edit   (edit-app user app)))
            (td (ulink user 'run    (run-app  user app)))
            (td (hspace 40)
                (ulink user 'delete (rem-app  user app))))))
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
        (readfile it)))

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

