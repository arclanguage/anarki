(require 'lib/files.arc)
(require 'lib/srv.arc)

(= mgappdir* (qualified-path (+ srvdir* "../../managed/")))
   
(defop editor req
  (let user (get-user req)
    (if (admin user)
      (editor-page user)
        (pr "Unknown"))))

(def editor-page (user . msg)
  
  (ensure-dir mgappdir*)

  (whitepage
    (prbold "App Editor")
    (sp)
    (hspace 20)
    (pr user " | ")
    (link "logout")

    (when msg (hspace 10) (apply pr msg))

    (br2)
    (aform (fn (req)
      (aif (goodname (arg req "app")) ; check if file exists
           (app-edit user it)
           (editor-page user "Bad name.")))
     
    (tab (row "name:" (input "app") (submit "create app"))))

    (tag (table border 0 cellspacing 10)
      (each app (dir mgappdir*)
        (tr (td app)
            (td (ulink user 'edit   (app-edit user app)))
            (td  (if (hooks* app)
                  (ulink user 'unhook (app-unhook user app))
                  (ulink user 'hook (app-hook user app))))
            (td (ulink user 'run (app-page user app)))
            (td (hspace 40)
                (ulink user 'delete (app-delete user app))))))

    (prbold "Repl")    
    
    (aform (fn (req) 
      (do 
        (each expr (readall (or (arg req "expr") ""))
              (on-err (fn (c) (push (list expr c t) repl-history*))
                      (fn ()
                        (= that (eval expr) thatexpr expr)
                        (push (list expr that) repl-history*))))
        (editor-page (get-user req) nil)))
      (textarea "expr" 8 60)
      (sp)
      (submit))
    (tag xmp
      (each (expr val err) (firstn 20 repl-history*)
        (pr "> ")
        (ppr expr)
        (prn)
        (prn (if err "Error: " "")
             (ellipsize (tostring (write val)) 800))))))

(def app-filepath (app)
  (qualified-path (string mgappdir* app)))

(def app-read (app)
  (aand (app-filepath app)
        (file-exists it)
        (readfile it)))

(def app-write (app exprs)
  (awhen (app-filepath app)
    (w/outfile o it
      (each e exprs (write e o)))))

;Deleting an app will also unhook it. 

(def app-delete (user app)
  (let file (app-filepath app)
    (if (file-exists file)
      (do (= (hooks* app) nil)
          (rmfile (app-filepath app))
          (editor-page user "Program " app " deleted."))
      (editor-page user "No such app."))))

(def app-edit (user app)
  (whitepage
    (pr "app:" app)
    (br2)
    (aform (fn (req)
       (do (when (is (arg req "cmd") "save")
             (app-write app (readall (arg req "exprs"))))
           (editor-page user)))
      (textarea "exprs" 10 82
        (each e (app-read app)
          (ppr e)
          (pr "\n\n")))
      (br2)
      (buts 'cmd "save" "cancel"))))

(def app-view (user app)
  (whitepage
    (pr "app:" app)
    (br2)
    (tag xmp (pprcode (app-read app)))
    (br2)
      (link "editor")))

(def app-run (app)
  (map eval (app-read app)))

(def app-page (user app)
  (whitepage
    (pr "app:" app)
    (br2)
      (app-run app)
    (br2)
      (link "editor")))

(def app-unhook (user app)
  (= (hooks* app) nil)
  (editor-page user nil))

; The hook name and the app name (file name) are the same.
; To use an existing News hook, name the app as the hook.

(def app-hook (user app) 
  (if (file-exists (app-filepath app))
    (= (hooks* app) `(app-run ,app)))
  (editor-page user nil))

