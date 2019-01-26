; managed script editor, based on prompt.arc

(= mgappdir* "apps/managed")
   
(defop appeditor req
  (let user (get-user req)
    (if (admin user)
      (appeditor-page user)
        (pr "Unknown"))))

(def appeditor-page (user . msg)
  
  (ensure-dir mgappdir*)

  (whitepage
    (prbold "Script Editor")
    (sp)
    (hspace 20)
    (pr user " | ")
    (link "logout")

    (when msg (hspace 10) (apply pr msg))

    (br2)
    
    (aform (fn (req)
      (aif (goodname (arg req "app")) ; check if file exists
           (appmgr-edit user it)
           (appeditor-page user "Bad name.")))
     
    (tag table 
      (row "name:" (input "app") (submit "create"))))

    (tag (table border 0 cellspacing 10)
      (each app (dir mgappdir*)
        (tr (td app)
            (td (ulink user 'edit   (appmgr-edit user app)))
            (td  (if (hooks* app)
                  (ulink user 'unhook (appmgr-unhook user app))
                  (ulink user 'hook (appmgr-hook user app))))
            (td (ulink user 'run (appmgr-page user app)))
            (td (hspace 40)
                (ulink user 'delete (appmgr-delete user app))))))

;    (pr hooks*)

    (prbold "Repl")    
    
    (aform (fn (req) 
      (do 
        (each expr (readall (or (arg req "expr") ""))
              (on-err (fn (c) (push (list expr c t) repl-history*))
                      (fn ()
                        (= that (eval expr) thatexpr expr)
                        (push (list expr that) repl-history*))))
        (appeditor-page (get-user req) 'nil)))
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

(def appmgr-filepath (app)
  (string mgappdir* app))

(def appmgr-read (app)
  (aand (appmgr-filepath app)
        (file-exists it)
        (readfile it)))

(def appmgr-write (app exprs)
  (awhen (appmgr-filepath app)
    (w/outfile o it
      (each e exprs (write e o)))))

;Deleting an app should also unhook it. 

(def appmgr-delete (user app)
  (let file (appmgr-filepath app)
    (if (file-exists file)
      (do (= (hooks* app) 'nil)
          (rmfile (appmgr-filepath app))
          (appeditor-page user "Program " app " deleted."))
      (appeditor-page user "No such app."))))

(def appmgr-edit (user app)
  (whitepage
    (pr "app:" app)
    (br2)
    (aform (fn (req)
       (do (when (is (arg req "cmd") "save")
             (appmgr-write app (readall (arg req "exprs"))))
           (appeditor-page user)))
      (textarea "exprs" 10 82
        (each e (appmgr-read app)
          (ppr e)
          (pr "\n\n")))
      (br2)
      (buts 'cmd "save" "cancel"))))

(def appmgr-view (user app)
  (whitepage
    (pr "app:" app)
    (br2)
    (tag xmp (pprcode (appmgr-read app)))
    (br2)
      (link "appeditor")))

(def appmgr-run (app)
  (map eval (appmgr-read app)))

(def appmgr-page (user app)
  (whitepage
    (pr "app:" app)
    (br2)
      (appmgr-run app)
    (br2)
      (link "appeditor")))

(def appmgr-unhook (user app)
  (= (hooks* app) 'nil)
  (appeditor-page user 'nil))

; The hook name and the file name should match

(def appmgr-hook (user app) 
  (if (file-exists (appmgr-filepath app))
    (= (hooks* app) `(appmgr-run ,app)))
  (appeditor-page user 'nil))

