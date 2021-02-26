(def app-getpath (name)
  (string "apps/" name "/"))

(def app-setsrv (path)
  (= srvdir*    (+ path    "/www/")
     hpwfile*   (+ srvdir* "hpw")
     emailfile* (+ srvdir* "emails")
     oidfile*   (+ srvdir* "openids")
     adminfile* (+ srvdir* "admins")
     cookfile*  (+ srvdir* "cooks")
     logdir*    (+ srvdir* "logs/")  
     staticdir* (+ path    "/static/"))
     (prn (string "serving from " path))
  )

(def app-start (name)
  "Starts application (name). ex: (app-start \"news\")"
  (= approot* ($.arc-normalize-path (app-getpath name))
      apprun* (string approot* "/run-" name ".arc"))

  ; to match run-news,arc, each app needs an arc file
  ; named "run-" appname ".arc" 

  (if (file-exists ($.arc-normalize-path apprun*))
    (do
      (app-setsrv approot*)
      (prn (string "starting app " name ))
      (load apprun*))
    (prn (string "bootstrap file " approot* " not found!")))

; reset the current working path to the arc root so the repl works as expected

    ($ (current-directory (path-only arc-arc-path))))
