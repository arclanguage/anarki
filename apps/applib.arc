(def app-getpath (name)
  (string "apps/" name "/"))

(def app-setsrv (path)
  (= srvdir*    (+ path    "www/")
     hpwfile*   (+ srvdir* "hpw")
     emailfile* (+ srvdir* "emails")
     oidfile*   (+ srvdir* "openids")
     adminfile* (+ srvdir* "admins")
     cookfile*  (+ srvdir* "cooks")
     logdir*    (+ srvdir* "logs/")  
     staticdir* (+ path    "static/")))

(def app-start (name)
  "Starts application (name). ex: (app-start \"news\")"
  (= apprun* (string "run-" name ".arc"))

  ; to match run-news,arc, each app needs an arc file
  ; named "run-" appname ".arc" 

  (if (file-exists ($.normalize-path apprun*))
    (do
      (app-setsrv "")
      (prn (string "starting app " name))
      (load apprun*))
    (prn (string "bootstrap file " apprun* " not found!"))))
