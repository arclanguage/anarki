
(def app-start (name (o startfile (string "run-" name ".arc")))
  (= appdir*    (canonical-path-ts (string "apps/" name ))
     apprun*    (+ appdir* startfile))

  ; to match run-news,arc, each app needs an arc file
  ; named "run-" appname ".arc" 

  (if (file-exists apprun*)
    (do
      (prn (string "starting app " name " ... "))
  
  ; reset the variables required by the app server 

       (= srvdir*    (+ appdir* "www/")
          hpwfile*   (+ srvdir* "hpw")
          emailfile* (+ srvdir* "emails")
          oidfile*   (+ srvdir* "openids")
          adminfile* (+ srvdir* "admins")
          cookfile*  (+ srvdir* "cooks")
          logdir*    (+ srvdir* "logs/")  
          staticdir* (+ appdir* "static/"))

        (require apprun*))
    
    (prn (string "bootstrap file " apprun* "not found!"))))

