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
  (= appdir* (app-getpath name)
     apprun* (+ appdir* (string "run-" name ".arc")))

  ; to match run-news,arc, each app needs an arc file
  ; named "run-" appname ".arc" 

  (if (file-exists ($.normalize-path apprun*))
    (do
      (app-setsrv (string appdir* (path-separator))) ; this needs the end separator
      (prn (string "starting app " name " from " appdir*))
      (require apprun*))
    (prn (string "bootstrap file " apprun* "not found!"))))

(def app-create (name)
  "Creates a skeleton web application."
   (= appdir* (app-getpath name) 
      appcmd* ($.normalize-path "/apps/appcmd.arc"))

   (unless (dir-exists appdir*)
      (app-setsrv appdir*)
      ; create server directories and static directory
      (ensure-srvdirs)
      (ensure-dir (+ appdir* "static/")))

    ; windows start
    (textfile-write (string appdir* "run-" name ".cmd") 
      "@ECHO OFF"
      "SETLOCAL ENABLEEXTENSIONS"
      "pushd \"%~dp0\""
      (string "../../arc.cmd -i ../appcmd.arc \"app-start\" " "\"" name "\"")
      "popd"
      "ENDLOCAL")

    ; linux start
    (textfile-write (string appdir* "run-" name ".sh") 
      "#!/bin/bash"
      "#change to the directory of the script"
      "cd $(dirname \"$0\")"
      (string "../../arc.sh -i ../appcmd.arc \"app-start\" " "\"" name "\""))

    ; bootstrap
     (textfile-write (string appdir* "run-" name ".arc") 
      (string "(require \"apps/" name "/" name ".arc\")")
      "(thread (asv 8080)) ; run in a thread so repl remains usable"
      "(sleep 3)  ; wait for asv's initial messages to appear before printing first prompt")

    ; main
    (textfile-write (string appdir* name ".arc")
      "(defop || req (pr \"This is the home page.\"))"))