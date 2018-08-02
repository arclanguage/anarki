(require 'lib/env.arc)       ; environment vars
(require 'lib/srv.arc)       ; server
(require 'lib/html.arc)      ; html
(require 'lib/recaptcha.arc) ; recaptcha

(=
  quitsrv*   nil 
  breaksrv*  nil

  srvdir*    "www/"  
  logdir*    (+ srvdir* "logs/")  
  staticdir* "static/"
)

(thread (serve 8080))
(sleep 3)

(defop ||  req (aform form-handler 
  (do
    (recaptcha-form)
    (single-input "enter" 'foo 10 "Submit")
  )))

(def form-handler (req) 
  (prn "You entered") 
    (prbold (alref (req 'args) "foo")))
