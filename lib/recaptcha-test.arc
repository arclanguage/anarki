(require '(
 lib/env.arc       ; environment vars
 lib/srv.arc       ; server
 lib/html.arc      ; html
; lib/recaptcha.arc  recaptcha
))


(=
  quitsrv*   nil 
  breaksrv*  nil

  srvdir*    "www/"  
  logdir*    (+ srvdir* "logs/")  
  staticdir* "static/"
)

(thread (serve 8080))
(sleep 3)

(defop || (prn "hi"))

	
