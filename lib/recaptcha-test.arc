(require 'lib/client.arc)    ; http client
(require 'lib/env.arc)       ; environment vars
(require 'lib/srv.arc)       ; server
(require 'lib/html.arc)      ; html
(require 'lib/json.arc)      ; json
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

(defop || req 
  
  (do
    (pr "<script src=\"https://www.google.com/recaptcha/api.js\" async defer></script>")
    (pr "")
    (aform verify-captcha
    (pr "<div class=\"g-recaptcha\" data-sitekey=\"" recaptcha-sitekey* "\"></div>")
    (single-input "Enter:" 'foo 10 "Submit"))))

(def verify-captcha (req)
  (pr 
    (post-url 
    "https://www.google.com/recaptcha/api/siteverify"
    (list 'secret recaptcha-secret* 'response (alref (req 'args) "g-recaptcha-response")))))
