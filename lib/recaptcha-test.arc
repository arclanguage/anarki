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

;TODO: handle Google failure codes/bad response/404?

(def recaptcha-response (s r)
  (post-getjson "https://www.google.com/recaptcha/api/siteverify"
                (list 'secret s 'response r)))

;TODO: first, check for the response key, fail if it doesn't exist
;then check for the success key in the response, fail or handle
;error codes if it doesn't exist. 

(def verify-captcha (req)
  (do

; resp should look like
; #hash((challenge_ts . 2018-08-11T01:20:29Z) (hostname . testkey.google.com) (success . t))
; we only care about the value of success

    (= resp (recaptcha-response recaptcha-secret* (alref (req 'args) "g-recaptcha-response")))
    (if resp!success 
      (pr "success") 
      (pr "failure"))))
