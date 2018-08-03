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

; send a POST request, return a hashtable from an expected JSON response
(def post-getjson (url params (o port stdin))
  (fromstring (post-url url params) 
    (read-json (port))))

; #hash((challenge_ts . 2018-08-03T12:59:44Z) (hostname . testkey.google.com) (success . #t))
; TODO: handle failure codes
(def recaptcha-response (s r)
  (post-getjson "https://www.google.com/recaptcha/api/siteverify"
                (list 'secret s 'response r)))

;TODO: first, check for the response key, fail if it doesn't exist
;then check for the success key in the response, fail or handle
;error codes if it doesn't exist. 

(def verify-captcha (req)
  (do
    (= resp (recaptcha-response recaptcha-secret* (alref (req 'args) "g-recaptcha-response")))
    (pr resp!success)

    ;how to handle #t and #f when arc doesn't recognize them as types? 
    (if resp!success #t 
      (pr "success")
      (pr "failure"))))
