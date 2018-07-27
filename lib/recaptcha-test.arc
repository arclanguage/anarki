(require 'srv.arc)
(require 'recaptcha.arc)

; initialize the server and defops. 

(thread (serve 8080))

