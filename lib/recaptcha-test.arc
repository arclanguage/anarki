(require 'srv.arc)
(require 'recaptcha.arc)

; initialize the server and defops. 

(thread (srv 8080))

