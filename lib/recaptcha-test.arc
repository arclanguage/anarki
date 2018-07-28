(require 'srv.arc)
(require 'html.arc)
(require 'recaptcha.arc)

; initialize the server and defops. 

(def init() (
	(thread (serve 8080))
	(defop || (recaptcha_form))
))
