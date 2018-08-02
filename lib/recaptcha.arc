(require 'lib/env.arc)       ; environment vars
(require 'lib/srv.arc)       ; server
(require 'lib/html.arc)      ; html
(require 'lib/json.arc)     ; json

(=  recaptcha-sitekey* (env-if "RECAPTCHA_SITEKEY" "6LeIxAcTAAAAAJcZVRqyHh71UMIEGNQ_MXjiZKhI")
    recaptcha-secret*  (env-if "RECAPTCHA_SECRET" "6LeIxAcTAAAAAGG-vFI1TnRWxMZNFuojJ4WifJWe"))

; can't add "async defer" to the js
; can't get the div macro to work with data-sitekey (arity mismatch)
; recaptcha (requires https://?)

(def recaptcha-form ()
  (do
    (prn "<script type=\"text/javascript\" src=\"https://www.google.com/recaptcha/api.js\"></script>")
    (prn (string "<div class=\"g-recaptcha\" data-sitekey=\"" recaptcha-sitekey*  "\"></div>"))
))
