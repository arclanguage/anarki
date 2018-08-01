(require '(
 lib/env.arc       ; environment vars
 lib/srv.arc       ; server
 lib/json.arc      ; json
 lib/html.arc      ; html
))

(=  recaptcha-sitekey (env-if RECAPTCHA-SITEKEY "6LeIxAcTAAAAAJcZVRqyHh71UMIEGNQ_MXjiZKhI")
    recaptcha-secret  (env-if RECAPTCHA-SECRET  "6LeIxAcTAAAAAGG-vFI1TnRWxMZNFuojJ4WifJWe")
    recaptcha-uri-endpoint "https://www.google.com/recaptcha/api/siteverify")

