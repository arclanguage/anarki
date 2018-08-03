(require 'lib/env.arc)       ; environment vars

(=  recaptcha-sitekey* (env-if "RECAPTCHA_SITEKEY" "6LeIxAcTAAAAAJcZVRqyHh71UMIEGNQ_MXjiZKhI")
    recaptcha-secret*  (env-if "RECAPTCHA_SECRET"  "6LeIxAcTAAAAAGG-vFI1TnRWxMZNFuojJ4WifJWe"))
