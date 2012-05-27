;; -----------------------------57651155441074198547161975
;; Content-Disposition: form-data; name="fnid"
;;
;; 2iJaTziJtr
;; -----------------------------57651155441074198547161975
;; Content-Disposition: form-data; name="someField"
;;
;; 33
;; -----------------------------57651155441074198547161975--
(test-iso "parse-multipart-args works"
  (parse-multipart-args "--abc" "\r\n--abc\r\nContent-Disposition: form-data; name=\"a\"\r\n\r\n34\r\n--abc\r\nContent-Disposition: form-data; name=\"b\"\r\n\r\n209\r\n--abc--\r\n")
  '(("a" "34") ("b" "209")))

