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
  `(("a" ,(obj "contents" "34")) ("b" ,(obj "contents" "209")))
  (parse-multipart-args "--abc" (instring "\r\n--abc\r\nContent-Disposition: form-data; name=\"a\"\r\n\r\n34\r\n--abc\r\nContent-Disposition: form-data; name=\"b\"\r\n\r\n209\r\n--abc--\r\n")))

; currently fails; how to include binary data in string literals?
(test-iso "parse-multipart-args returns lists of ints for non-ascii data"
  `(("a" ,(obj "contents" "34")) ("b" ,(obj "contents" list.128))) ; \x80 in decimal
  (parse-multipart-args "--abc" (instring "\r\n--abc\r\nContent-Disposition: form-data; name=\"a\"\r\n\r\n34\r\n--abc\r\nContent-Disposition: form-data; name=\"b\"\r\n\r\n\x80\r\n--abc--\r\n")))
