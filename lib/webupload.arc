;
; Example of uploading a file from a web browser with
; the Anarki version of Arc.
;
; This example just echos back what the browser sends
; so you can see how to parse it.  Parsing may depend
; on the format of the data that is uploaded.
; You'll probably need to parse the boundary out of
; the Content-Type and then look for it.  Here's some
; sample output:
;
; ... headers ... 
; Content-Type: multipart/form-data; boundary=----WebKitFormBoundarysVQ7sAKHmOlv
; Content-Length: 200
; ... more headers ...
;
; ------WebKitFormBoundarysVQ7sAKHmOlv 
; Content-Disposition: form-data; name="name"; filename="test.txt" 
; Content-Type: text/plain 
; 
; A small
; 3 line
; test file.
; 
; ------WebKitFormBoundarysVQ7sAKHmOlv-- 
;
; Note that the final boundary an extra "--" at the beginning and
; at the end.
(def upload-action (req)
  (prn " ... headers ...")
  (prn "<br>Content-Type: " req!ctype)
  (prn "<br>Content-Length: " req!clen)
  (prn "<br> ... more headers ...")
  (prn "<br><br>")
  (let n req!clen
    (whilet c (and (> n 0) (readc req!in))
      (-- n)
      (if (is c #\newline) (pr "<br>") (pr c))))
)

(defop upload req
  (aform-multi upload-action ; must use aform-multi to get the multipart encoding
    (gentag input type 'file name 'name)
    (submit "upload")))
