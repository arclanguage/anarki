(def urlencode (str)
  "Return STR, but with weird characters hex-encoded."
  (apply +
         (map [coerce _ 'string]
              (map [ if (alphadig _) _
                        (+ "%" (coerce (coerce _ 'int ) 'string 16))]
                   (coerce str 'cons)))))

(def tinyurl (url)
  "Make URL tiny by sending it to http://tinyurl.com."
  (with (targ  "http://tinyurl.com/"
               prefix "<blockquote><b>"
               )
    (car (tokens
          (apply +
                 (map [cut _ (len prefix)]
                      (keep  [headmatch (+ prefix targ) _]
                             (let (ip op) (client "tinyurl.com" 80)
                                  (w/stdout op
                                    (disp "GET /create.php?url=")
                                    (disp (urlencode url))
                                    (disp " HTTP/1.0\r\n")
                                    (disp "Host: tinyurl.com\r\n")
                                    (disp "\r\n"))
                                  (w/stdin ip (drain (readline)))))))
          #\<))))
