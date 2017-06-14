(suite srv
       (test read-header-prrn
             (assert-same '("a: b" "c: d")
                          (pipe-to (read-header)
                            (prrn "a: b")
                            (prrn "c: d")
                            (prrn)
                            (prrn "body")
                            (prrn "body2"))))
       (test read-header-prn
             (assert-same '("a: b" "c: d")
                          (pipe-to (read-header)
                            (prn "a: b")
                            (prn "c: d")
                            (prn)
                            (prn "body")
                            (prn "body2"))))
       (test split-header
             (assert-same '("content-type" "multipart/form-data; boundary=aaaabbbb")
                          (split-header "Content-Type: multipart/form-data; boundary=aaaabbbb")))
       (test parse-header
             (assert-same (obj 
                            op
                            empty-sym*
                            type
                            'get
                            "host"
                            "localhost:8080"
                            "user-agent"
                            "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:28.0) Gecko/20100101 Firefox/28.0"
                            "accept"
                            "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
                            "accept-language"
                            "en-US,en;q=0.5"
                            "accept-encoding"
                            "gzip, deflate"
                            "dnt"
                            "1"
                            "cookie"
                            "user=Qr9lN3Vn"
                            "connection"
                            "keep-alive"
                            "cache-control"
                            "max-age=0")
                          (pipe-to (parse-header)
                            (prrn "GET / HTTP/1.1")
                            (prrn "Host: localhost:8080")
                            (prrn "User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:28.0) Gecko/20100101 Firefox/28.0")
                            (prrn "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
                            (prrn "Accept-Language: en-US,en;q=0.5")
                            (prrn "Accept-Encoding: gzip, deflate")
                            (prrn "DNT: 1")
                            (prrn "Cookie: user=Qr9lN3Vn")
                            (prrn "Connection: keep-alive")
                            (prrn "Cache-Control: max-age=0"))))
       (test read-header
             (assert-same (list "GET / HTTP/1.1" "Host: localhost:8080" "User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:28.0) Gecko/20100101 Firefox/28.0" "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8" "Accept-Language: en-US,en;q=0.5" "Accept-Encoding: gzip, deflate" "DNT: 1" "Cookie: user=Qr9lN3Vn" "Connection: keep-alive" "Cache-Control: max-age=0")
                          (pipe-to (read-header)
                            (prrn "GET / HTTP/1.1")
                            (prrn "Host: localhost:8080")
                            (prrn "User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:28.0) Gecko/20100101 Firefox/28.0")
                            (prrn "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
                            (prrn "Accept-Language: en-US,en;q=0.5")
                            (prrn "Accept-Encoding: gzip, deflate")
                            (prrn "DNT: 1")
                            (prrn "Cookie: user=Qr9lN3Vn")
                            (prrn "Connection: keep-alive")
                            (prrn "Cache-Control: max-age=0"))))
       (test parse-cmd
             (assert-same '((type get)
                            (op p1)
                            (args (("foo" "bar") ("ug" ""))))
                          (parse-cmd "GET /p1?foo=bar&ug")))
       (test parse-multipart-args
             (assert-same `(("a" ,(obj "contents" "34"))
                            ("b" ,(obj "contents" "209")))
                          (parse-multipart-args "--abc"
                                                (instring "\r\n--abc\r\nContent-Disposition: form-data; name=\"a\"\r\n\r\n34\r\n--abc\r\nContent-Disposition: form-data; name=\"b\"\r\n\r\n209\r\n--abc--\r\n"))))
       (suite run-request
              (test simple-request
                    (do (wipe ranked-stories*)
                        (ensure-dir logdir*)
                        (fromstring "GET / HTTP/1.1\n\n"
                          (handle-request-thread (stdin)
                                                 (stdout)
                                                 "no ip"))))
              (test empty-request
                    (do (wipe ranked-stories*)
                        (ensure-dir logdir*)
                        (fromstring "\n\n"
                          (handle-request-thread (stdin)
                                                 (stdout)
                                                 "no ip"))))))

