; written by Brian J Rubinton

(suite web
       (suite parse-url
              default-resource (assert-same "http"
                                            ((parse-url "www.google.com") 'resource))
              anchor-is-ignored (assert-same "www.google.com"
                                             ((parse-url "www.google.com#anchor") 'host))
              path-nesting-is-ignored (assert-same "global/asia/china/"
                                                   ((parse-url "www.nytimes.com/global/asia/china/") 'path))
              port-is-ignored (assert-same 8080
                                           ((parse-url "localhost:8080") 'port))
              default-port (assert-same 80
                                        ((parse-url "www.google.com") 'port))
              query-is-retained (assert-same "a=1&b=2&c=3"
                                             ((parse-url "www.google.com/search?a=1&b=2&c=3") 'query)))
       (suite build-request
              querylist-to-string (assert-same "a=1&b=2&c=3"
                                               (to-query-str '(a 1 b 2 c 3)))
              non-urlsafe-inputs (assert-same "a=%20"
                                              (to-query-str '(a " ")))
              arglist-and-argstr-together (assert-same "a=1&b=2&c=3&d=4&e=5&f=6"
                                                       (build-query "a=1&b=2&c=3" '(d 4 e 5 f 6)))
              arglist-only (assert-same "d=4&e=5&f=6"
                                        (build-query "" '(d 4 e 5 f 6)))
              cookie-encoding (assert-same "Cookie: name=value; name2=value2; Expires=Wed, 09 Jun 2021;"
                                           (encode-cookies (list "name" "value"
                                                                 "name2" "value2"
                                                                 "Expires" "Wed, 09 Jun 2021"))))
       (suite send-request
              ping-google (assert-same "HTTP/1.0 200 OK"
                                       (caar (mkreq "www.google.com")))))
