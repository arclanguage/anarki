(suite web
       (suite parse-url
              (test default-resource
                    (assert-same "http"
                                 ((parse-url "www.google.com") 'resource)))
              (test anchor-is-ignored
                    (assert-same "www.google.com"
                                 ((parse-url "www.google.com#anchor") 'host)))
              (test path-nesting-is-ignored
                    (assert-same "global/asia/china/"
                                 ((parse-url "www.nytimes.com/global/asia/china/")
                                  'path)))
              (test port-is-ignored
                    (assert-same 8080
                                 ((parse-url "localhost:8080") 'port)))
              (test default-port
                    (assert-same 80
                                 ((parse-url "www.google.com") 'port)))
              (test query-is-retained
                    (assert-same "a=1&b=2&c=3"
                                 ((parse-url "www.google.com/search?a=1&b=2&c=3")
                                  'query))))
       (suite build-request
              (test querylist-to-string
                    (assert-same "a=1&b=2&c=3"
                                 (to-query-str '(a 1 b 2 c 3))))
              (test non-urlsafe-inputs
                    (assert-same "a=%20"
                                 (to-query-str '(a " "))))
              (test arglist-and-argstr-together
                    (assert-same "a=1&b=2&c=3&d=4&e=5&f=6"
                                 (build-query "a=1&b=2&c=3" '(d 4 e 5 f 6))))
              (test arglist-only
                    (assert-same "d=4&e=5&f=6"
                                 (build-query "" '(d 4 e 5 f 6))))
              (test cookie-encoding
                    (assert-same "Cookie: name=value; name2=value2; Expires=Wed, 09 Jun 2021;"
                                 (encode-cookies (list "name" "value" "name2" "value2" "Expires" "Wed, 09 Jun 2021")))))
       (suite send-request
              (test ping-google
                    (assert-same "HTTP/1.0 200 OK"
                                 (caar (mkreq "www.google.com"))))))

