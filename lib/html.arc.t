(suite html
       (test id-is-a-string
             (assert-same "<div id=\"this-is-an-id\"></div>"
                          (tostring (tag (div id "this-is-an-id"))))))

