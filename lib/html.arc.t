(suite html
       (test id-is-a-string
             (assert-same "<div id=\"this-is-an-id\"></div>"
                          (tostring (tag (div id "this-is-an-id")))))

       (suite para
              (test empty-para
                    (assert-same "<p></p>"
                                 (tostring (para))))
              (test para-body
                    (assert-same "<p>other stuff</p>"
                                 (tostring (para "other stuff")))))

       (suite sctag
              (test bare
                    (assert-same "<abcd />"
                                 (tostring (sctag abcd))))
              (test spec-list-just-name
                    (assert-same "<abcd />"
                                 (tostring (sctag (abcd)))))
              (test spec-list-with-contents
                    (assert-same "<abcd efgh=\"ijkl\" mnop=\"qrst\" />"
                                 (tostring (sctag (abcd efgh "ijkl" mnop "qrst"))))))

       (suite start-tag
              (test empty
                    (assert-same "<abcd>"
                                 (tostring (eval (start-tag 'abcd)))))
              (test empty-self-close
                    (assert-same "<abcd />"
                                 (tostring (eval (start-tag 'abcd t)))))
              (test opts-are-strings
                    (assert-same "<abcd class=\"efgh\">"
                                 (tostring (eval (start-tag '(abcd class "efgh"))))))
              (test opts-are-strings-self-close
                    (assert-same "<abcd class=\"efgh\" />"
                                 (tostring (eval (start-tag '(abcd class "efgh") t)))))
              (test opts-not-strings
                    (assert-same "<abc def=\"ghi\">"
                                 (tostring (eval (start-tag '(abc def "ghi"))))))
              (test opts-not-strings-self-close
                    (assert-same "<abc def=\"ghi\" />"
                                 (tostring (eval (start-tag '(abc def "ghi") t)))))))
