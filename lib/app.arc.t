(suite markdown
       (test strings
             (assert-same "abc def" (markdown "abc def")))
       (test urls
             (assert-same "<a href=\"http://foo.com\" rel=\"nofollow\">http://foo.com</a>"
                          (markdown "http://foo.com")))
       (test ssl-urls
             (assert-same "<a href=\"https://foo.com\" rel=\"nofollow\">https://foo.com</a>"
                          (markdown "https://foo.com")))
       (test links-suppressed
             (assert-same "http://foo.com"
                          (markdown "http://foo.com" nil t)))
       (test long-urls-trimmed
             (assert-same "<a href=\"http://foo.com\" rel=\"nofollow\">http...</a>"
                          (markdown "http://foo.com" 4)))
       (test urls-with-punctuation
             (assert-t (posmatch "https://en.wikipedia.org/wiki/Sherlock_Holmes_(1984_TV_series)"
                                 (markdown "https://en.wikipedia.org/wiki/Sherlock_Holmes_(1984_TV_series)"))))
       (test paragraphs-are-segmented
             (assert-same "abc<p>def"
                          (markdown "abc\n\ndef")))
       (test indented-regions-are-code
             (assert-t (posmatch "<pre><code>  abc</code></pre>"
                                 (markdown "  abc"))))
       (test asterisks-are-italics
             (assert-same "<i>abc</i>"
                          (markdown "*abc*")))
       (test asterisks-across-multiple-words
             (assert-same "<i>abc def</i>"
                          (markdown "*abc def*")))
       (test html-entities-are-left-alone
             (assert-same "&#382;abc&#285;"
                          (markdown "&#382;abc&#285;"))))

