(test-iso "markdown passes text through"
  "abc def"
  (markdown "abc def"))

(test-contains "markdown linkifies urls"
  "<a href=\"http://foo.com\" rel=\"nofollow\">http://foo.com</a>"
  (markdown "http://foo.com"))

(test-contains "markdown linkifies SSL urls"
  "<a href=\"https://foo.com\" rel=\"nofollow\">https://foo.com</a>"
  (markdown "https://foo.com"))

(test-contains "markdown can suppress linkification"
  "http://foo.com"
  (markdown "http://foo.com" nil t))

(test-contains "markdown can trim long urls for display"
  "<a href=\"http://foo.com\" rel=\"nofollow\">http...</a>"
  (markdown "http://foo.com" 4))

(test-contains "markdown handles urls with punctuation"
  "https://en.wikipedia.org/wiki/Sherlock_Holmes_(1984_TV_series)"
  (markdown "https://en.wikipedia.org/wiki/Sherlock_Holmes_(1984_TV_series)"))

(test-contains "markdown segments paragraphs"
  "abc<p>def"
  (markdown "abc\n\ndef"))

(test-contains "markdown formats indented regions as code"
  "<pre><code>  abc</code></pre>"
  (markdown "  abc"))

(test-contains "markdown formats asterisks in italics"
  "<i>abc</i>"
  (markdown "*abc*"))

(test-contains "markdown formats asterisks across words"
  "<i>abc def</i>"
  (markdown "*abc def*"))

(test-contains "markdown leaves html entities alone"
  "&#382;abc&#285;"
  (markdown "&#382;abc&#285;"))
