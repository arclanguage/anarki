; build-web-help.arc
;
; Running this file executes the unit tests and then generates
; help.html, a simple HTML file full of docstrings.


; Running the tests is a fine way to load a lot of docstrings.
(require 'tests.arc)

(w/outfile out "help.html"
  (w/stdout out
    (doctype "html")
    (html
      (head
        (tag (style type "text/css")
          (pr "
            body {
              background: #336699;
            }
            .help-entry {
              margin: 20px;
              border: 3px solid black;
              padding: 10px;
              background: white;
            }
            .type-and-sig {
              font-size: 1.7em;
            }
            .docstring-and-examples {
              font-size: 1.2em;
            }
            .broken-link {
              background: #FFCCCC;
              color: black;
            }
            "))
        (tag body
          (each (name doc) help*
            (tag (div class "help-entry")
              (let doc helpstr.name
                (zap string name)
                (zap
                  [$.regexp-replace*
                    '#px"(.*?)(?:\\[\\[([^\\]\\s]*)\\]\\]|$)"
                    _
                    (fn (entire-match normal-text possible-link)
                      (tostring
                        (pr:esc-tags normal-text)
                        (when possible-link
                          (tag (a
                                 href (+ "#" possible-link)
                                 class (if (help*:sym possible-link)
                                         "good-link"
                                         "broken-link"))
                            (pr:esc-tags possible-link)))))]
                  doc)
                (let (sig docstring) (split-at doc "\n")
                  (tag:a href (+ "#" name) name name)
                  (tag (pre class "type-and-sig")
                    (pr sig))
                  (tag (pre class "docstring-and-examples")
                    (pr:trim docstring)))))))))))
