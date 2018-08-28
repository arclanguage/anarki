; build-web-help.arc
;
; Running this file executes the unit tests and then generates
; build-gh-pages/site/help/index.html, a simple HTML file full of
; docstrings.


; Running the tests is a good way to load a lot of docstrings at once.
(require 'tests.arc)

(ensure-dir "build-gh-pages/site/help")
(w/outfile out "build-gh-pages/site/help/index.html"
  (w/stdout out
    (doctype "html")
    (tag (html lang "en")
      (head
        (pr "<meta http-equiv=\"Content-Type\" content=\"text/html;charset=UTF-8\" />")
        (title "Anarki Reference Documentation")
        (tag (style type "text/css")
          (pr "
            body {
              margin: 0;
              font-family: sans-serif;
              background: #336699;
            }
            .intro {
              padding: 33px;
              background: white;
            }
            .intro > h1 {
              margin-top: 0;
            }
            .unstable {
              width: 100%;
              border-top: 1px solid black;
              border-bottom: 1px solid black;
              padding: 5px 0;

              text-align: center;
              white-space: nowrap;
              overflow: hidden;

              font-size: 1.4em;
              font-weight: bold;

              background: #993333;
              color: white;
            }
            "
            ; We put this "UNSTABLE" banner in a pseudo-element in the
            ; hope that screen readers will skip it.
            "
            .unstable::after {
              content:
                \""
                  ; ⚠️ U+26A0 WARNING SIGN
                  (string:intersperse " ⚠️ "
                    (n-of 9 "UNSTABLE"))
                "\";
              margin: 0 -50%;
            }
            .help-entry {
              margin: 20px;
              border: 3px solid black;
              padding: 10px;
              background: white;
            }
            .help-entry > .type-and-sig {
              font-size: 1.7em;
            }
            .help-entry > .docstring-and-examples {
              font-size: 1.2em;
            }
            .help-entry .broken-link {
              background: #FFCCCC;
            }
            "))
        (tag body
          (tag (div class "intro")
            (tag h1 (pr:esc-tags "Anarki Reference Documentation"))
            (tag p
              (pr:esc-tags:+
                "All this functionality is subject to change! You're "
                "welcome to edit it with us ")
              (tag (a href "https://github.com/arclanguage/anarki")
                (pr:esc-tags
                  "on GitHub"))
              (pr:esc-tags
                ", or you can open a GitHub issue or an ")
              (tag (a href "http://arclanguage.org/forum")
                (pr:esc-tags
                  "Arc Forum"))
              (pr:esc-tags:+
                " "
                "thread for some help! We count on contributions "
                "like yours to make Anarki better."))
            (tag p
              (pr:esc-tags:+
                "Since others are welcome to do the same thing, "
                "watch out for changes in this space.")))
          (tag:div class "unstable")
          (each name (sort < keys.help*)
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
                          (if (help*:sym possible-link)
                            (tag (a href (+ "#" possible-link))
                              (pr:esc-tags possible-link))
                            (tag (span class "broken-link")
                              (pr:esc-tags possible-link))))))]
                  doc)
                (let (sig docstring) (split-at doc "\n")
                  (tag:a href (+ "#" name) name name)
                  (tag (pre class "type-and-sig")
                    (pr sig))
                  (tag (pre class "docstring-and-examples")
                    (pr:trim docstring)))))))))))
