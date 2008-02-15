
(require "lib/defpat.arc")

(mac w/html body
  " Encloses the body in <html> tags; forms starting in
    ('htmltag ...) are converted to tags:
      (w/html
        ('head ('title (prn \"Page\")))
        ('(body style \"font-size: 200%\")
          (prn \"HELLO!\")))
    See also [[tag]] "
  `(tag html
     ,@(map *w/html-helper body)))

(defpat *w/html-helper
  " Converts ('htm ...) and ('(htm attrib val ...) ...) to
    (tag (htm attrib val ...) ...) "
  ;Don't enter quoted forms
  ( ('quote _) )
    `(quote ,_)
  (
    ( ('quote ,(htm (isa htm 'sym)) )
      . body))
    `(tag ,htm ,@(map *w/html-helper body))
  (
    ( ('quote (,(htm (isa htm 'sym)) . attribs))
      . body))
    `(tag (,htm ,@attribs)
       ,@(map *w/html-helper body))
  ( ,(l (isa l 'cons)))
    (map *w/html-helper l)
  ( _ )
    _)

(def *w/html-test ()
  (w/html
    ('head ('title (pr "Page")))
    ('body
      ('(div style "font-size: 120%")
        (pr "Hello world!"))
      ('b
        (pr "bold")) ('br)
      (pr "hmm"))))

