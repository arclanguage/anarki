
(require "lib/defpat.arc")

(let (helper tagp classesp) nil

  (= tagp
    (fn (s)
      (breakable:withs
             (st (string s)
              rv nil
              tlrv nil
              addc
              (fn (c)
                (if rv (do (= (cdr tlrv) (cons c nil)) (= tlrv (cdr tlrv)))
                       (do (= rv (cons c nil)) (= tlrv rv))))
              return
              (fn ()
                (if rv (break (sym (string rv)))
                       (break 'div))))
        (each c st
          (if (in c #\# #\.)
            (return)
            (addc c)))
        (return))))

  (= classesp
    (fn (s)
      (let (class id)
           (withs
                  (st (string s)
                   cl nil
                   tlcl nil
                   id nil
                   tlid nil
                   addcl
                   (fn (c)
                     (if cl (do (= (cdr tlcl) (cons c nil)) (= tlcl (cdr tlcl)))
                            (do (= cl (cons c nil)) (= tlcl cl))))
                   addid
                   (fn (c)
                     (if id (do (= (cdr tlid) (cons c nil)) (= tlid (cdr tlid)))
                            (do (= id (cons c nil)) (= tlid id)))))
             ; state machine
             (let (tag class id state) nil
               (= tag
                  (fn (c)
                    (if (is c #\.) class
                        (is c #\#) id
                                   tag)))
               (= class
                  (fn (c)
                    (if (is c #\.) (do (addcl #\space) class)
                        (is c #\#) id
                                   (do (addcl c) class))))
               (= id
                  (fn (c)
                    (if (is c #\.) class
                        (is c #\#) (do (addid #\space) id)
                                   (do (addid c) id))))
               (= state tag)
               (each c st
                 (= state (state c))))
             `(,(if cl (string cl))
               ,(if id (string id))))
        `(,@(if class (list 'class class))
          ,@(if id    (list 'id    id))))))

  (= helper
    (p-m:fn
      " Converts ('htm ...) and ('(htm attrib val ...) ...) to
	(tag (htm attrib val ...) ...)
	Used as helper within [[w/html]] "
      ;Don't enter quoted forms
      ( ('quote _) )
	`(quote ,_)
      (
	( ('quote ,(htm (isa htm 'sym)) )
	  . body))
	`(tag (,(tagp htm) ,@(classesp htm)) ,@(map helper body))
      (
	( ('quote (,(htm (isa htm 'sym)) . attribs))
	  . body))
	`(tag (,(tagp htm) ,@(classesp htm) ,@attribs)
	   ,@(map helper body))
      ( ,(l (isa l 'cons)))
	(map helper l)
      ( _ )
	_))

  (mac w/html body
    " Encloses the body in <html> tags; forms starting in
      ('htmltag ...) are converted to tags:
	(w/html
	  ('head ('title (prn \"Page\")))
	  ('(body style \"font-size: 200%\")
	    (prn \"HELLO!\")))
      See also [[tag]] [[w/html-tags]] "
    `(tag html
       ,@(map helper body)))

  (mac w/html-tags body
    " Performs the body; forms starting in ('htmltag ...)
      are converted to tags.
      Similar to w/html except the body is not enclosed in
      <html> tags.
      See also [[w/html]] "
    `(do ,@(map helper body))))


(def *w/html-test ()
  (w/html
    ('head ('title (pr "Page")))
    ('body
      ('(div style "font-size: 120%")
        (pr "Hello world!"))
      ('b
        (pr "bold")) ('br)
      (pr "hmm"))))

(def *w/html-test2 ()
  (w/html
    ('head ('title (pr "Page 2")))
    ('body
      ('.content
        (pr "Content!"))
      ('.left.column
        (pr "Left Column")
        ('span#id
          (pr "In a #id span")))
      ('.foo#id
        (pr "foo class id id")))))

