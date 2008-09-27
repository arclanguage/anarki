; Simple XML parser
; all functions read from standard input
; ignores DTD, processing instructions, prologs
; namespaces and entities are left untouched

; Example:
; - Reading the first XML element from a string:
;   (w/stdin (instring str)
;     (x-get-elem))
;   if str is <a> <b> </b> <c foo="bar"> </c> </a> it will return
;   ("a" nil (("b" nil nil) ("c" (("foo" . "bar")) nil)))

(= x-sep* '(#\newline #\return #\space #\tab))

(def x-skip-sep ()
  "return first character that isn't a separator"
  (let c (readc (stdin))
    (if (mem c x-sep*) (x-skip-sep) c)))

(def x-upto-sep ()
  "throw away separators"
  (let c (peekc (stdin))
    (when (mem c x-sep*)
      (readc (stdin))
      (x-upto-sep))))

(def x-upto (stop-list)
  "read until a character in stop-list is found, character in stop-list
   is left in the stream"
  (tostring
    (let c nil 
      (while (~mem (= c (peekc (stdin))) stop-list) 
        (unless c (err "input finished during parsing!"))
        (writec (readc (stdin)))))))

(def x-get-upto (stop-list)
  "read until a character in stop-list is found"
  (tostring
    (let c nil
      (while (~mem (= c (readc (stdin))) stop-list)
        (unless c (err "input finished during parsing!"))
        (writec c)))))

(def x-skip-special-tag ((o level 1))
  "skip a special tag until the ending #\\>. A special tag may contain 
   paired #\\< #\\>"
  (unless (is level 0)
    (let c (readc (stdin))
      (if (is c #\<) (x-skip-special-tag (+ level 1))
          (is c #\>) (x-skip-special-tag (- level 1))
          (x-skip-special-tag level)))))

(def x-get-string ()
  "read a string"
  (let c (x-skip-sep)
    (unless (is c #\") (err "not a string!"))
    (x-get-upto '(#\"))))

(def x-get-attr ()
  "read an attribute"
  (with (name (string (x-skip-sep) (x-get-upto '(#\=)))
         attr (x-get-string))
    (cons name attr)))

(def x-get-name ()
  "read the name of an element"
  (tostring 
    (with (c nil first t) 
      (while (and (~mem (= c (peekc (stdin))) (cons #\> x-sep*))
                  (or first (~is c #\/)))
        (= first nil)
        (writec (readc (stdin)))))))

(def x-get-cdata ()
  "read a cdata section"
  (let in (stdin)
    (tostring
      ((afn ()
         (let s (x-upto '(#\]))
           (pr s)
           (with (c0 (readc in)
                  c1 (readc in)
                  c2 (readc in))
             (if (no (and c0 c1 c2)) (err "CDATA section not closed!")
                 (no (is (string c0 c1 c2) "]]>"))
                   (do
                     (pr c0 c1 c2)
                     (self))))))))))

; element accessor functions
(def x-name (elem)
  "name of an xml element"
  (car elem))

(def x-attr (elem)
  "list of attributes of an element"
  (cadr elem))

(def x-childs (elem)
  "child elements"
  (elem 2))

(def x-terminal (name)
  "is this name a terminator?"
  (is (name 0) #\/))

(def x-prolog (name)
  "is this a prolog?"
  (is (name 0) #\?))

(def x-processing (name)
  "is this the name of a processing instruction?"
  (is (name 0) #\!))

(def x-cdata (name)
  "CDATA section?
   must be checked before x-processing"
  (is name "![CDATA["))

(def x-terminal-of (name elem)
  "is elem the terminator of name?"
  (is (string #\/ name) (x-name elem)))

(def x-mk-elem (name (o attrs) (o childs))
  (list name attrs childs))

(def x-get-text ()
  "read text as a child element" 
  (x-mk-elem "text" nil (x-upto '(#\<))))

(def x-empty-text (txt)
  (blank (x-childs txt)))

(def x-get-elem ()
  "read an element"
  (let c (x-skip-sep)
    (unless (is c #\<) (prn c) (err "cannot read element!"))
    (let name (x-get-name)
      (if 
        (x-cdata name) (x-mk-elem "cdata" nil (x-get-cdata))
        (or (x-processing name) (x-prolog name))
          ; skip special tag and try to read another element
          (do (x-skip-special-tag) (x-get-elem)) 
        (x-terminal name)
          ; throw away any contents and build the closing element
          (do (x-get-upto '(#\>)) (x-mk-elem name))
        (withs (stop nil
                attrs (accum acc
                        ((afn ()
                           (x-upto-sep) ; throw away separators
                           (let c (peekc (stdin))
                             (if (is c #\>) nil ; stop
                                 (is c #\/) 
                                   (do
                                     (x-get-upto '(#\>)) ; finish element
                                     (= stop t))
                                 (do
                                   (acc (x-get-attr))
                                   (self))))))))
          (if stop
             (x-mk-elem name (rev attrs) nil)
             (let body (accum acc
                         (readc (stdin)) ; throw away #\>
                         ((afn ()
                           (let txt (x-get-text)
                             (unless (x-empty-text txt)
                               (acc txt)))
                           (let e (x-get-elem) 
                             (unless (x-terminal-of name e)
                               (acc e)
                               (self))))))
               (x-mk-elem name (rev attrs) (rev body)))))))))
