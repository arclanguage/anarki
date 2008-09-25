; Simple XML parser
; all functions read from standard input

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

(def x-processing (name)
  "is this the name of a processing instruction?"
  (is (name 0) #\?))

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
      (if (or (x-processing name) (x-terminal name))
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
             (x-mk-elem name attrs nil)
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
