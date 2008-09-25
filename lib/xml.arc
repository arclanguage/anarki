(= x-sep* '(#\newline #\return #\space #\tab))

(def x-skip-sep ()
  "return first character that isn't a separator"
  (let c (readc (stdin))
    (if (mem c x-sep*) (x-skip-sep) c)))

(def x-upto-sep ()
  "skip characters until a non-separator"
  (when (mem (peekc (stdin)) x-sep*)
    (readc (stdin))
    (x-upto-sep)))

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

(def x-name (elem)
  "name of an xml element"
  (car elem))

(def x-terminal (name)
  "is this name a terminator?"
  (is (name 0) #\/))

(def x-terminal-of (name elem)
  "is elem the terminator of name?"
  (is (string #\/ name) (x-name elem)))

(def x-mk-elem (name (o attrs) (o childs))
  (list name attrs childs))

(def x-get-elem ()
  "read an element"
  (let c (x-skip-sep)
    (unless (is c #\<) (err "cannot read element!"))
    (let name (x-get-upto (cons #\> x-sep*))
      (if (x-terminal name)
        (x-mk-elem name)
        (with (attrs (accum acc
                       ((afn ()
                          (x-upto-sep)
                          (unless (is (peekc (stdin)) #\>)
                            (acc (x-get-attr))
                            (self)))))
               body (accum acc
                      ((afn ()
                         (let e (x-get-elem)
                           (unless (x-terminal-of name e)
                             (acc e)
                             (self)))))))
          (x-mk-elem name attrs body))))))
