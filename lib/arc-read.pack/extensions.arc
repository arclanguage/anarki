; extensions.arc

; Written by Chris Hooper, released into the public domain

; unprocessed strings
(def unprocessed-string-reader (port char)
  (if (isnt (peekc port) #\") (accum-token char port nil)
      (do 
       (readc port)
       (string:rev:accum add 
          (whiler c (readc port) #\" 
                  (if (and (is #\\ c) (is #\" (peekc port))) (add:readc port)
                      (add c)))))))

(def unprocessed-strings ()
  (= (readtable* #\@) 'non-term-macro)
  (= (reader-macros* #\@) unprocessed-string-reader))

; an example of flexibility: swapping the meaning of commas and dots
(def swap-commas-and-dots ()
  (= (readtable* #\.) 'term-macro)
  (= (readtable* #\,) 'decimal-point)
  (= (reader-macros* #\.) comma-reader)
  (wipe (reader-macros* #\,)))

; heredocs (allows documents to be embedded in Arc code)
(def heredoc-reader (port unused)
  (let term (+ (readline port) "\n")
    (apply + (rev:accum add 
               (whiler l (+ (readline port) "\n") term (add l))))))

(def heredocs ()
  (= (sharpsign-dispatch* #\<) heredoc-reader))
