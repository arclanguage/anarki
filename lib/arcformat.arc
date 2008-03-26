; arcformat.arc
; by AmkG
; Arc code formatter

(require "lib/cps_treeparse.arc")

(let (redden purplen constant backslashed commentize
      lparen rparen d-quote backslash-escaped
      start-block-comment end-block-comment
      charconst
      p-list p-string block-comment line-comment
      arc-code many-arc-code
      escaped print-out) nil
  ; filters
  ; (change this to use css classes if necessary)
  (= redden
     [list "<b style='background-color: red; color: white'>" _ "</b>"])
  (= purplen
     [list "<span style='color: #c000ff'>" _ "</span>"])
  (= constant
     [list "<span style='color: #c00000'>" _ "</span>"])
  (= backslashed
     [list "<b>" _ "</b>"])
  (= commentize
     [list "<span style='color: #00c000'>" _ "</span>"])
  ; parsers
  (= lparen (lit #\( ))
  (= rparen (lit #\) ))
  (= d-quote (lit #\"))
  (= backslash-escaped
     (seq #\\ anything))
  (= start-block-comment
     (seq #\# #\|))
  (= end-block-comment
     (seq #\| #\#))
  (= charconst
     (filt constant
       (seq #\# #\\ (many (pred nonwhite:car anything)))))
  (= p-list
     ; can be cut
     (seq (filt purplen lparen)
          (many (seq (cant-see rparen) (delay-parser arc-code)))
          (filt purplen rparen)))
  (= p-string
     (filt constant
       ; can be cut
       (seq d-quote
            (many (seq (cant-see d-quote)
                       (alt (filt backslashed backslash-escaped) anything)))
            d-quote)))
  (= block-comment
     (filt commentize
       ; can be cut
       (seq start-block-comment
            (many (anything-but end-block-comment))
            end-block-comment)))
  (= line-comment
     (filt commentize
       (seq #\;
            (many (anything-but #\newline))
            #\newline)))
  (= arc-code
     (alt
       p-list
       p-string
       block-comment
       charconst
       ; errors - unpaired/unterminated stuff
       (filt redden lparen)
       (filt redden rparen)
       (filt redden d-quote)
       (filt redden start-block-comment)
       (filt redden end-block-comment)
       anything))
  (= many-arc-code
     (many arc-code))
  ; output
  (= escaped
     (fn (c)
       (case c
         #\< "&lt;"
         #\> "&gt;"
         #\& "&amp;"
         #\" "&quot;"
         #\return ""
         #\newline "<br>"
         #\space "&nbsp;"
             c)))
  (= print-out
     (fn (p)
       (if (alist p)
           (each c p (print-out c))
           (pr:escaped p))))
  (def arc-format-l (l)
    (pr "<code>")
    (print-out:car:parse many-arc-code l)
    (prn "</code>")
    nil)
  (def arc-format (s)
    (arc-format-l
      (coerce s 'cons))))

