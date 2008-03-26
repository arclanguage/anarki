; arcformat.arc
; by AmkG
; Arc code formatter

(require "lib/cps_treeparse.arc")

(let (redden purplen constant backslashed commentize
      symbol-format
      lparen rparen d-quote backslash-escaped
      start-block-comment end-block-comment
      charconst digit p-white p-nonwhite
      p-list p-string p-num p-sym block-comment line-comment
      arc-code many-arc-code
      renewline escaped print-out) nil
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
  (let (syntaxes one-symbol enhelp sym-syntax) nil
    (= syntaxes
       (alt #\! #\. #\~ #\: #\' #\` (seq #\, (maybe #\@))))
    (= one-symbol
       (filt [list (list (sym:string _) _)]
         (alt (many1 (anything-but syntaxes))
              syntaxes)))
    (= sym-syntax
       (many one-symbol))
    (= enhelp
       (fn ((symbol orig))
         (iflet (typ str) (help* symbol)
              (list "<span style='text-decoration: none; color: "
                    (case typ
                      fn "#007f7f"
                      mac 'brown
                          'blue)
                    "' title=\""
                    (tostring:pr-escaped str)
                    "\">" orig "</span>")
              orig)))
    (= symbol-format
      [map enhelp (car:parse sym-syntax _)]))
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
  (= digit
     (pred [<= #\0 (car _) #\9] anything))
  (= p-white
     (pred whitec:car anything))
  (= p-nonwhite
     (pred nonwhite:car anything))
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
  ;number parsing
  (let (exactness sign radix10 prefix10
        sign exponent suffix uinteger10 decimal10
        ureal10 real10 imag10
        complex10 num10) nil
    ; local parsers
    (= exactness
       (alt (seq #\# #\i) (seq #\# #\e) nothing))
    (= radix10
       (alt (seq #\# #\d) nothing))
    (= prefix10
       (alt (seq exactness radix10)
            (seq radix10 exactness)))
    (= sign
       (alt #\+ #\- nothing))
    (= exponent
       (seq
         (alt #\e #\E #\s #\S #\f #\F #\d #\D #\l #\L)
         sign
         (many1 digit)))
    (= suffix
       (alt exponent nothing))
    (= uinteger10
       (many1 digit))
    (= decimal10
       (alt
          (seq #\. uinteger10 (maybe suffix))
          suffix))
    (= ureal10
       (alt (seq uinteger10
                 (alt
                   (seq  #\/ uinteger10)
                   (maybe (alt decimal10 #\.))))
            (seq #\. uinteger10 (maybe suffix))))
    (= real10
       (seq sign ureal10))
    (= imag10
       (seq (maybe ureal10) #\i))
    (= complex10
       (alt (seq (alt #\+ #\-) imag10)
            (seq real10
                 (alt (seq #\@ real10)
                      (seq (alt #\+ #\-) imag10)
                      nothing))))
    (= num10
       (seq prefix10 complex10))
    ;exported parser
    (= p-num
       (filt constant
          (seq (alt num10)
               (cant-see (pred nonwhite:car anything))))))
  (= p-sym
     (filt symbol-format
       (alt (seq #\| (many (anything-but #\|)) #\|)
            (many1 (seq (cant-see:alt lparen rparen #\|) p-nonwhite)))))
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
       p-num
       charconst
       block-comment
       line-comment
       ; errors - unpaired/unterminated stuff
       (filt redden lparen)
       (filt redden rparen)
       (filt redden d-quote)
       (filt redden start-block-comment)
       (filt redden end-block-comment)
       ; anything else has got to be a symbol
       p-sym
       anything))
  (= many-arc-code
     (many arc-code))
  ; output
  (= renewline
     (fn (l)
       (car:parse
         (many
           (alt (filt [list #\newline]
                  (alt (nil-seq #\newline (maybe #\return))
                       (nil-seq #\return (maybe #\newline))))
                anything))
         l)))
  (= escaped
     (fn (c)
       (case c
         #\< "&lt;"
         #\> "&gt;"
         #\& "&amp;"
         #\" "&quot;"
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
    (print-out:car:parse many-arc-code (renewline l))
    (prn "</code>")
    nil)
  (def arc-format (s)
    (arc-format-l
      (coerce s 'cons))))

