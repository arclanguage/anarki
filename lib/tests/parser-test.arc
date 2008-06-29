(register-test '(suite "parser tests"
  ("parse a single symbol"              (parse "foo")                   foo                 )
  ("parse a single symbol: is sym"      (type:parse "foo")              sym                 )
  ("parse a single string"              (parse "\"foo\"")               "foo"               )
  ("parse a single string: is string"   (type:parse "\"foo\"")          string              )
  ("parse a quoted symbol"              (parse "'foo")                  'foo                )
  ("parse an empty list"                (parse "()")                    ()                  )
  ("parse an empty list: is sym"        (type:parse "()")               sym                 )
  ("parse a character"                  (parse "#\\a")                  #\a                 )
  ("parse a char by number"             (parse "#\\103")                #\C                 )
  ("parse a newline character"          (parse "#\\
")                                                                      #\newline           )
  ("parse a character: is char"         (type:parse "#\\a")             char                )
  ("parse a number"                     (parse "99/101")                99/101              )
  ("parse a number: is num"             (type:parse "99/101")           num                 )
  ("parse numbers in a list"            (parse "(12 34.56 -17 3/4)")    (12 34.56 -17 3/4)  )
  ("parse a list of characters"         
    (eval (parse "(coerce '(#\\( #\\a #\\b #\\space #\\c #\\  #\\d #\\)) 'string)"))
    "(ab c d)")
  ("raise error for unrecognised chars"
    (on-err (fn (ex) (details ex)) (fn () (parse "#\\spade")))
    "unknown char: #\\spade")
  ("parse a string containing spaces"
    (parse "\"foo bar\"")               
    "foo bar")
  ("completely ignore comments"
    (parse "(foo bar) ; the foo bar\n; more commentary")               
    (foo bar))
  ("parse quote non-atom"
    (parse "'(foo bar)")               
    '(foo bar))
  ("parse whitespace before closing paren"
    (parse "(foo bar )")               
    (foo bar))
  ("parse a nasty string containing parens and escapes"
    (parse "(parse \"\\\"foo bar\\\"\")")
    (parse "\"foo bar\""))
  ("parse bracket syntax for functions"
    (apply (eval (parse "[* _ _]")) '(27))
    729)
  ("parse a complex expression"    
    (parse "(foo bar '(toto) `(do ,blah ,@blahs \"astring\") titi)")
    (foo bar '(toto) `(do ,blah ,@blahs "astring") titi))))

(register-test '(suite "source code indexer test"
  ("index a proper expression"
    (index-source "\n(def foo (bar)\n  (toto 'a \"blah\"))")
    ((left-paren    1 35 )
     ("def"         2  5 )
     ("foo"         6  9 )
     (left-paren   10 15 )
     ("bar"        11 14 )
     (right-paren  10 15 )
     (left-paren   18 34 )
     ("toto"       19 23 )
     (quote        24 25 )
     ("a"          25 26 )
     ("\"blah\""   27 33 )
     (right-paren  18 34 )
     (right-paren   1 35 )))
       
  ("index an unbalanced left-paren"
    (index-source "(def foo (bar) (toto 'a \"blah\")") 
    ((unmatched-left-paren     0  1 )
     ("def"                    1  4 )
     ("foo"                    5  8 )
     (left-paren               9 14 )
     ("bar"                   10 13 )
     (right-paren              9 14 )
     (left-paren              15 31 )
     ("toto"                  16 20 )
     (quote                   21 22 )
     ("a"                     22 23 )
     ("\"blah\""              24 30 )
     (right-paren             15 31 )))
     
  ("index an unbalanced left-bracket"
    (index-source "(map [disp _ val)") 
    ((left-paren               0 17 )
     ("map"                    1  4 )
     (unmatched-left-bracket   5  6 )
     ("disp"                   6 10 )
     ("_"                     11 12 )
     ("val"                   13 16 )
     (right-paren              0 17 )))
     
  ("index an unbalanced right-paren"
    (index-source "(prn (foo (x) y))) (prn 'yo)") 
    ((left-paren               0 17 )
     ("prn"                    1  4 )
     (left-paren               5 16 )
     ("foo"                    6  9 )
     (left-paren              10 13 )
     ("x"                     11 12 )
     (right-paren             10 13 )
     ("y"                     14 15 )
     (right-paren              5 16 )
     (right-paren              0 17 )
     (unmatched-right-paren   17 18 )
     (left-paren              19 28 )
     ("prn"                   20 23 )
     (quote                   24 25 )
     ("yo"                    25 27 )
     (right-paren             19 28 )))))
