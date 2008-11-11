; arc-read-tests.arc

; Written by Chris Hooper, released into the public domain

; Unit tests for read.arc
;
; To run all the tests:
;
; (register-test full-test-suite*)
; (run-all-tests)

(require "lib/unit-test.arc")

(= integer-tests*
   '(suite "Integer Tests"
     ("One Digit"
      (arc-read "3")
      3)
     ("Multiple digits"
      (arc-read "1234")
      1234)
     ("Trailing decimal point"
      (string (arc-read "1234."))
      "1234")
     ("Plus sign"
      (arc-read "+432")
      432)
     ("Minus sign"
      (arc-read "-432")
      -432)
     ("Sign and trailing decimal"
      (string (arc-read "-432."))
      "-432")
     ("Letter in digits should produce symbol"
      (arc-read "43a2")
      43a2)
     ("Malformed sign should produce symbol"
      (arc-read "++432")
      ++432)))

(= ratio-tests*
   '(suite "Ratio Tests"
     ("Simple ratio"
      (arc-read "3/4")
       3/4)
     ("Long rato"
      (arc-read "123/13")
      123/13)
     ("Ratio with plus sign"
      (arc-read "+3/4")
      3/4)
     ("Ratio with minus sign"
      (arc-read "-3/4")
       -3/4)
     ("Sign in denominator should produce symbol"
      (arc-read "3/+4")
      3/+4)
     ("Malformed numerator should produce symbol"
      (arc-read "1a3/13")
      1a3/13)
     ("Malformed denominator should produce symbol"
      (arc-read "123/1b3")
      123/1b3)
     ("Missing numerator should produce symbol"
      (arc-read "/13")
      /13)
     ("Missing denominator should produce symbol"
      (arc-read "123/")
      |123/|)
     ("Trailing decimal in numerator produces symbol"
      (arc-read "123./13")
      123./13)
     ("Trailing decimal in denominator produces symbol"
      (arc-read "123/13.")
      123/13.)
     ("Non-whole numerator produces symbol"
      (arc-read "12.3/13")
       12.3/13)
     ("Non-whole denominator produces symbol"
      (arc-read "123/1.3")
      123/1.3)
     ("Exponential character in numerator produces symbol"
      (arc-read "12e3/13")
      |12e3/13|)
     ("Exponential character in denominator produces symbol"
      (arc-read "123/1e3")
      |123/1e3|)))

(= float-tests*
   '(suite "Float tests"
     ("Simple float with no exponential"
      (arc-read "123.456")
       123.456)
      ("Integer with exponential"
       (arc-read "3e4")
       30000.0)
      ("Float with exponential"
       (arc-read "1.2e4")
       12000.0)
      ("Positive signed exponential"
       (arc-read "3e+2")
       300.0)
      ("Negative signed exponential"
       (arc-read "3e-2")
       0.03)
      ("Float with plus sign"
       (arc-read "+4.5")
       4.5)
      ("Float with minus sign"
       (arc-read "-4.5")
       -4.5)
      ("Float beginning with decimal point"
       (arc-read ".456")
       0.456)
      ("Missing exponent produces symbol"
       (arc-read "123e")
       123e)
      ("Missing float producs symbol"
       (arc-read "e3")
       e3)
      ("Non-digit in float produces symbol"
       (arc-read "12.a4e4")
       12.a4e4)
      ("Non-digit in exponent produces symbol"
       (arc-read "12.3e4a5")
       12.3e4a5)
      ("Multiple exponent markers produces symbol"
       (arc-read "12e34e5")
       12e34e5)
      ("Trailing decimal point in exponent produces symbol"
       (arc-read "12e34.")
       12e34.)
      ("Float exponent produces symbol"
       (arc-read "12e3.4")
       12e3.4)))

(mac tl (desc test expected)
  `'(,desc 
     (let result (arc-read ,test) (w/outstring s (if (~alist result) (write "Not a list: " s)
                                          (~iso ',expected result) (write "Not isomorphic: " s))
                                      (write result s)
                                      (inside s)))
     ,(w/outstring s (write expected s) (inside s))))

(= list-tests*
   (list 'suite "List tests"
    '("Nil symbol read correctly"
     (arc-read "nil")
     nil)
    '("Empty list correctly converted to nil"
     (arc-read "()")
     nil)
    '("Spaces should be ignored in an empty list"
      (arc-read "( )")
      nil)
    (tl "Singleton list"
        "(a)"
        (a))
    (tl "Long list"
        "(foo \"foo\" 'c 1234)"
        (foo "foo" 'c 1234))
    (tl "Nested list"
        "((((a))))"
        ((((a)))))
    (tl "Multiple nested lists"
        "(a (b c) ((d) e) (f (g)) (h (i (j k) l) m) n)"
        (a (b c) ((d) e) (f (g)) (h (i (j k) l) m) n))
    (tl "Spaces separating parens and items"
        "( a b )"
        (a b))
    (tl "Multiple spaces between items"
        "(a      b        c)"
        (a b c))
    (tl "Dotted pair"
        "(a . b)"
        (a . b))
    (tl "Dotted list"
        "(a b c . d"
        (a b c . d))
    (tl "Symbol containing dot should not be read a dotted list"
        "(a.b)"
        (a.b))))

(= quasiquote-tests*
   (list 'suite "Quasiquote tests"
     (tl "Simple quasiquote"
         "`(a b c)"
         `(a b c))
     (tl "Simple comma"
         ",foo"
         (unquote foo))
     (tl "Quasiquote followed by unquote"
         "`,(+ 1 2)"
         (quasiquote (unquote (+ 1 2))))
     (tl "Quasiquote of list containing unquote"
         "`(a b ,c d)"
         (quasiquote (a b (unquote c) d)))
     (tl "Multiple nested quasiquotes and unquotes"
         "`(a ,b `c ``,@d (e ,,f `(g ,h ,,i)))"
         (quasiquote 
          (a (unquote b) (quasiquote c) (quasiquote (quasiquote (unquote-splicing d)))
             (e (unquote (unquote f)) (quasiquote (g (unquote h) 
                                                     (unquote (unquote i))))))))
     (tl "Simple unquote-splice"
         ",@foo"
         (unquote-splicing foo))))

(mac char-names names
  `'(,@(map [list (string _ " character test")
                  (list 'arc-read (string "#\\" _))
                  (read (string "#\\" _))] names)))

(= named-character-tests* 
   (cons 'suite 
         (cons "Named character tests" 
               (char-names newline space rubout page 
                           tab backspace return linefeed))))

(= string-tests*
   '(suite "String tests"
     ("Empty string"
      (arc-read "\"\"")
      "")
     ("Simple string"
      (arc-read "\"abc\"")
      "abc")
     ("Escape double quotes"
      (arc-read "\"a\\\"b\"")
      "a\"b")
     ("Escape backslash"
      (arc-read "\"a\\\\nb\"")
      "a\\nb")
     ("Single escape character"
      (arc-read "\"\\n\"")
      "\n")
     ("Espace character embedded in normal characters"
      (arc-read "\"abc\\ndef\"")
      "abc\ndef")))

(= bracket-tests*
   (list 'suite "Bracketed function tests"
     (tl "Empty brackets"
         "[]"
         (make-br-fn nil))
     (tl "Empty brackets with spaces"
         "[   ]"
         (make-br-fn nil))
     (tl "Bracketed function call"
         "[foo]"
         (make-br-fn (foo)))
     (tl "Bracketed function call with arguments"
         "[foo 1 2 3]"
         (make-br-fn (foo 1 2 3)))
     (tl "Dotted bracketed function"
         "[1 2 . 3]"
         (make-br-fn (1 2 . 3)))))

(= test-value* 'foo)

(= other-tests* 
   '(suite "Other tests"
     ("Sharpsign-backslash: Characters read correctly"
      (arc-read "#\\x")
      #\x)
     ("Sharpsign-backslash: Number correctly treated as character"
      (arc-read "#\\5")
      #\5)
     ("Sharpsign-backslash: newline read correctly"
      (arc-read "#\\newline")
      #\newline)
     ("Sharpsign-backslash: escape character read correctly"
      (arc-read "#\\\\")
      #\\)
     ("Sharpsign-backslash: macro character read correctly"
      (arc-read "#\\\"")
      #\")
     ("Sharpsign-dot: code correctly evaluated"
      (arc-read "#.test-value*")
      foo)
     ("Quote test"
      (arc-read "'foo")
      'foo)
     ("Comment test"
      (arc-read ";foo")
      nil)
     ("Long comment test"
      (arc-read ";foo (1 2 3) \"hello\" #.foo")
      nil)
     ("Comment after code"
      (let s (instring "(1 2 3) ; (4 5 6)")
        (do (arc-read s) (arc-read s)))
      nil)
     ("Empty multi-escape"
      (arc-read "||")
      ||)
     ("Multi-escape"
      (arc-read "|123|")
      |123|)
     ("Single escape"
      (arc-read "1\\23")
      |123|)))

(= full-test-suite* 
   (list 'suite "All arc-read tests" integer-tests* ratio-tests* float-tests*
         list-tests* quasiquote-tests* string-tests* bracket-tests* other-tests*))
