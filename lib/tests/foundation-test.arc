
(register-test '(suite "Foundation Tests"

  (suite "Symbols"
    (suite "plain"
      ("write the value of a plain symbol"
        (tostring:write 'foo)
        "foo")

      ("disp the value of a plain symbol"
        (tostring:disp 'foo)
        "foo"))

    (suite "piped"
      (suite "plain"
        ("write the value of a plain symbol enclosed in pipes"
          (tostring:write '|foo|)
          "foo")

        ("disp the value of a plain symbol enclosed in pipes"
          (tostring:disp '|foo|)
          "foo"))

      (suite "containing #\\;"
        ("write '|foo;bar|"
          (tostring:write '|foo;bar|)
          "|foo;bar|")

        ("disp '|foo;bar|"
          (tostring:disp '|foo;bar|)
          "foo;bar"))

      (suite "containing #\\newline"
        ("write '|foo\\nbar|"
          (tostring:write (coerce "foo\nbar" 'sym))
          "|foo
bar|")

        ("disp '|foo\\nbar|"
          (tostring:disp (coerce "foo\nbar" 'sym))
          "foo
bar"))

      (suite "empty"
        ("write '||"
          (tostring:write '||)
          "||")

        ("disp '||"
          (tostring:disp '||)
          ""))

      (suite "dot"
        ("write '|.|"
          (tostring:write '|.|)
          "|.|")

        ("disp '|.|"
          (tostring:disp '|.|)
          "."))

      (suite "numbers and special chars need piping"
        ("21, for example"
          (coerce "21" 'sym)
          |21|)

        ("1.23E10"
          (coerce "1.23E10" 'sym)
          |1.23E10|)

        (";"
          (coerce ";" 'sym)
          |;|)

        ("NaN"
          (coerce "+nan.0" 'sym)
          |+nan.0|))

      (suite "it's not over yet"
        ("show sym with pipes"
          (coerce "|foo|" 'sym)
          \|foo\|)

        ("show sym with parens"
          (coerce "(foo)" 'sym)
          |(foo)|)

        ("write sym containing pipes"
          (tostring:write (coerce "|foo|" 'sym))
          "\\|foo\\|")

        ("disp sym containing pipes"
          (tostring:disp (coerce "|foo|" 'sym))
          "|foo|")

        ("sym containing double-quotes"
          (coerce "\"foo\"" 'sym)
          |"foo"|)

        ("sym containing single-quotes"
          (coerce "'" 'sym)
          |'|)

        ("let's go wild"
          (coerce "||||||||||||||" 'sym)
          \|\|\|\|\|\|\|\|\|\|\|\|\|\|)
      )

      (suite "displaying"
        ("empty symbol"
          (coerce (coerce "" 'sym) 'string)
          "")
      )
    )
  )

  ("timedate breaks date into calendar components"
    ((fn (d)
      (list d.5 d.4 d.3)) (timedate 1200700800))
    (2008 01 19))

  (suite "Strings"
    ("get the length of a string"
      (len "foo-bar-toto")
      12 )

    ("set a character in a string"
      ( (fn (s) (sref s #\b 2) s) "boo" )
      "bob" )

    ("+ concatenates"
      (+ "fo" "ob" "ar")
      "foobar")

    ("returns argth character"
      ("abcd" 2)
      #\c )
  )

  (suite "Characters"
    ("newstring makes a string of simple characters"
      (newstring 5 #\a)
      "aaaaa")

    ("newstring makes a string of ascii characters"
      (newstring 5 #\102)
      "BBBBB")
  )

  (suite "Tables"
    ("create a table"
      ( (fn () (assign x (table)) (sref x "foo" 'toto) (x 'toto) ) )
      "foo" )

    ("map a function over a table to retrieve keys"
      ((fn (hash hash2)
          (sref hash2 t "alpha")
          (sref hash2 t "beta")
          (sref hash2 t "gamma")
          (sref hash 'a "alpha")
          (sref hash 'b "beta")
          (sref hash 'c "gamma")
          (maptable (fn (k v) (sref hash2 nil k)) hash)
          (len hash2)
        ) (table) (table))
      0)

    ("map a function over a table to retrieve values"
      ((fn (hash hash2)
          (sref hash2 t 'a)
          (sref hash2 t 'b)
          (sref hash2 t 'c)
          (sref hash 'a "alpha")
          (sref hash 'b "beta")
          (sref hash 'c "gamma")
          (maptable (fn (k v) (sref hash2 nil v)) hash)
          (len hash2)
        ) (table) (table))
      0)

    ("constructor function"
      ((table [do (= _!foo 'bar) (= _!toto 'titi)]) 'foo)
      bar
    )

    ("get the size of a table"
      ( len ( (fn (hash)
              (sref hash 'a "alpha")
              (sref hash 'b "beta")
              (sref hash 'c "gamma")
              (sref hash 'd "pizza")
              hash
            ) (table)))
      4 )

    ("nil values not counted in table size"
      ( len ( (fn (hash)
              (sref hash 10  'a)
              (sref hash 12  'b)
              (sref hash 14  'c)
              (sref hash nil 'b)
              hash
            ) (table)))
      2))

  (suite "IO"
    (suite "String IO"
      (suite "writing"
        ("display to a string"
          ((fn ()
            (assign out (outstring))
            (disp "foobody" out)
            (inside out)
          ))
          "foobody")

        ("display to a string using call-w/stdout"
          ((fn ()
            (assign out (outstring))
            (call-w/stdout out (fn () (disp "barbody")))
            (inside out)
          ))
          "barbody")

        ("write a byte to a string"
          ((fn ()
            (assign out (outstring))
            (writeb 67 out)
            (inside out)
          ))
          "C")

        ("write a character to a string"
          ((fn ()
            (assign out (outstring))
            (writec #\F out)
            (inside out)
          ))
          "F")

        ("writec returns its argument" ; thanks rocketnia, http://arclanguage.org/item?id=12269
          (writec #\C (outstring))
          #\C)
      )

      (suite "reading"
        ("read from a string"
          (sread (instring "(foo bar yadda)") nil)
          (foo bar yadda))

        ("read from a string using call-w/stdin"
          ((fn (sin)
            (call-w/stdin sin (fn () (assign read-foo (sread sin nil)) (assign read-bar (sread sin nil))))
            `(,read-foo ,read-bar)
          ) (instring "foo-from-string bar-from-string"))
          (foo-from-string bar-from-string))

        ("read a byte from a string"
          (readb (instring "Every Valley"))
          69)

        ("read a character from a string"
          (readc (instring "dude bar yadda"))
          #\d)

        ("read a sequence of characters from a string"
          ((fn ()
            (assign inp (instring "pqrst"))
            (cons (readc inp) (cons (readc inp) (cons (readc inp) nil)))
          ))
          (#\p #\q #\r))
      )
    )

    (suite "system"
      ("system copies output to arc's stdout"
        ((fn (outstr)
             (call-w/stdout outstr (fn () (system "echo boggabogga")))
             (inside outstr)) (outstring))
        "boggabogga
"))

    (suite "File IO"
      ("file io"
        ((fn ()
          (assign fname (+ "/tmp/" (coerce (uniq) 'string)))
          (assign outp (outfile fname))
          (write '(a b c d (e f g)) outp)
          (close outp)
          (assign inp (infile fname))
          (assign alpha (sread inp nil))
          (close inp)
          alpha
        ))
        (a b c d (e f g)))))


  ("ignore comments" ; here's one, for example
    ; this is another comment
    nil
    nil ; including comments at the end of the line
; and comments at the very beginning
;         ( (fn (a b . c) (* (- a b) (apply + c))) 20 15 19 20 21)
; (also commented-out code)
)))

