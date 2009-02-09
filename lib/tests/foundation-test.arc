
(register-test '(suite "Foundation Tests"
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
      ( (fn () (set x (table)) (sref x "foo" 'toto) (x 'toto) ) )
      "foo" )

    ("map a function over a table to retrieve keys"
      ((fn (hash)
          (sref hash 'a "alpha")
          (sref hash 'b "beta")
          (sref hash 'c "gamma")
          (maptable (fn (k v) (eval `(set ,(coerce k 'sym) ',v))) hash)
          (list alpha beta gamma)
        ) (table))
      (a b c))

    ("map a function over a table to retrieve values"
      ((fn (hash)
          (sref hash 'a "alpha")
          (sref hash 'b "beta")
          (sref hash 'c "gamma")
          (maptable (fn (k v) (eval `(set ,v ,k))) hash)
          (list a b c)
        ) (table))
      ("alpha" "beta" "gamma") )

    ("get the size of a table"
      ( len ( (fn (hash)
              (sref hash 'a "alpha")
              (sref hash 'b "beta")
              (sref hash 'c "gamma")
              (sref hash 'd "pizza")
              hash
            ) (table)))
      4 ))

  (suite "IO"
    (suite "String IO"
      (suite "writing"
        ("display to a string"
          ((fn ()
            (set out (outstring))
            (disp "foobody" out)
            (inside out)
          ))
          "foobody")

        ("display to a string using call-w/stdout"
          ((fn ()
            (set out (outstring))
            (call-w/stdout out (fn () (disp "barbody")))
            (inside out)
          ))
          "barbody")

        ("write a byte to a string"
          ((fn ()
            (set out (outstring))
            (writeb 67 out)
            (inside out)
          ))
          "C")

        ("write a character to a string"
          ((fn ()
            (set out (outstring))
            (writec #\F out)
            (inside out)
          ))
          "F")
      )

      (suite "reading"
        ("read from a string"
          (sread (instring "(foo bar yadda)") nil)
          (foo bar yadda))

        ("read from a string using call-w/stdin"
          ((fn (sin)
            (call-w/stdin sin (fn () (set read-foo (sread sin nil)) (set read-bar (sread sin nil))))
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
            (set inp (instring "pqrst"))
            (cons (readc inp) (cons (readc inp) (cons (readc inp) nil)))
          ))
          (#\p #\q #\r))
      )
    )

    (suite "system"
      ("system copies output to arc's stdout"
        ((fn () 
             (set out (outstring)) 
             (call-w/stdout out (fn () (system "echo boggabogga"))) 
             (inside out)))
        "boggabogga
")
    )

    (suite "File IO"
      ("file io"
        ((fn ()
          (set fname (+ "/tmp/" (coerce (uniq) 'string)))
          (set outp (outfile fname))
          (write '(a b c d (e f g)) outp)
          (close outp)
          (set inp (infile fname))
          (set alpha (sread inp nil))
          (close inp)
          alpha
        ))
        (a b c d (e f g)))
    )
  )

  ("ignore comments" ; here's one, for example
    ; this is another comment
    nil
    nil ; including comments at the end of the line
; and comments at the very beginning
;         ( (fn (a b . c) (* (- a b) (apply + c))) 20 15 19 20 21)
; (also commented-out code)
  )
))

