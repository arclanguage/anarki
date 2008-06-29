(mac my-mac (x y) `(+ ,x ,y))
(mac my-plus (x y) `(+ ,x ,y))
(mac plusall args `(+ ,@args))
(mac the-mac args (if (is nil args) 1 `(* ,(car args) (the-mac ,@(cdr args)))))
(mac make-pair (a b) `(cons ,a ,b))
(mac new-repeat-mac (x n) (if (is n 0) nil `(make-pair ,x (new-repeat-mac ,x ,(- n 1)))))
(mac test-double (x) `(+ ,x ,x))
(mac act-of-god () 'earthquake)
(mac overwrite (after) `(+ ,after 3))

(register-test '(suite "Foundation Tests"
  (suite "Special Forms"
    (suite "Quotation"
      (suite "quote"
        ("quotation"
          'a
          a)

        ("quotes its argument"
          (quote a)
          a)

        ("quotes a list argument"
          (quote (1 2 3))
          (1 2 3))

        ("quotes a list of symbols"
          (quote (fooble bar))
          (fooble bar))

        ("quote char as shortcut"
          'foogle
          foogle )
      )

      (suite "quasiquote"
        ("quasi-quotation"
          '`qqfoo
          (quasiquote qqfoo) )

        ("unquote"
          ',uqfoo
          (unquote uqfoo)
        )

        ("unquote-splicing"
          ',@uqsfoo
          (unquote-splicing uqsfoo) )

        ("quasi-quotation is just like quotation"
          `qqqfoo
          qqqfoo )

        ("quais-quote quasi-quote"
          ``double-qq
          `double-qq )

        ("quasiquotation unquote is identity"
          ( (fn (x) `,x) "foo" )
          "foo" )

        ("quasiquotation of list with unquote"
          `(1 2 ,(+ 1 2))
          (1 2 3) )

        ("quasiquotation of list with unquote"
          `(a b c (+ 1 2 ,(+ 1 2)))
          (a b c (+ 1 2 3)) )

        ("quasiquotation of list with unquote in first position"
          `(,(+ 1 2) 2 1)
          (3 2 1) )

        ("quasiquotation of list with unquote-splicing"
          `(a b c (+ 1 2 ,@(cons 3 (cons 4 nil))))
          (a b c (+ 1 2 3 4)) )

        ("quasiquotation of list with unquote-splicing in first position"
          `(,@(cons 3 (cons 4 nil)) a b c ,(+ 0 5))
          (3 4 a b c 5) )

        ("don't expand nested quasiquotes"
          `(* 17 ,(plusall 1 2 3) `(+ 1 ,(plusall 1 2 3)))
          (* 17 6 `(+ 1 ,(plusall 1 2 3))))

        ("quasiquote uses local namespace"
          ((fn (x) (overwrite (+ x x))) 7)
          17
        )
      )
    )

    (suite "If"
      ("returns first argument if only one argument"
        (if 12)
        12 )

      ("any non-nil atom is t"
        (if 'toto 'titi)
        titi )

      ("returns nil if condition is false"
        (if (is 12 13) 'foo)
        nil )

      ("returns value corresponding to first true condition"
        (if (is 12 13) 'foo
            (is 'foo 'bar) 'toto
            (is "a" "a") "yadda"
            'doh
        )
        "yadda" )

      ("returns default value corresponding if no true condition"
        (if (is 12 13) 'foo
            (is 'foo 'bar) 'toto
            'doh
        )
        doh )
    )

    (suite "Function Definition"
      ("a simple no-args function that returns a string"
        ( (fn () "foobar") )
        "foobar")

      ("a simple addition function"
        ( (fn (x y) (+ x y)) 17 13)
        30)

      ("a simple addition function with varargs"
        ( (fn args (apply + args)) 17 13 14 16)
        60)

      ("a simple function with a body parameter"
        ( (fn (a b . c) (* (- a b) (apply + c))) 20 15 19 20 21)
        300)

      ("set a value inside a function"
        ( (fn () (set foo "bar") foo) )
        "bar")

      ("nested variables are lexically scoped"
        ( (fn ()
          ( (fn (x)
              (set toto (fn () x))) 99)
          (toto)) )
        99)

      ("nested variables are lexically scoped - really"
        ( (fn ()
          ((fn (a)
            ((fn (b)
              ((fn (c)
                (set abcfun (fn (x) (+ a b c x)))
              ) "toto")
            ) "bar")
          ) "foo")
          (abcfun "-foobartoto")
        ))
        "foobartoto-foobartoto")

      ("optional arguments fill in missing arguments"
        ((fn (x y (o z 2))
          (+ x y z)
        ) 31 41)
        74)

      ("optional values ignored if value provided"
        ((fn (x y (o z 2))
          (+ x y z)
        ) 31 41 3)
        75)

      ("defaults for optionals may be other args"
        ((fn (x y (o z x))
          (+ x y z)
        ) 16 33)
        65)

      ("optional args evaluated at invocation time, but in lexical scope of fn definition"
        ((fn ()
          (set fopt (fn (x (o y z)) (cons x y)))
          (set z 'goo)
          ( (fn (z) (fopt 2)) 'zoo)
        ))
        (2 . goo))

      ("optional arg may be invocation"
        ((fn ()
          (set fopt (fn (x (o y (+ z z))) (cons x y)))
          (set z 25)
          ( (fn (z) (fopt 2)) 101)
        ))
        (2 . 50))
        
      ("o is not always an optional arg"
        ((fn ()
          (set fioip (fn ((i o ip)) o))
          (fioip '(ifoo obar "iptoto"))
        ))
        obar)

      ("destructuring-bind params"
        ((fn (a b (c d e (f g)) h) `(,a ,b ,c ,d ,e ,f ,g ,h)) 1 2 '(3 4 5 (6 7)) 8)
         (1 2 3 4 5 6 7 8))

      ("destructuring-bind params with nested optional argument"
        ((fn (a b (c d e (f (o g 22))) h) `(,a ,b ,c ,d ,e ,f ,g ,h)) 1 2 '(3 4 5 (6)) 8)
         (1 2 3 4 5 6 22 8))

      ("destructuring-bind in first place"
          ((fn (out)
            (set foo (fn ((opt val) . rest) (disp opt) (disp "=") (disp val) (disp " ")  (if (is (car rest) nil) '() (foo (car rest) (cdr rest)))))
            (call-w/stdout out (fn () (foo '(id 4) '(class "myclass"))))
            (inside out)
          ) (outstring))
        "id=4 class=myclass ")

      ("empty body returns nil"
        ((fn ()))
        nil)
    )

    (suite "Set"
      ("sets a value in the top namespace"
        ( (fn () (set earthquake 10.3) earthquake))
        10.3)

      ("macro-expands first argument"
        ((fn ()
            (set (act-of-god) 8.9)
            earthquake))
        8.9)

      ("sets several values at once"
        ( (fn ()
                (set act-of-god (annotate 'mac (fn () 'earthquake)))
                (set volcano 2.4 (act-of-god) 10.3 tsunami 15.3 avalanche 4.1)
                `(,tsunami ,volcano ,avalanche ,earthquake)))
        (15.3 2.4 4.1 10.3))
    )
  )

  (suite "Typing"
    (suite "type"
      ("of nil is symbol"
        (type nil)
        sym)

      ("of empty list is also symbol"
        (type ())
        sym)

      ("of t is symbol"
        (type t)
        sym)

      ("of symbol"
        (type 'foo)
        sym)

      ("of string"
        (type "foo")
        string)

      ("of integer rational is int"
        (type 12)
        int)

      ("of integer rational is int"
        (type 12/3)
        int)

      ("of fractional rational is num"
        (type 1/4)
        num)

      ("of double is num"
        (type 3.1415)
        num)

      ("of integer double is int"
        (type 3.0)
        int)

      ("of pair is cons"
        (type '(a b c))
        cons)

      ("of procedure is fn"
        (type (fn (x) x))
        fn)

      ("of character"
        (type #\a)
        char)

      ("of character"
        (type #\newline)
        char)

      ("of table"
        (type (table))
        table)
    )

    (suite "coerce"
      ("nil" ; news url posting requires this somewhere
        (coerce nil 'string)
        "")

      (suite "strings"
        ("list"
          (coerce "abc" 'cons)
          (#\a #\b #\c))

        ("sym"
          (coerce "foo" 'sym)
          foo)

        ("empty string is ||"
          (coerce "" 'sym)
          ||)

        ("int"
          (coerce "256" 'int)
          256)

        ("int"
          (coerce "12/4" 'int)
          3)

        ("int even if it's not an integer"
          (coerce "3/4" 'int)
          3/4)

        ("int even if it's not an integer in a given base"
          (coerce "101/100" 'int 2)
          5/4)

        ("int even if it's a double"
          (coerce "14.2857" 'int)
          14.2857)

        ("int even if it's a double with an exponent"
          (coerce "1.3E10" 'int)
          13000000000.0)

        ("exponent is case-insensitive"
          (coerce "1.3e10" 'int)
          13000000000.0)

        ("exponent may have + sign"
          (coerce "1.3e+10" 'int)
          13000000000.0)

        ("exponent may have - sign"
          (< 0.012999 (coerce "1.3e-2" 'int) 0.013001)
          t)

        ("string to int even if it's a double in another base"
          (coerce "10.11" 'int 2)
          2.75)

        ("string to int even if it's a double in another base with an exponent"
          (coerce "10.11E1010" 'int 2)
          2816.0)

        ("string to int even if it's a double in another base with an exponent"
          (coerce "10.11E1010" 'int 3)
          709180566103791.0)

        ("string to int even if it's a double in another base with an exponent (simpler case)"
          (coerce "101E100" 'int 2)
          80.0)

        ("string to int of given base"
          (coerce "FF" 'int 16)
          255)
      )

      (suite "iso"
        ("sym"
          (coerce 'foo 'sym)
          foo)

        ("string"
          (coerce "montmartre" 'string)
          "montmartre")

        ("int"
          (coerce 2079 'int)
          2079)
      )

      ("sym to string"
        (coerce 'foo 'string)
        "foo" )

      ("list to string"
        (coerce '(#\z #\o #\o #\, #\?) 'string)
        "zoo,?" )

      ("list to string"
        (coerce '(#\a #\b #\c #\@ #\e #\x #\a #\m #\p #\l #\e #\. #\c #\o #\m) 'string)
        "abc@example.com" )

      (suite "characters"
        ("char to int"
          (coerce #\A 'int)
          65)

        ("char to string"
          (coerce #\A 'string)
          "A")

        ("char to sym"
          (coerce #\g 'sym)
          g)
      )

      (suite "numbers"
        ("int to string"
          (coerce 66 'string)
          "66")

        ("int to char"
          (coerce 67 'char)
          #\C)

        ("int to string of given base"
          (coerce 63 'string 8)
          "77")

        ("double to string"
          (coerce 45.23e-5 'string)
          "0.0004523")

        ("fraction to string in another base"
          (coerce 41/13 'string 8)
          "51/15")

        ("double to int"
          (coerce 3.14 'int)
          3.0 )
      )
    )

    (suite "Annotation"
      ("get the type of an annotation"
        (type (annotate 'foo 'bar))
        foo )

      ("get the value of an annotation"
        (rep (annotate 'foo 'bar))
        bar )
    )
  )

  (suite "Macros"
    (suite "expansion"
      ("identity for non-macros"
        ((fn ()
          (set my-fn (fn (x y) `(+ ,x ,y)))
          (macex '(my-fn 19 71))
        ))
        (my-fn 19 71))

      ("identity for atoms"
        ((fn ()
          (macex 'foo)
        ))
        foo)

      ("identity for atoms, really"
        ((fn ()
          (macex 13)
        ))
        13)

      ("identity for undefined symbols"
        (macex '(i-do-not-exist 'blah))
        (i-do-not-exist 'blah) )

      ("expand a simple macro"
        ((fn ()
          (set do (annotate 'mac
                    (fn args `((fn () ,@args)))))
          (macex '(do a b c))
        ))
        ((fn () a b c)))

      ("expand a simple macro"
        ((fn ()
          (macex '(my-plus 19 71))
        ))
        (+ 19 71))

      ("expand a recursive macro"
        ((fn ()
          (set my-mac (annotate 'mac (fn (x) (if (is x 0) 0 `(+ ,x (my-mac ,(- x 1)))))))
          (macex '(my-mac 3))
        ))
        (+ 3 (my-mac 2)))

      ("expand a repeat macro"
        ((fn ()
          (set repeat-mac (annotate 'mac (fn (x n) (if (is n 0) nil `(cons ,x (repeat-mac ,x ,(- n 1)))))))
          (macex '(repeat-mac 'foo 14))
        ))
        (cons 'foo (repeat-mac 'foo 13)))

      ("nested expansion"
        ((fn ()
          (set make-pair (annotate 'mac (fn (a b) `(cons ,a ,b))))
          (set repeat-mac (annotate 'mac (fn (x n) (if (is n 0) nil `(make-pair ,x (repeat-mac ,x ,(- n 1)))))))
          (macex '(repeat-mac 'foo 14))
        ))
        (cons (quote foo) (repeat-mac (quote foo) 13)))

      ("more expansion"
        ((fn ()
          (set do (annotate 'mac
                    (fn args `((fn () ,@args)))))
          (set mac (annotate 'mac
                     (fn (name parms . body)
                       `(do (sref sig ',parms ',name)
                            (safeset ,name (annotate 'mac (fn ,parms ,@body)))))))
          (macex '(mac foo (x y) (prn "foo" x y) `(,x y ,y)))
        ))
        ((fn () (sref sig (quote (x y)) (quote foo)) (safeset foo (annotate (quote mac) (fn (x y) (prn "foo" x y) (quasiquote ((unquote x) y (unquote y))))))))
      )
    )

    (suite "invocation"
      ("do"
        ((fn ()
          (set do (annotate 'mac
                    (fn args `((fn () ,@args)))))
          (do (set x 3.14) (set y 2) (* x y))
        ))
        6.28 )

      ("invoke a simple macro"
        ((fn ()
          (set my-plus (annotate 'mac (fn (x y) `(+ ,x ,y))))
          (my-plus 19 71)
        ))
        90 )

      ("invoke a recursive macro"
        ((fn ()
          (set my-mac-caller (fn () (the-mac 5 6 7)))
          (my-mac-caller)
        ))
        210 )

      ("repeat macro"
        ((fn ()
          (set repeat-mac (annotate 'mac (fn (x n) (if (is n 0) nil `(cons ,x (repeat-mac ,x ,(- n 1)))))))
          (repeat-mac 'foo 3)
        ))
        (foo foo foo) )

      ("nested macro"
        (new-repeat-mac 'foo 4)
        (foo foo foo foo))
    )
  )

  (suite "Errors and Continuations"
    (suite "ccc"
      ("use ccc to return a value"
        (ccc (fn (esc) (esc "bailout value") 42))
        "bailout value")

      ("support continuation-passing style to calculate hypoteneuse"
        ( (fn ()
          (set cps* (fn (x y k) (k (* x y))))
          (set cps+ (fn (x y k) (k (+ x y))))
          (set cps-sqrt (fn (x k) (k (sqrt x))))
          (set cps-pyth (fn (x y k)
            (cps* x x (fn (x2)
              (cps* y y (fn (y2)
                (cps+ x2 y2 (fn (x2py2)
                  (cps-sqrt x2py2 k)))))))))
          (< 6.40312423743284 (ccc (fn (cc) (cps-pyth 4 5 cc))) 6.40312423743285)
        ))
        t)

      ("support co-routines" ; adapted from http://community.schemewiki.org/?call-with-current-continuation
        ((fn (hefty-info)
          (set hefty-stuff (fn (other-stuff)
            (set rec-hefty (fn (n)
              (set hefty-info (cons "hefty" (cons n hefty-info)))
              (set other-stuff (ccc other-stuff))
              (if (> n 0) (rec-hefty (- n 1)))))
            (rec-hefty 5)))

          (set light-stuff (fn (other-stuff)
            (set rec-light (fn (x)
              (set hefty-info (cons "light" hefty-info))
              (set other-stuff (ccc other-stuff))
              (rec-light 0)))))

          (if (is hefty-info nil) (hefty-stuff light-stuff))

          hefty-info
        ) nil)
        ("light" "hefty" 0 "light" "hefty" 1 "light" "hefty" 2 "light" "hefty" 3 "light" "hefty" 4 "light" "hefty" 4 "hefty" 5))
    )

    ("protect"
      ((fn ()
        (protect (fn () (/ 1 2)) (fn () (set x "protected-foo")))
        x
      ))
      "protected-foo")

    (suite "Error handling"
      ("no error"
        ((fn()
          (on-err (fn (ex) "got error") (fn () (* 6 7)))
        ))
        42 )

      ("error"
        ((fn()
          (on-err (fn (ex) (+ "got error " (details ex))) (fn () (/ 42 0)))
        ))
        "got error /: division by zero" )

      ("explicit error"
        ((fn()
          (on-err (fn (ex) (+ "got error " (details ex))) (fn () (err "we can also throw our own exceptions")))
        ))
        "got error we can also throw our own exceptions" )
    )
  )

  (suite "Lists"
    (suite "cons"
      ("cons creates a list"
        (cons 'a '(b c))
        (a b c))

      ("cons conses two strings"
        (cons "a" "b")
        ("a" . "b"))
    )

    (suite "car"
      ("car of nil is nil"
        (car nil)
        nil)

      ("car of empty list is nil"
        (car '())
        nil)

      ("car - no need to quote empty list"
        (car ())
        nil)

      ("car returns car of argument"
        (car '(foo 12.34 "bar"))
        foo)
    )

    (suite "cdr"
      ("cdr returns cdr of argument"
        (cdr '("foo" bar 123.45))
        (bar 123.45))

      ("cdr of empty list is nil"
        (cdr ())
        nil)

      ("'each' macro relies on cdr of empty list returning nil"
        ((fn stuff ((fn (outer) (((fn (self) (set self (fn (inner) (if (is (type inner) 'cons) ((fn () ((fn (x) (disp x)) (car inner)) (self (cdr inner))))) ))) nil) outer)) stuff)))
        nil)
    )

    (suite "scar"
      ("sets the first element of a list"
        ( (fn (x) (scar x 99) x ) '(1 2 3))
        (99 2 3) )

      ("sets the first character of a string"
        ( (fn (x) (scar x #\b) x ) "foo" )
        "boo" )
    )

    (suite "scdr"
      ("scdr sets the remainder of a list"
        ( (fn (x) (scdr x '(a a)) x ) '(a b c) )
        (a a a) )

      ("scdr sets the remainder of any list"
        ( (fn (x) (scdr (cdr (cdr x)) '(d e f)) x ) '(a b c) )
        (a b c d e f) )
    )

    ("get the size of a list"
      (len '(a b c d e f g))
      7 )

    ("set an element of a list"
      ( (fn (lst) (sref lst 'b 0) lst) '(a b c) )
      (b b c) )

    ("get an element of a list"
      ( '(a b c d) 2 )
      c )
  )

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

  (suite "Predicates"
    (suite "bound"
      ("true for items set with set"
        ( (fn () (set foo 'bar) (bound 'foo)) )
        t )

      ("false for unset items"
        (bound 'toto-yadda)
        nil )

      ("false for local parameters"
        ( (fn (yadda-toto) (bound 'yadda-toto)) "blah" )
        nil )
    )

    (suite "<"
      ("t if first number argument is less than second"
        (< 1 2)
        t)

      ("nil if first number argument is greater than second"
        (< 20 5)
        nil)

      ("nil if first number argument is equal to second"
        (< 20 20)
        nil)

      ("t if first string argument is less than second"
        (< "bar" "foo")
        t)

      ("nil if first string argument is greater than second"
        (< "zizi" "alpha")
        nil)

      ("nil if first string argument is equal to second"
        (< "foo" "foo")
        nil)

      ("t if first symbol argument is less than second"
        (< 'bar 'foo)
        t)

      ("nil if first symbol argument is greater than second"
        (< 'zizi 'alpha)
        nil)

      ("nil if first symbol argument is equal to second"
        (< 'foo 'foo)
        nil)

      ("is variadic"
        (< 1 2 3 4)
        t)

      ("is variadic"
        (< 1 2 10 4)
        nil)

      ("works on characters too"
        (< #\a #\c #\z)
        t)

      ("works on characters too"
        (< #\z #\c #\a)
        nil)
    )

    (suite ">"
      ("nil if first number argument is less than second"
        (> 1 2)
        nil)

      ("t if first number argument is greater than second"
        (> 20 5)
        t)

      ("nil if first number argument is equal to second"
        (> 20 20)
        nil)

      ("nil if first string argument is less than second"
        (> "bar" "foo")
        nil)

      ("t if first string argument is greater than second"
        (> "zizi" "alpha")
        t)

      ("nil if first string argument is equal to second"
        (> "foo" "foo")
        nil)

      ("nil if first symbol argument is less than second"
        (> 'bar 'foo)
        nil)

      ("t if first symbol argument is greater than second"
        (> 'zizi 'alpha)
        t)

      ("nil if first symbol argument is equal to second"
        (> 'foo 'foo)
        nil)

      ("is variadic"
        (> 4 3 2 1)
        t)

      ("is variadic"
        (> 100 20 40 10)
        nil)

      ("works on characters too"
        (> #\a #\c #\z)
        nil)

      ("works on characters too"
        (> #\z #\c #\a)
        t)
    )

    (suite "exact"
      ("t for integers"
        (exact 12)
        t)

      ("nil for reals even if they are integral"
        (exact 12.0)
        nil)

      ("t for fractions evaluating to integer"
        (exact 12/4)
        t)

      ("nil for fractions evaluating to non-integer"
        (exact 12/5)
        nil)

      ("nil for non-integers"
        (exact 3.14)
        nil)
    )

    (suite "is"
      ("t for two nils"
        (is nil nil)
        t)

      ("t for two t's"
        (is t t)
        t)

      ("t is not nil"
        (is t nil)
        nil)

      ("nil is not t"
        (is nil t)
        nil)

      ("t for the same characters"
        ((fn ()
          (set ss (coerce "  " 'cons))
          (is (car ss) #\space)
        ))
        t)

      ("t for the same characters"
        ((fn ()
          (set ss (coerce "
  " 'cons))
          (is (car ss) #\newline)
        ))
        t)

      ("t for the same numbers"
        (is 2 2)
        t)

      ("t for the same numbers"
        (is 22/7 22/7)
        t)

      ("t for the same numbers"
        (is 1.618 1.618)
        t)

      ("t for the same strings"
        (is "foobar" "foobar")
        t)

      ("nil for different integers"
        (is 2 3)
        nil)

      ("nil for same numbers but different types"
        (is 3/4 0.75)
        nil)

      ("nil for different reals"
        (is 1.1234567 1.1234569)
        nil)

      ("nil for different strings"
        (is "foobar" "toto")
        nil)

      ("variadic for numbers"
        (is 1.23 1.23 1.23 1.23)
        t)

      ("variadic for different numbers"
        (is 1.23 1.23 1.23 3.14159265)
        nil)

      ("variadic for strings"
        (is "f" "f" "f" "f" "f" "f")
        t)

      ("variadic for different strings"
        (is "f" "f" "f" "g" "f" "f")
        nil)

      ("variadic for symbols"
        (is 'f 'f 'f 'f 'f 'f)
        t)

      ("variadic for different symbols"
        (is 'f 'f 'f 'g 'f 'f)
        nil)
        
      ("nil for different lists, even with identical content" ; see http://arclanguage.org/item?id=6985
        ((fn (a b) (is a b)) (list 'cons) (list 'cons))
        nil)
        
      ("t for empty lists, however"
        (is () ())
        t)
    )
  )

  (suite "Maths"
    (suite "+"
      ("sums all args"
        (+ 1 2 3)
        6)

      ("sums all args"
        (+ 1.2 3.4 5.6)
        10.2)

      ("concatenates lists"
        (+ '(a b) '(c d) '(e f))
        (a b c d e f))

      ("concatenates lists ignoring nil"
        (+ '(a b) nil '(c d) nil '(e f))
        (a b c d e f))

      ("converts fraction to int if no loss"
        (+ 12 20/4)
        17 )

      ("retains fraction if not integer"
        (+ 12 20/7)
        104/7 )

      ("retains double even for integer result"
        (+ 13 2.0)
        15.0 )

      ("simplifies fractions"
        (+ 1/10 1/15)
        1/6 )
    )

    (suite "-"
      ("subtracts second arg from first"
        (- 25 7)
        18)

      ("subtracts all subsequent integer args from first"
        (- 1 2 3)
        -4)

      ("subtracts all subsequent double args from first"
        (- 1.2 3.4 5.6)
        -7.8)

      ("converts fraction to int if no loss"
        (- 12 20/4)
        7 )

      ("simplifies fractions"
        (- 1/10 1/15)
        1/30 )
    )

    (suite "*"
      ("multiplies integers"
        (* 23 7)
        161)

      ("multiplies floats"
        (* 2.4 3.5)
        8.4)

      ("multiplies fractions"
         (* 5 1/2)
         5/2 )

      ("multiplies fractions and converts to double"
         (* 2.5 7/2)
         8.75 )

      ("multiplies fractions and converts to double regardless of parameter order"
         (* 7/2 2.5)
         8.75 )

      ("converts fraction to int if no loss"
        (* 9/4 4/3)
        3 )

      ("reads exponents"
        (* 1000000 4e-5)
        40.0)

      ("reads exponents"
        (* 1000000 4.01e-5)
        40.1)
    )

    (suite "/"
      ("divides integers"
        (/ 192 6)
        32)

      ("divides floats"
        (< 3.19999 (/ 11.2 3.5) 3.20001)
        t)

      ("divides first arg by product of remaining args"
        (/ 192 3 2)
        32)

      ("divides first arg by product of remaining args"
        (/ 30 2 2.5)
        6.0)
    )

    (suite "trunc"
      ("truncates integers with no effect"
        (trunc 13.0)
        13)

      ("truncates floats"
        (trunc 3.1415)
        3)
    )

    (suite "expt"
      ("simple integer exponents"
        (expt 2.5 3)
        15.625 )

      ("double exponents"
        (expt 100 0.5)
        10.0 )

      ("more double exponents"
        (expt 1000 0.5)
        31.622776601683793 )
    )

    (suite "sqrt"
      ("simple square root"
        (sqrt 6.25)
        2.5 )

      ("non-integer roots"
        (sqrt 2)
        1.4142135623730951)

      ("root of double"
        (sqrt 3.14)
        1.772004514666935 )
    )

    (suite "mod"
      ("for positive integers"
        (mod 10 3)
        1)

      ("for negative integers"
        (mod -10 3)
        2)

      ("for negative integers"
        (mod -21 5)
        4)

      ("for fractions"
        (mod -77/7 5)
        4)
    )
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
      4 )

  )

  (suite "Evaluation"
    (suite "_ shortcut"
      ("call directly"
        ([* _ _] 15)
        225
      )

      ("useful in apply"
        (apply [* _ _] '(16))
        256
      )
    )

    (suite "apply"
      ("a simple sum function"
        (apply + '(1 2))
        3)

      ("an inline function"
        (apply (fn (x y) (* x y)) '(17 3))
        51)

      ("passes all args to function"
        (apply + "a" "b" '("c" "d"))
        "abcd" )

      ("passes all args to function"
        (apply + '(a b) '(c d) '((e f) (g h)))
        (a b c d e f g h) )
    )

    (suite "eval"
      ("a simple sum function"
        (eval '(+ 21 4))
        25)

      ("an inline function invocation"
        (eval '( (fn (x y) (* x y)) 16 4))
        64)
    )

    (suite "ssexpand"
      ("expand compose"
        (ssexpand 'x:y)
        (compose x y))

      ("expand complement"
        (ssexpand '~p)
        (complement p))

      ("expand compose/complement"
        (ssexpand 'p:~q:r)
        (compose p (complement q) r) )

      ("expand compose/complement"
        (ssexpand '~p:q:r)
        (compose (complement p) q r) )

      ("expand list"
        (ssexpand '+.a.b)
        (+ a b))

      ("expand quoted list"
        (ssexpand 'cons!a!b)
        (cons (quote a) (quote b)) )
    )

    (suite "ssyntax"
      ("recognises compose"
        (ssyntax 'a:b)
        t )

      ("recognises complement and compose"
        (ssyntax '~a:b)
        t )

      ("recognises complement alone"
        (ssyntax '~a)
        t )

      ("recognises list"
        (ssyntax 'a.b)
        t )

      ("recognises list-quoted"
        (ssyntax 'a!b)
        t )
    )

    (suite "special syntax invocation (compose is implemented in Arc)"
      ("direct invocation"
        ((fn ()
          (sqrt:+ 40 2.25)
        ))
        6.5 )

      ("compose macro invocation"
        (coerce (test-double:sqrt 256) 'int)
        32)

      ("invoke as parameter"
        ((fn ()
          (set addand (fn (op x y z) (+ z (op x y))))
          (addand sqrt:* 5 20 1.0)
        ))
        11.0 )
    )
  )

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
          ((fn ()
            (set sin (instring "foo-from-string bar-from-string"))
            (call-w/stdin sin (fn () (set read-foo (sread sin nil)) (set read-bar (sread sin nil))))
            `(,read-foo ,read-bar)
          ))
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
        ((fn () (set out (outstring)) (call-w/stdout out (fn () (system "echo boggabogga"))) (inside out)))
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

