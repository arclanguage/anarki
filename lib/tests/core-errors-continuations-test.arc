(register-test '(suite "Foundation Tests"
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
        (on-err (fn (ex) "got error") 
                (fn () (* 6 7)))
        42 )

      ("error"
        (on-err (fn (ex) (+ "got error " (details ex))) 
                (fn () (/ 42 0)))
        "got error /: division by zero" )

      ("explicit error"
        (on-err (fn (ex) (+ "got error " (details ex))) 
                (fn () (err "we can also throw our own exceptions")))
        "got error we can also throw our own exceptions" )
    )
  )))
