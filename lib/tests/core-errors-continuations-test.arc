(def test-find-char (str c)
  (catch
    (let i -1
      (each ch (coerce str 'cons)
        (++ i)
        (if (is ch c)
            (throw i))))))

(register-test '(suite "Foundation Tests"
  (suite "Errors and Continuations"
    (suite "ccc"
      ("use ccc to return a value"
        (ccc (fn (esc) (esc "bailout value") 42))
        "bailout value")

      ("catch expands to ccc: call throw"
        (test-find-char "abcdefg" #\d)
        3)

      ("catch expands to ccc: nil result"
        (test-find-char "abcdefg" #\z)
        nil)

      ("support continuation-passing style to calculate hypoteneuse"
        ( (fn ((cps* cpsplus cps-sqrt cps-pyth))
          (assign cps* (fn (x y k) (k (* x y))))
          (assign cpsplus (fn (x y k) (k (+ x y))))
          (assign cps-sqrt (fn (x k) (k (sqrt x))))
          (assign cps-pyth (fn (x y k)
            (cps* x x (fn (x2)
              (cps* y y (fn (y2)
                (cpsplus x2 y2 (fn (x2py2)
                  (cps-sqrt x2py2 k)))))))))
          (< 6.40312423743284 (ccc (fn (cc) (cps-pyth 4 5 cc))) 6.40312423743285)) nil)
        t)

      ("support co-routines" ; adapted from http://community.schemewiki.org/?call-with-current-continuation
        ((fn (hefty-info)
          (assign hefty-stuff (fn (other-stuff)
            (assign rec-hefty (fn (n)
              (assign hefty-info (cons "A" (cons n hefty-info)))
              (assign other-stuff (ccc other-stuff))
              (if (> n 0) (rec-hefty (- n 1)))))
            (rec-hefty 5)))

          (assign light-stuff (fn (other-stuff)
            (assign rec-light (fn (x)
              (assign hefty-info (cons "B" hefty-info))
              (assign other-stuff (ccc other-stuff))
              (rec-light 0)))))

          (if (is hefty-info nil) (hefty-stuff light-stuff))

          hefty-info
        ) nil)
        ("B" "A" 0 "B" "A" 1 "B" "A" 2 "B" "A" 3 "B" "A" 4 "B" "A" 4 "A" 5))
    )

    (suite "Protect"
      ("simple protect"
        ((fn (x)
          (protect (fn () (/ 1 2)) (fn () (assign x "protected-foo")))
          x) nil)
        "protected-foo")

      ("protect through continuation"
        (tostring (catch (after (throw pr!problem) pr!-free)))
        "problem-free")

      ("protect all over the place inside a co-routine pair"
        (accum trace
          (assign proc-A (fn (my-b)
            (trace 'proc-A-start)
            (assign inner-A (fn (n)
              (trace (sym:string 'inner-A-start- n))
              (after (assign my-b (do (trace 'pre-ccc-my-b) (ccc my-b))) (trace (sym:string 'after-ccc-my-b- n)))
              (trace 'end-inner-A)
              (if (> n 0) (after (inner-A (- n 1)) (trace (sym:string 'after-inner-A-tail-call- n))))))
            (after (inner-A 5) (trace 'after-initial-inner-A-call))))

          (assign proc-B (fn (my-a)
            (trace 'proc-B-start)
            (assign inner-B (fn (x)
              (trace 'inner-B-start)
              (after (assign my-a (do (trace 'pre-ccc-my-a) (ccc my-a))) (trace 'after-ccc-my-a))
              (trace 'end-inner-B)
              (after (inner-B 0) (trace 'after-inner-B-tail-call))))))

          (after (proc-A proc-B) (trace 'final-after)))

        (proc-A-start inner-A-start-5 pre-ccc-my-b proc-B-start after-ccc-my-b-5
         end-inner-A inner-A-start-4 pre-ccc-my-b inner-B-start pre-ccc-my-a
         after-ccc-my-a after-ccc-my-b-4 after-inner-A-tail-call-5
         after-ccc-my-b-5 end-inner-A inner-A-start-4 pre-ccc-my-b
         after-ccc-my-b-4 after-inner-A-tail-call-5 after-ccc-my-a end-inner-B
         inner-B-start pre-ccc-my-a after-ccc-my-a after-inner-B-tail-call
         after-ccc-my-b-4 after-inner-A-tail-call-5 after-ccc-my-b-4 end-inner-A
         inner-A-start-3 pre-ccc-my-b after-ccc-my-b-3 after-inner-A-tail-call-4
         after-inner-A-tail-call-5 after-ccc-my-a end-inner-B inner-B-start
         pre-ccc-my-a after-ccc-my-a after-inner-B-tail-call
         after-inner-B-tail-call after-ccc-my-b-4 after-inner-A-tail-call-5
         after-ccc-my-b-3 end-inner-A inner-A-start-2 pre-ccc-my-b
         after-ccc-my-b-2 after-inner-A-tail-call-3 after-inner-A-tail-call-4
         after-inner-A-tail-call-5 after-ccc-my-a end-inner-B inner-B-start
         pre-ccc-my-a after-ccc-my-a after-inner-B-tail-call
         after-inner-B-tail-call after-inner-B-tail-call after-ccc-my-b-4
         after-inner-A-tail-call-5 after-ccc-my-b-2 end-inner-A inner-A-start-1
         pre-ccc-my-b after-ccc-my-b-1 after-inner-A-tail-call-2
         after-inner-A-tail-call-3 after-inner-A-tail-call-4
         after-inner-A-tail-call-5 after-ccc-my-a end-inner-B inner-B-start
         pre-ccc-my-a after-ccc-my-a after-inner-B-tail-call
         after-inner-B-tail-call after-inner-B-tail-call after-inner-B-tail-call
         after-ccc-my-b-4 after-inner-A-tail-call-5 after-ccc-my-b-1 end-inner-A
         inner-A-start-0 pre-ccc-my-b after-ccc-my-b-0 after-inner-A-tail-call-1
         after-inner-A-tail-call-2 after-inner-A-tail-call-3
         after-inner-A-tail-call-4 after-inner-A-tail-call-5 after-ccc-my-a
         end-inner-B inner-B-start pre-ccc-my-a after-ccc-my-a
         after-inner-B-tail-call after-inner-B-tail-call after-inner-B-tail-call
         after-inner-B-tail-call after-inner-B-tail-call after-ccc-my-b-4
         after-inner-A-tail-call-5 after-ccc-my-b-0 end-inner-A
         after-inner-A-tail-call-1 after-inner-A-tail-call-2
         after-inner-A-tail-call-3 after-inner-A-tail-call-4
         after-inner-A-tail-call-5 after-initial-inner-A-call final-after)
      )
    )

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
