
(let (uarg rpname curcode copy-table) nil
  (= uarg (uniq) rpname (uniq) curcode (uniq))
  (= copy-table
     (fn (tb)
       (let ntb (table)
         (ontable k v tb
           (= (ntb k) v))
         ntb)))
  (mac repage body
    `((rfn ,rpname (,uarg)
        ,@body)
      (table)))
  (mac repage-point body
    (w/uniq code
      `(let ,curcode ',code
         (aif (,uarg ,curcode)
              (it)
              (do ,@body)))))
  (mac repage-link (ln . body)
    `(w/link
       (,rpname
         (fill-table (,copy-table ,uarg)
                     (list ,curcode (fn () ,@body))))
       (prn ,ln)))
  (mac repage-back-link (ln)
    `(w/link
       (,rpname
         (fill-table (,copy-table ,uarg)
                     (list ,curcode nil)))
       (prn ,ln))))

(defop repage-test args
  (repage
    (whitepage
      (center
        (prn "repage and friends test page")
        (br)
         (repage-point
           (tag b (prn "Choose one: "))
           (w/bars
             (repage-link "Foo"
               (prn "You chose the foo link. ")
               (repage-back-link "Go back"))
             (repage-link "Bar"
               (prn "You chose the bar link.")
               (repage-back-link "Go back."))))
        (br)
        (tag hr)
        (prn "So... how do you like it?")))))

#|
Note!  In spite of expectations, nested repage-point - repage-link
- repage-point
 (repage-point ....
   (repage-link ... (repage-point ...) ...))
do not work.  This has to do with the fact that w/link
creates functions, which capture the current lexical
environment.  When the outer repage-link creates a
function, it captures the outer repage-point as being
unopened at the time.  This means that the inner
repage-point's repage-links will also capture the
outer repage-point as being unopened, and will
feed that link as unopened.

It's kinda hard to explain, and I'm not 100% sure of my
explanation.
|#

(defop repage-test-bugged args
  (repage
    (whitepage
      (center
        (prn "repage macro and friends test page")
        (br)
        (repage-point:w/bars
          (tag b
            (prn "Choose one "))
          (repage-link " Foo "
            (prn "You chose the foo link. ")
            (repage-back-link " Go back."))
          (repage-link " Bar "
            (prn "You chose the bar link")
            (br)
            (repage-point
              (prn "Within the bar is: ")
              (w/bars
                (repage-link "qux"
                   (prn "It was the qux!!")
                   (repage-back-link "back off qux"))
                (repage-link "quux"
                   (prn "It was the quux!!  quux!!")
                   (repage-back-link "back off quux"))))
            (br)
            (repage-back-link "Quit the bar")))
        (br)
        (prn "Choose one of the above: because it's there")))))

