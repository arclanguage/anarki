; narrow lines for simpler test cases

; running this test messes up the pretty-printer
(load "unit-test.arc/unit-test.arc")
(= oneline* 5 ifline* 7)

; helper
(def collect-pretty-print (expr)
  (+ "\n"  ; so expected string can start on an unindented line
     (tostring
       (ppr expr))))

(suite pretty-print
  if-2  (assert-same "
(if abc
  def)
"
                     (collect-pretty-print '(if abc def)))
  if-3  (assert-same "
(if abc
  def
  ghi)
"
                     (collect-pretty-print '(if abc def ghi)))
  if-5  (assert-same "
(if a b
    c d
      e)
"
                     (collect-pretty-print '(if a b c d e)))
  if-5-wide-and-wavy  (assert-same "
(if abcdef
      ghi
    jkl
      mno
    pqr)
"
                                   (collect-pretty-print '(if abcdef ghi jkl mno pqr)))

  with (assert-same "
(with (a b
       c d)
  (e f g))
"
                    (collect-pretty-print '(with (a b c d) (e f g))))
)

(run-all-suites)
