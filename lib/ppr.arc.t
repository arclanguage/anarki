(= oneline* 5 ifline* 7)

(def collect-pretty-print (expr)
  (+ "\n" (tostring (ppr expr))))

(mac ppr-test-macro (a b c . body)
  `(list a b c body))

(suite pretty-print
       (test if-2
             (assert-same "\n(if abc\n  def)\n"
                          (collect-pretty-print '(if abc def))))
       (test if-3
             (assert-same "\n(if abc\n  def\n  ghi)\n"
                          (collect-pretty-print '(if abc def ghi))))
       (test if-5
             (assert-same "\n(if a b\n    c d\n      e)\n"
                          (collect-pretty-print '(if a b c d e))))
       (test if-5-wide-and-wavy
             (assert-same "\n(if abcdef\n      ghi\n    jkl\n      mno\n    pqr)\n"
                          (collect-pretty-print '(if abcdef ghi jkl mno pqr))))
       (test with
             (assert-same "\n(with (a b\n       c d)\n  (e f g))\n"
                          (collect-pretty-print '(with (a b c d) (e f g)))))
       (test custom-macro
             (assert-same "\n(ppr-test-macro (a b) (c d) (e f)\n  (g h i))\n"
                          (collect-pretty-print '(ppr-test-macro (a b) (c d) (e f) (g h i))))))
