(require 'lib/re.arc)

(suite re-match
       (suite simple-regex
              (test ex1
                    (assert-same '("a")
                                 (re-match (re "a|b") "cat")))
              (test ex2
                    (assert-same '("a")
                                 (re-match (re "[at]") "cat")))
              (test ex3
                    (assert-same '("caaat")
                                 (re-match (re "ca*[at]") "caaat")))
              (test ex4
                    (assert-same '("caaat")
                                 (re-match (re "ca+[at]") "caaat")))
              (test ex5
                    (assert-same '("ct")
                                 (re-match (re "ca?t?") "ct")))
              (test ex6
                    (assert-same '("ca")
                                 (re-match (re "ca*?[at]") "caaat")))
              (test ex7
                    (assert-same '("caa" "c" "aa")
                                 (re-match (re "(c*)(a*)") "caat")))
              (test ex8
                    (assert-same '("t")
                                 (re-match (re "[^ca]") "caat")))
              (test ex9
                    (assert-same '("cat" "a")
                                 (re-match (re ".(.).") "cat")))
              (test ex10
                    (assert-same '("c")
                                 (re-match (re "^a|^c") "cat")))
              (test ex11
                    (assert-same '("t")
                                 (re-match (re "a$|t$") "cat")))
              (test ex12
                    (assert-same '("|")
                                 (re-match (re "\\|") "c|t")))
              (test ex13
                    (assert-same '("ca")
                                 (re-match (re "[a-f]*") "cat")))
              (test ex14
                    (assert-same '("]")
                                 (re-match (re "[]]") "c]t")))
              (test ex15
                    (assert-same '("-")
                                 (re-match (re "[-]") "c-t")))
              (test ex16
                    (assert-same '("[a]")
                                 (re-match (re "[]a[]+") "c[a]t")))
              (test ex17
                    (assert-same '("a^")
                                 (re-match (re "[a^]+") "ca^t")))
              (test ex18
                    (assert-same '("na")
                                 (re-match (re ".a(?=p)") "cat nap")))
              (test ex19
                    (assert-same '("na")
                                 (re-match (re ".a(?!t)") "cat nap")))
              (test ex20
                    (assert-same '("ap")
                                 (re-match (re "(?<=n)a.") "cat nap")))
              (test ex21
                    (assert-same '("ap")
                                 (re-match (re "(?<!c)a.") "cat nap")))
              (test ex22
                    (assert-same '("Ap")
                                 (re-match (re "(?i:a)[tp]") "cAT nAp")))
              (test ex23
                    (assert-same '("ab")
                                 (re-match (re "(?(?<=c)a|b)+") "cabal")))
       )

       (suite perl-regex
              (test ex1
                    (assert-same '("caa")
                                 (re-match (pre "ca{2}") "caaat")))
              (test ex2
                    (assert-same '("caat")
                                 (re-match (pre "ca{2,}t") "catcaat")))
              (test ex3
                    (assert-same '("cat")
                                 (re-match (pre "ca{,2}t") "caaatcat")))
              (test ex4
                    (assert-same '("cat")
                                 (re-match (pre "ca{1,2}t") "caaatcat")))
              (test ex5
                    (assert-same '("caat" "a")
                                 (re-match (pre "c(.)\\1t") "caat")))
              (test ex6
                    (assert-same '("t ")
                                 (re-match (pre ".\\b.") "cat in hat")))
              (test ex7
                    (assert-same '("ca")
                                 (re-match (pre ".\\B.") "cat in hat")))
              (test ex8
                    (assert-same '("a")
                                 (re-match (pre "\\p{Ll}") "Cat")))
              (test ex9
                    (assert-same '("!")
                                 (re-match (pre "\\P{Ll}") "cat!")))
              (test ex10
                    (assert-same '("1ca")
                                 (re-match (pre "[a-f\\d]*") "1cat")))
              (test ex11
                    (assert-same '(" h")
                                 (re-match (pre " [\\w]") "cat hat")))
              (test ex12
                    (assert-same '("t\n")
                                 (re-match (pre "t[\\s]") "cat\nhat")))
              (test ex13
                    (assert-same '("at")
                                 (re-match (pre "[[:lower:]]+") "Cat")))
       ))
