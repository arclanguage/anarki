(require 'lib/collect.arc)

(suite collect
  (test for (assert-same '(1 2 3)
                         (collect i (for i from 1 to 3))))
  (test down (assert-same '(3 2 1)
                          (collect i (for i from 3 down to 1))))
  (test real-for (assert-same '(0 1 2)
                              (collect i (for i 0 (< i 3) ++.i))))
  (test filter (assert-same '(1 3 5)
                            (collect i (for i from 1 to 5)
                                       (if odd.i))))
  (test expr (assert-same '(2 4 6)
                          (collect (* i 2)
                                   (for i from 1 to 3))))
  (test compound (assert-same '((1 2) (1 3) (2 3))
                              (collect (list i j)
                                       (for i from 1 to 3)
                                       (for j from 1 to 3)
                                       (if (< i j)))))
  (test disambiguate (assert-same '(1 2 3)
                                  (let from 1
                                    (collect i (for i from (<= i 3) ++.i)))))
  (test lists (assert-same '(2 4 6)
                           (let l '(1 2 3)
                             (collect (* i 2) (for i in l)))))
  (test tables (assert-same '(3 7 11)
                            (let h (obj 1 2 3 4 5 6)
                              (sort < (collect (+ k v) (for (k v) in h))))))

  (suite ingest
    (test zero-lists (assert-same nil (ingest)))
    (test one-list (assert-same '(1 2 3)
                                (ingest '(1 2 3))))
    (test two-lists (assert-same '(1 2 3
                                     (4 5 6))
                                 (ingest '(1 2 3)
                                         '(4 5 6))))
    (test three-lists (assert-same '(1 2 3
                                       (4 5 6
                                          (7 8 9)))
                                   (ingest '(1 2 3)
                                           '(4 5 6)
                                           '(7 8 9)))))

)
