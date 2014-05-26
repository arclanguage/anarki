(suite-w/setup lru (calls 0
                    test-lru (lru 2
                                  [do (++ calls)
                                      (+ _ 1)]))
               lru-uses-body (assert-same 3
                                          (test-lru 2))
               lru-uses-body-once (do (test-lru 2)
                                      (assert-same 1
                                                   calls))
               lru-caches-result (do (test-lru 2)
                                     (test-lru 2)
                                     (assert-same 1
                                                  calls))
               cached-result-is-correct (do (test-lru 2)
                                            (assert-same 3
                                                         (test-lru 2)))
               capacity-of-2-caches-2-things (do (test-lru 2)
                                                 (test-lru 3)
                                                 (test-lru 2)
                                                 (test-lru 3)
                                                 (assert-same 2
                                                              calls))
               evicts-least-recently-used-call (do (test-lru 2)
                                                   (test-lru 27)
                                                   (test-lru 42)
                                                   (test-lru 2) ;;this should run the body code
                                                   (assert-same 4
                                                                calls))
               repeated-calls-yield-identical-results (do (test-lru 3)
                                                          (test-lru 42)
                                                          (test-lru 3)
                                                          (assert-same 4
                                                                       (test-lru 3))))
