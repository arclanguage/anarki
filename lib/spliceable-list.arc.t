(require 'lib/spliceable-list.arc)

(suite spliceable-list
       (setup one-element    (spliceable-list 2 '(1))
              two-elements   (spliceable-list 2 '(1 2))
              three-elements (spliceable-list 2 '(1 2 3))
              four-elements  (ret temp-three-elements (spliceable-list 2 '(1 2 3))
                               (nappend temp-three-elements 4)))
       (test initializes-without-list
             (assert-same (obj contents  nil
                               last  nil
                               suffix-len  2
                               pre-suffix  nil)
                          (rep:spliceable-list 2)))
       (test initializes-with-list
             (assert-same (obj contents  list.1
                               last  list.1
                               suffix-len  2
                               pre-suffix  nil)
                          (rep:spliceable-list 2 '(1))))
       (test suffix-is-empty-if-list-is-too-short
             (assert-nil (suffix one-element)))
       (test suffix-is-list-if-just-long-enough
             (assert-same '(1 2) (suffix two-elements)))
       (test appending-works
             (assert-same (obj contents  '(1 2)
                               last  list.2
                               pre-suffix  nil
                               suffix-len  2)
                          (do (nappend one-element 2)
                              (rep one-element))))
       (test suffix-3
             (assert-same '(2 3)
                          (do (nappend one-element 2)
                              (nappend one-element 3)
                              (suffix one-element))))
       (test splicing-without-suffix
             (assert-same '(1) (splice three-elements)))
       (test suffix-4
             (assert-same '(3 4) (suffix four-elements)))
       (test appending-to-spliceable-list-updates-suffix
             (assert-same (obj contents  '(1 2 3 4)
                               last  list.4
                               pre-suffix  '(2 3 4)
                               suffix-len  2)
                          (rep four-elements)))
       (test splicing-with-suffix-works
             (assert-same '(1 2) (splice four-elements)))

       (test nslide-with-empty-list-is-idempotent
             (assert-same (spliceable-list 2 '(1 2 3))
                          (ret x (spliceable-list 2 '(1 2 3))
                            (nslide x '()))))
       (test nslide-on-empty-list-works
             (assert-same (obj contents  '(1 2 3 4)
                               last  list.4
                               pre-suffix  '(2 3 4)
                               suffix-len  2)
                          (let x (spliceable-list 2)
                            (nslide x '(1 2 3 4))
                            rep.x)))
       (test nslide-on-empty-list-can-return-nil-suffix
             (assert-same (obj contents  '(1 2)
                               last  list.2
                               pre-suffix  nil
                               suffix-len  2)
                          (let x (spliceable-list 2)
                            (nslide x '(1 2))
                            rep.x)))
       (test suffix-after-short-nslide-on-empty-list-remains-nil
             (assert-nil (let x (spliceable-list 4)
                           (nslide x '(1 2))
                           suffix.x)))
       (test suffix-after-short-nslide-remains-nil
             (assert-nil (let x (spliceable-list 4 '(1))
                           (nslide x '(2 3))
                           suffix.x)))
)
