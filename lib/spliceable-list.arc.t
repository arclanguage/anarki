(suite spliceable-list
       (setup one-element    (spliceable-list 2 '(1))
              two-elements   (spliceable-list 2 '(1 2))
              three-elements (spliceable-list 2 '(1 2 3))
              four-elements  (ret temp-three-elements (spliceable-list 2 '(1 2 3))
                               (append temp-three-elements '(4))))
       (test initializes-without-list
             (assert-same (obj 
                            contents
                            nil
                            last
                            nil
                            suffix-len
                            3
                            suffix
                            nil)
                          (rep:spliceable-list 2)))
       (test initializes-with-list
             (assert-same (obj 
                            contents
                            list.1
                            last
                            list.1
                            suffix-len
                            3
                            suffix
                            nil)
                          (rep:spliceable-list 2 '(1))))
       (test suffix-is-empty-if-list-is-too-short
             (assert-nil (suffix one-element)))
       (test suffix-is-list-if-just-long-enough
             (assert-same '(1 2) (suffix two-elements)))
       (test appending-works
             (assert-same (obj 
                            contents
                            '(1 2)
                            last
                            list.2
                            suffix
                            nil
                            suffix-len
                            3)
                          (do (append one-element (list 2))
                              (rep one-element))))
       (test suffix-3
             (assert-same '(2 3)
                          (do (append one-element (list 2))
                              (append one-element (list 3))
                              (suffix one-element))))
       (test splicing-without-suffix
             (assert-same '(1) (splice three-elements)))
       (test suffix-4
             (assert-same '(3 4) (suffix four-elements)))
       (test appending-to-spliceable-list-updates-suffix
             (assert-same (obj 
                            contents
                            '(1 2 3 4)
                            last
                            list.4
                            suffix
                            '(2 3 4)
                            suffix-len
                            3)
                          (rep four-elements)))
       (test splicing-with-suffix-works
             (assert-same '(1 2) (splice four-elements))))

