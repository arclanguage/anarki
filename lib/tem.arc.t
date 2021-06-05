(def normalize (tab)
  (sort (compare < tostring:write)
        (only.coerce tab 'cons)))

(deftem foo field1 'default)

(deftem default-expression field1 (seconds))

(deftem empty-tem)

(suite template
       (setup tem-with-default         (inst 'foo)
              instance-with-expression (inst 'default-expression (seconds))
              empty-instance           (inst 'empty-tem))
       (test default-works
             (assert-same 'default tem-with-default!field1))
       (test assigning-templates
             (do (= tem-with-default!field1 'changed)
                 (assert-same 'changed tem-with-default!field1)))
       (test assigning-returns-value
             (assert-same (= tem-with-default!field2 'changed)
                          'changed))
       (test empty-template
             (assert-nil empty-instance!field1))
       (test assigning-nil
             (do (wipe tem-with-default!field1)
                 (assert-nil tem-with-default!field1)))
       (test copy-templates
             (assert-same tem-with-default
                          (copy tem-with-default)))
       (test temlist
             (assert-same '((field1 34))
                          (normalize:temlist 'foo
                                             (inst 'foo 'field1 34))))
       (test temlist-includes-default-fields
             (assert-same '((field1 default) (new-field 3))
                          (normalize:temlist 'foo
                                             (inst 'foo 'new-field 3))))
       (test temlist-keeps-nil-non-default-fields
             (assert-same '((field1 nil))
                          (normalize:temlist 'foo
                                             (inst 'foo 'field1 nil))))
       (test temlist-includes-explicitly-set-default-fields
             (assert-same '((field1 default))
                          (normalize:temlist 'foo
                                             (inst 'foo 'field1 'default))))
       (test temlist-includes-unknown-nil-fields
             (assert-same '((field1 default)
                            (new-field1 nil)
                            (new-field2 3))
                          (normalize:temlist 'foo
                                             (inst 'foo 'new-field1 nil 'new-field2 3))))
       (test templist-doesnt-include-unknown-fields
             (assert-same (inst 'foo 'new-field 34)
                          (listtem 'foo '((new-field 34)))))
       (test listtem-handles-nil
             (assert-same (inst 'foo) (listtem 'foo nil)))
       (test temlist-is-converse-of-listtem
             (assert-same (inst 'foo 'field1 34)
                          (listtem 'foo
                                   (temlist 'foo (inst 'foo 'field1 34)))))
       (test temread-is-converse-of-temwrite
             (assert-same (inst 'foo 'field1 34)
                          (w/instring i (w/outstring o (temwrite 'foo (inst 'foo 'field1 34) o) (inside o))
                            (temread 'foo i))))
       (test temread-is-converse-of-temwrite-complicated
             (assert-same (inst 'foo 'field1 nil)
                          (w/instring i (w/outstring o (temwrite 'foo (inst 'foo 'field1 nil) o) (inside o))
                            (temread 'foo i))))
       (test nil-in-file-overwrites-default
             (assert-nil (w/instring i (w/outstring o (temwrite 'foo (inst 'foo 'field1 nil) o) (inside o))
                           ((temread 'foo i) 'field1))))
       (test template-expressions-are-evaluated-at-inst
             (assert-same instance-with-expression!field1
                          (do (sleep 1)
                              instance-with-expression!field1)))
       (test empty-tem-has-len-zero
             (assert-same 0
                          (len empty-instance)))
       (test tem-with-default-has-len-one
             (assert-same 1
                          (len tem-with-default))))
