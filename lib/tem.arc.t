(def normalize (tab)
  (sort (compare < tostring:write)
        (only.coerce tab 'cons)))

(deftem foo field1 'default)
(= f inst!foo)

(prn "WARNING: template tests are a mess")
(test-iso "templates pick up defaults"
  'default
  f!field1)

(= f!field1 34)
(test-iso "reading and assigning templates works"
  34
  f!field1)

(= f!field1 nil)
(test-iso "assigning templates to nil works"
  nil
  f!field1)

(test-iso "temlist works"
  '((field1 34))
  (normalize:temlist 'foo (inst 'foo 'field1 34)))

(test-iso "temlist skips default fields"
  '((new-field 3))
  (normalize:temlist 'foo (inst 'foo 'new-field 3)))

(test-iso "temlist keeps nil non-default fields"
  '((field1 nil))
  (normalize:temlist 'foo (inst 'foo 'field1 nil)))

(test-iso "temlist includes explicitly set default fields"
  '((field1 default))
  (normalize:temlist 'foo (inst 'foo 'field1 'default)))

(test-iso "temlist skips unknown nil fields"
  '((new-field2 3))
  (normalize:temlist 'foo (inst 'foo 'new-field1 nil 'new-field2 3)))

(test-iso "listtem DOES NOT ignore unknown fields"
  (inst 'foo 'new-field 34)
  (listtem 'foo '((new-field 34))))

(test-iso "temlist and listtem are converses"
  (inst 'foo 'field1 34)
  (listtem 'foo (temlist 'foo (inst 'foo 'field1 34))))

(test-iso "temread and temwrite are converses"
  (inst 'foo 'field1 34)
  (w/instring i (w/outstring o
                  (temwrite 'foo (inst 'foo 'field1 34) o)
                  (inside o))
    (temread 'foo i)))

(test-iso "temread and temwrite are converses - 2"
  (inst 'foo 'field1 nil)
  (w/instring i (w/outstring o
                  (temwrite 'foo (inst 'foo 'field1 nil) o)
                  (inside o))
    (temread 'foo i)))

(test-iso "nil in file overwrites default"
  nil
  (w/instring i (w/outstring o
                  (temwrite 'foo (inst 'foo 'field1 nil) o)
                  (inside o))
    ((temread 'foo i) 'field1)))

(test-iso "templates can distinguish explicit defaults from implicit defaults"
  (obj field1 'default)
  (let tem (inst 'foo)
    (= tem!field1 34)
    (= tem!field1 'default) ; explicit set
    rep.tem.1))

(deftem foo field1 (seconds))
(let f (inst 'foo)
  (test-is "template expressions work"
    f!field1
    (do sleep.2 f!field1)))
