(deftem foo field1 'default)
(= f inst!foo)

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
