(def test (msg val)
  (if val
    (ero "ok - " msg)
    (ero "not ok - " msg)))

(mac def-test-fn(fn-name)
  `(def ,(sym:string "test-" fn-name) (msg expected got)
     (iflet verdict (,fn-name expected got)
       (ero "ok - " msg)
       (do
         (ero "not ok - " msg)
         (ero "expected: " expected)
         (ero "got: " got)
         nil))))

(def-test-fn is)
(def-test-fn iso)
(def-test-fn contains)

(= is-deeply test-iso)

(= contains findsubseq)

(= not no)
