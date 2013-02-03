;;; http://testanything.org
;;; Original: Shlomi Fish

(with (curr-test 0 num-planned 0)
      (def ok (value (o msg))
           (if value (pr "ok") (pr "not ok"))
           (pr " ")
           (pr (++ curr-test))
           (pr " ")
           (if msg (pr "- " msg))
           (prn)
           value)
      (def plan (num)
           (= num-planned num)
           (prn "1.." num)))

(mac def-test-fn(fn-name)
  `(def ,(sym:string "test-" fn-name) (msg expected got)
     (with (verdict (ok (,fn-name expected got) msg))
       (if verdict
         t
         (do
           (ero "expected: " expected)
           (ero "got: " got)
           nil)))))

(def-test-fn is)
(def-test-fn iso)
(def-test-fn contains)

(= is-deeply test-iso)

(= contains findsubseq)
