;;; This software is copyright (c) Will Fitzgerald.
;;; Permission to use it is granted under
;;; the Perl Foundations's Artistic License 2.0.

;; An individual result is the description,
;; the body (expected) and a value (nil or non-nil)
(def def-result (desc body value) (list desc body value))
(def result-desc (result) (car result))
(def result-body (result) (cadr result))
(def result-value (result) (car (cddr result)))

;; A spec result is the description consed to a list of individual results
(def def-results (desc results) (list desc results))
(def results-desc (results) (car results))
(def results-results (results) (cadr results))

(def print-results (results (o all nil))
     (pr ";; " (results-desc results) "\n")
     (with (totals 0 goods 0 errors 0)
       (each result (results-results results)
         (++ totals)
           (when (result-value result)
             (if (is 'exception (type (result-value result)))
                 (++ errors)
                 (++ goods)))
           (when (or all (no (result-value result)))
             (pr ";; " (result-desc result) ": " (result-value result) "\t"
                 (car (result-body result)) "\n")))
       (pr ";; Tests: " totals "; Good: " goods "; Errors: " errors "; Pct: " (* 100.0 (/ goods totals)) "\n")
       (if (is totals goods)
           'green
           'red)))
            
(def assocs (key list)
     (keep [caris _ key] list))

(mac do2 args
  (w/uniq g
    `(do
       ,(car args)
         (let ,g ,(cadr args)
       ,@(cddr args)
       ,g))))

(mac describe body
  (with (desc (car body)
     prolog (cdr (assoc 'prolog (cdr body)))
     epilog (cdr (assoc 'epilog (cdr body)))
     setup  (cdr (assoc 'setup (cdr body)))
     teardown (cdr (assoc 'teardown (cdr body)))
     its (assocs 'it (cdr body)))
     `(fn ()
        (def-results ,desc
          (do2
           (do ,@prolog)
           (list
            ,@(map
               (fn (it)
                   `(do2
                     (do ,@setup)
                     (def-result
                       ,(cadr it)
                       ',(cddr it)
                       (on-err (fn (err) err) (fn () ,@(cddr it))))
                     (do ,@teardown)))
               its))
           (do ,@epilog))))))             

;; Example spec:
;;
;; (= test-basics
;;    (describe "Basic ARC list functions"
;;      (prolog
;;       (= li '(a b c))
;;       (= pair '(a . b)))
;;      (it "CAR should return the first item in a list"
;;        (is (car li) 'a))
;;      (it "CAR should return nil for an empty list"
;;        (is (car '()) nil))
;;      (it "CADR should return the second item of a list"
;;        (is (cadr li) 'b))
;;      (it "CADR should return NIL for an empty list"
;;        (is (cadr '()) nil))
;;      (it "CADR should return NIL for the 2nd item of a one item list"
;;        (is (cadr '(a)) nil))
;;      (it "CDR should return the rest of a list"
;;        (iso (cdr li) '(b c)))
;;      (it "CDR should returns '() for an empty list"
;;        (is (cdr '()) '()))
;;      (it "CDR should return the rest of a dotted pair"
;;        (is (cdr pair) 'b))
;;      (it "CDDR should return the rest of the rest of a list"
;;        (iso (cddr li) '(c)))
;;      (it "CDDR should return '() for an empty list"
;;        (is (cddr '()) '()))
;;      (it "CDDR should returns '() for a one item list"
;;        (is (cddr '(a)) '()))))
;;
;; (print-results (test-basics) t)
