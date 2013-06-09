(declare 'atstrings nil)

(system "rm -rf html")
(ensure-dir "html")
(load "template.arc")
(runall)

;; fails with "Expected tests" error
;; (run "foundation-doc.tem")
(system "cp foundation-doc.html html/")

(system "rm -rf output")
(ensure-dir "output")
(system "cp docs/* html/* output/")
