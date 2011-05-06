(system "rm -rf html")
(ensure-dir "html")
(load "template.arc")
(runall)
; currently not working
; (run "foundation-doc.tem")
(system "rm -rf output")
(ensure-dir "output")
(system "cp docs/* html/* output/")
