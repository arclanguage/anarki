;;
;; Routines for manipulating sml
;;
;; sml is an S-Expression Meta Language for XML that looks like:
;;
;; (tagname attr "value" attr2 "value2"
;;   (tagname2)
;;   (tagname3 "data"))
;;

;; Convert S-Expression Meta Language to XML
(def sml-pr-xml (tag (o indent 0))
  (with (name (sml-tag-name tag)
	 elements (sml-elements tag))
    (repeat indent (pr " "))
    (pr "<" name)
    (sml-pr-attrs (sml-attrs tag))
    (if (is (len elements) 0)
	(prn "/>")
        (do
	  (prn ">")
	  (map [sml-pr-element _ (+ 2 indent)] elements)
	  (repeat indent (pr " "))
	  (prn "</" name ">")))
    nil))

(def sml-tag-name (tag)
  (car tag))

(def sml-attrs (tag)
  (let rest (cdr tag)
    (if (no rest) nil
        (caris (car rest) '@) (cdr:car rest) ;; old format
        (no (isa (car rest) 'sym)) nil
        (cons (car rest) (cons (cadr rest) (sml-attrs (cdr rest)))))))

(def sml-elements (tag)
  (let rest (cdr tag)
    (if (no rest) nil
        (caris (car rest) '@) (cdr rest) ;; old format
        (no (isa (car rest) 'sym)) rest
        (sml-elements (cdr rest)))))

(def sml-pr-attrs (attrs)
  (when attrs
    (pr " " (car attrs) "=\"")
    (each c (string (cadr attrs))
      (pr (case c #\\ "&#x5c;"
		  #\" "&#x22;"
		  #\& "&amp;"
		  c)))
    (pr "\"")
    (sml-pr-attrs (cddr attrs))))

(def sml-pr-element (el indent)
  (if (is (type el) 'cons) (sml-pr-xml el indent)
      (pr-escaped el)))

(def sml-get-attr (tag name)
  (sml-get-attr-from-attrs (sml-attrs tag) name))

(def sml-get-attr-from-attrs (attrs name)
  (when attrs
    (if (is (car attrs) name) (cadr attrs)
	    (sml-get-attr-from-attrs (cddr attrs) name))))

;; Pretty-print the S-Expression ML
(def sml-ppr (tag (o indent 0))
  (prn)
  (repeat indent (pr " "))
  (if (is (type tag) 'cons)
      (with (name (car tag)
             attrs (sml-attrs tag)
             elements (sml-elements tag))
        (pr "(" name)
	(each attr attrs (pr " ") (write attr))
        (each el elements (sml-ppr el (+ indent 2)))
        (pr ")"))
      (is (type tag) 'string) (write tag)
      (is tag nil) (pr nil)
      (err "Unrecognized type in sml:" (type tag)))
  (if (is indent 0) (prn)))
