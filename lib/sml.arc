;;
;; Routines for manipulating sml
;;
;; sml is an S-Expression Meta Language for XML that looks like:
;;
;; (tagname (@ attr "value" attr2 "value2")
;;   (tagname2)
;;   (tagname3 "data"))
;;
;; The attributes are optional (for brevity).
;; Since @ can't be a tag name, if the first thing in the
;; list (after the tag name) is a list whose car is @ then
;; it is the XML attributes for that tag.

;; Convert S-Expression Meta Language to XML
(def sml-pr-xml (tag (o indent 0))
  (let name (sml-tag-name tag)
    (repeat indent (pr " "))
    (pr "<" name)
    (sml-pr-attrs (sml-attrs tag))
    (prn ">")
    (map [sml-pr-element _ (+ 2 indent)] (sml-elements tag))
    (repeat indent (pr " "))
    (prn "</" name ">")
    nil))

(def sml-tag-name (tag)
  (car tag))

(def sml-attrs (tag)
  (let attr-list (cadr tag)
    (if (is (safecar attr-list) '@) (cdr attr-list))))

(def sml-elements (tag)
  (let attr-list (cadr tag)
    (if (is (safecar attr-list) '@) (cddr tag) (cdr tag))))

;; Return car if the argument is a cons, otherwise nil
(def safecar (xs)
  (if (is (type xs) 'cons) (car xs)))

(def sml-pr-attrs (attrs)
  (when attrs
    (pr " " (car attrs) "=\"")
    (each c (cadr attrs)
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

(def sml-attrs2db (attrs)
  (let attdb (newdb is)
    (sml-attrs-add attrs attdb)
	attdb))

(def sml-attrs-add (attrs attdb)
  (when attrs
    (= (attdb (car attrs)) (cadr attrs))
    (sml-attrs-add (cddr attrs) attdb)))

;; Pretty-print the S-Expression ML
(def sml-ppr (tag (o indent 0))
  (prn)
  (repeat indent (pr " "))
  (if (is (type tag) 'cons)
      (with (name (car tag)
             attrs (sml-attrs tag)
             elements (sml-elements tag))
        (pr "(" name)
        (if attrs (do (pr " ") (write (cons '@ attrs))))
        (each el elements (sml-ppr el (+ indent 2)))
        (pr ")"))
      (is (type tag) 'string) (write tag)
      (is tag nil) (pr nil)
      (err "Unrecognized type in sml:" (type tag)))
  (if (is indent 0) (prn)))
