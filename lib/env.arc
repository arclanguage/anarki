
; environment variable wrappers

(defmemo getenv (e) 
	($.getenv e))

(def putenv (e) 
	($.putenv e))

(defmemo envhas (e) 
	($.environment-variables? e))

(defmemo envif (e d) 
	(if (envhas e) (getenv e) d)