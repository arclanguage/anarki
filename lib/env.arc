
; environment variable wrappers

(defmemo getenv (e) 
	($.getenv e))

(def putenv (e) 
	($.putenv e))

(defmemo env-has (e) 
	($.environment-variables? e))

(defmemo envf-if (e d) 
	(if (envhas e) (getenv e) d)