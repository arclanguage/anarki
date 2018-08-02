
; environment variable wrappers

(defmemo getenv (e) 
	($.getenv e))

(def putenv (e) 
	($.putenv e))

(defmemo env-has (e) 
	($.environment-variables? e))

(defmemo env-if (e d) 
	(if (env-has e) (getenv e) d))