; written by Mark Huetsch

(load "lib/util.arc")

(def args>query-string (args)
  (if args
      (let equals-list (map [joinstr _ "="] (pair (map [coerce _ 'string] args)))
	(joinstr equals-list "&"))
      ""))

(def parse-url (url)
  (withs (components (joinstr (tokens url #\#) "") ; throw away anchor component
		     has-trailing-slash (endmatch "/" url)
		     components (tokens url #\/)
		     first-component (pop components)
		     first-component-is-resource (endmatch ":" first-component)
		     resource (if first-component-is-resource (butlast first-component)
				  "http") ; defaults to http
		     host (if first-component-is-resource (pop components)
			      first-component)
		     host-tokens (tokens host #\:)
		     host (car host-tokens)
		     port (if (> (len host-tokens) 1) (int (last host-tokens)) 80)
		     components (tokens (if components 
					    (+ (joinstr components "/") (if has-trailing-slash "/"))
					    "")
					#\?)
		     filename (if (and components (isnt "" (components 0))) (components 0))
		     query (if (> (len components) 1) (components 1)))
    (obj resource resource host host port port filename filename query query)))

; TODO this isn't very pretty
(def get-or-post-url (url (o args) (o method "GET"))
  (withs (method (upcase method)
		 parsed-url (parse-url url)
		 args-query-string (args>query-string args)
		 full-args (joinstr (list args-query-string (parsed-url 'query)) "&")
		 request-path (+ "/" (parsed-url 'filename) 
				 (if (and (is method "GET") (> (len full-args) 0))
				     (+ "?" full-args)))
		 header-components (list (+ method " " request-path " HTTP/1.0") 
					 (+ "Host: " (parsed-url 'host))
					 "User-Agent: Mozilla/5.0 (Windows; U; Windows NT 5.1; uk; rv:1.9.1.2) Gecko/20090729 Firefox/3.5.2"
					 (if (is method "POST") (+ "Content-Length: " (len full-args)) 
					     "")
					 (if (is method "POST") "Content-Type: application/x-www-form-urlencoded"
					     ""))
		 header (reduce [+ _1 "\r\n" _2] header-components)
		 body (if (is method "POST") (+ full-args "\r\n"))
		 request-message (+ header "\r\n\r\n" body))
    (let (in out) (socket-connect (parsed-url 'host) (parsed-url 'port))
      (w/stdout out
	(disp request-message)
	(withs (result (tostring
			 (whilet line (readline in) (if line (prn line))))
		       body-start (posmatch "\r\n\r\n" result))
	  (close in out)
	  (if body-start
	      (cut result (+ 4 body-start))
	      result))))))

(def get-url (url)
  (get-or-post-url url))

(def post-url (url args)
  (get-or-post-url url args "POST"))
