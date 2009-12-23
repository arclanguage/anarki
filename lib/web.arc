; written by Mark Huetsch
; same license as Arc

($ (require openssl))
($ (xdef ssl-connect (lambda (host port)
		                   (ar-init-socket
		                     (lambda () (ssl-connect host port))))))

(load "lib/re.arc")
(load "lib/util.arc")

(def parse-server-cookies (s)
  (map [map trim _]
       (map [matchsplit "=" _] 
	    (tokens (cut s (len "Set-Cookie:")) #\;))))

(def parse-server-header (lines)
  (withs (http-response (tokens (car lines))
	  ret 
	   (list 
	     (list
	       (http-response 0)
	       (http-response 1)
	       (http-response 2))
	     (some (fn (s)
		     (and (begins s "Set-Cookie:")
			  (parse-server-cookies s)))
		   (cdr lines))))
    ret))

(def args->query-string (args)
  (if args
      (let equals-list (map [joinstr _ "="] (pair (map [coerce _ 'string] args)))
	(joinstr equals-list "&"))
      ""))

(def parse-url (url)
  (withs (url (joinstr (butlast (tokens url #\#)) "") ; throw away anchor component
	      has-trailing-slash (endmatch "/" url)
	      components (tokens url #\/)
	      first-component (pop components)
	      first-component-is-resource (endmatch ":" first-component)
	      resource (if first-component-is-resource (downcase (butlast first-component))
			   "http") ; defaults to http
	      host (if first-component-is-resource (pop components)
		       first-component)
	      host-tokens (tokens host #\:)
	      host (car host-tokens)
	      port (if (> (len host-tokens) 1) (int (last host-tokens)) (is "https" resource) 443 80)
	      components (tokens (if components 
				     (+ (joinstr components "/") (if has-trailing-slash "/"))
				     "")
				 #\?)
	      filename (if (and components (isnt "" (components 0))) (components 0))
	      query (if (> (len components) 1) (components 1)))
    (obj resource resource host host port port filename filename query query)))

(def encode-cookie (o)
  (let joined-list (map [joinstr _ #\=] (tablist o))
    (+ "Cookie: " 
       (if (len> joined-list 1)
	   (reduce [+ _1 "; " _2] joined-list)
	   (car joined-list))
       ";")))

; TODO this isn't very pretty
(def get-or-post-url (url (o args) (o method "GET") (o cookie))
  (withs (method (upcase method)
		 parsed-url (parse-url url)
		 args-query-string (args->query-string args)
		 full-args (joinstr (list args-query-string (parsed-url 'query)) "&")
		 request-path (+ "/" (parsed-url 'filename) 
				 (if (and (is method "GET") (> (len full-args) 0))
				     (+ "?" full-args)))
		 header-components (list (+ method " " request-path " HTTP/1.0") 
					 (+ "Host: " (parsed-url 'host))
					 "User-Agent: Mozilla/5.0 (Windows; U; Windows NT 5.1; uk; rv:1.9.1.2) Gecko/20090729 Firefox/3.5.2"))
    (when (is method "POST")
      (pushend (+ "Content-Length: " (len (utf-8-bytes full-args))) header-components)
      (pushend "Content-Type: application/x-www-form-urlencoded" header-components))
    (when cookie
      (push (encode-cookie cookie) header-components))
    (withs (header (reduce [+ _1 "\r\n" _2] header-components)
		   body (if (is method "POST") (+ full-args "\r\n"))
		   request-message (+ header "\r\n\r\n" body))
      (let (in out) (if (is "https" (parsed-url 'resource))
			(ssl-connect (parsed-url 'host) (parsed-url 'port))
			(socket-connect (parsed-url 'host) (parsed-url 'port)))
	(disp request-message out)
	(withs (result (tostring
			 (whilet line (readline in) (if line (prn line))))
		       body-start (posmatch "\r\n\r\n" result)
		       header-string (cut result 0 body-start)
		       header-lines (re-split "\r\n" header-string)
		       header (parse-server-header header-lines))
	  (close in out)
	  (list
	    header
	    (if body-start
		(cut result (+ 4 body-start))
		result)))))))

(def get-url (url)
  ((get-or-post-url url) 1))

(def post-url (url args)
  ((get-or-post-url url args "POST") 1))

(def google (q)
  (get-url (+ "www.google.com/search?q=" (urlencode q))))

; just some preliminary hacking
(mac w/browser body
  `(withs (cookies* (table)
		    get-url
		    (fn (url) (let (parsed-header html) (get-or-post-url url '() "GET" cookies*)
				(= cookies* (fill-table cookies* (flat (parsed-header 1))))
				html))
		    post-url
		    (fn (url args) (let (parsed-header html) (get-or-post-url url args "POST" cookies*)
				     (= cookies* (fill-table cookies* (flat (parsed-header 1))))
				     html)))
     (do ,@body)))
