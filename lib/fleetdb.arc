; FleetDB Client Library for Arc written by Tim Robinson <tim@blackstag.com>
; license: Public domain (http://creativecommons.org/licenses/publicdomain/)

; Introduction to FleetDB (http://fleetdb.org/docs/introduction.html)
; 1. Download FleetDB: $ wget http://fleetdb.s3.amazonaws.com/fleetdb-standalone.jar
; 2. Start DB: $ java -cp fleetdb-standalone.jar fleetdb.server -f demo.fdb

; FleetDB Introduction Replicated via Arc client:
; (query client '(ping))       -> "pong"
; (query client '("bogus"))    <- should fail
; (query client (obj insert "accounts" with (obj id 1 owner "Eve" credits 100)))
; (query client (obj insert "accounts" 
;                    with (list (obj id 2 owner "Bob" credits 150)
;                               (obj id 3 owner "Dan" credits 50)
;                               (obj id 4 owner "Amy" credits 1000 vip 'true))))
; (query client (obj select "accounts" where '(= id 1)))
; (query client (obj select "accounts" where '(>= credits 150)))
; (query client (obj select "accounts" where '(in id (1 3))))
; (query client (obj select "accounts" order '(credits asc) limit 2))
; (query client (obj select "accounts"))
; (query client (obj count "accounts" where '(= vip true)))
; (query client (obj delete "accounts" where '(= id 3)))
; (query client (obj update "accounts" with (obj credits 55) where '(= owner Bob)))
; (query client (list "explain" (obj count "accounts" where '(= owner "Eve"))))
; (query client '(create-index accounts owner))
; (query client
;	  (list "multi-read" 
;      (obj select "accounts" (obj order '(credits desc) limit 1 only "owner"))
;      (obj select "accounts" (obj order '(credits asc) limit 1 only "owner"))))
; (query client
;	  (list "multi-write"
;      (obj update "accounts" with (obj credits 995) where '(= owner "Amy"))
;      (obj update "accounts" with (obj credits 105) where '(= owner "Eve"))))
; (query client
;	  (list "checked-write"
;      (obj select "accounts" where '(= owner "Amy") only "credits")
;     '(105)
;      (obj update "accounts" with (obj credits 100) where '(= owner "Eve"))))
; (query client '(list-indexes accounts))
; (query client '(drop-index accounts owner))
; (query client '(list-collections))
; (query client '(delete accounts))

($ (require (file "lib/json.ss")))
($ (xdef read-json read-json))
($ (xdef write-json write-json))

(= client (obj host "127.0.0.1" port 3400))

(def construct-query (q)
  (if (acons q) 
      (withs ((x . args) q v (string x)) 
        (if (in v "ping" "list-indexes" "create-index" "drop-index" "list-collections" "delete") q
            (case v 
	            "explain"     (list "explain" (construct-query (car args)))
	            "multi-read"  (list "multi-read"  (map construct-query args))
              "multi-write" (list "multi-write" (map construct-query args))
              "checked-write" (let (ifq val thenq) args 
                                   (list  "checked-write" (construct-query ifq) val (construct-query thenq))))))
      (let t (table)
         (aif q!where (= (t "where") it))
         (aif q!order (= (t "order") it))
         (aif q!only  (= (t "only") it))
         (aif q!limit (= (t "limit") it))
         (accum a 
	         (awhen q!select (a "select")(a it))
	         (awhen q!delete (a "delete")(a it))
	         (awhen q!insert (a "insert")(a it))
	         (awhen q!update (a "update")(a it))
	         (awhen q!count  (a "count") (a it))
		       (aif q!with  (a it))
           (a t)))))
       
(def query (x q)
  (let (i o) (socket-connect x!host x!port)
    (disp (tostring (write-json (construct-query q))(prn)) o)
    (let (status result) (read-json i)
    (close i o)
    (if (is status 0) result 
        (err "no response from server."))))))

  
  	
